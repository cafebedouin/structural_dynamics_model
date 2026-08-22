% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Sovereignty-Primary Reading of Free Movement: Member-State Consent Gate
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   A federation of sovereign states establishes free movement of workers as
 *   a treaty commitment, but the sovereignty_primary reading holds that this
 *   commitment is conditional on continuing state consent: member states
 *   retain enforceable authority to protect national labor markets via
 *   quotas, transitional restrictions, and labor-market tests, and to protect
 *   welfare systems via residency-based benefit gating. The coordination
 *   function (federation membership sustainable for labor-sensitive states)
 *   is genuine, but it operates through a structure that systematically
 *   transfers bargaining power from mobile workers to incumbent domestic
 *   labor and welfare systems, and that transfer requires active
 *   administrative enforcement (permit regimes, border checks, benefit
 *   residency tests) to persist.
 *
 * KEY AGENTS:
 *   - national_governments: agenda_setter (institutional/arbitrage) — sets and enforces consent-based restrictions
 *   - national_labor_markets: beneficiary (organized/constrained) — protected from wage competition
 *   - domestic_welfare_systems: beneficiary (institutional/analytical) — shielded from open-pool claims
 *   - mobile_workers: payer (powerless/trapped) — bears the restriction directly
 *   - federation_treaty_bodies: observer (institutional/analytical) — reviews restrictions within a sovereignty-primary default
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.61).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Sovereignty-Primary Reading of Free Movement: Member-State Consent Gate").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, 'ea8940d7-69ed-436e-9bf9-89f6750f87f8').
narrative_ontology:cs_kernel_codification('ea8940d7-69ed-436e-9bf9-89f6750f87f8', fixed_text).
narrative_ontology:cs_authority_grounding('ea8940d7-69ed-436e-9bf9-89f6750f87f8', lineage).
narrative_ontology:cs_interpretation_layer_present('ea8940d7-69ed-436e-9bf9-89f6750f87f8').
narrative_ontology:cs_reading_relation('ea8940d7-69ed-436e-9bf9-89f6750f87f8', federation_membership_treaty__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('ea8940d7-69ed-436e-9bf9-89f6750f87f8', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('ea8940d7-69ed-436e-9bf9-89f6750f87f8', foundational, member_state_consent_is_constitutive_of_mobility_scope).
narrative_ontology:cs_axiom_status(member_state_consent_is_constitutive_of_mobility_scope, holdable).
narrative_ontology:cs_axiom_grounding('ea8940d7-69ed-436e-9bf9-89f6750f87f8', member_state_consent_is_constitutive_of_mobility_scope, conventional).
narrative_ontology:cs_axiom('ea8940d7-69ed-436e-9bf9-89f6750f87f8', secondary, national_welfare_system_solvency_justifies_residency_gating).
narrative_ontology:cs_axiom_status(national_welfare_system_solvency_justifies_residency_gating, holdable).
narrative_ontology:cs_axiom_grounding('ea8940d7-69ed-436e-9bf9-89f6750f87f8', national_welfare_system_solvency_justifies_residency_gating, instrumental).
narrative_ontology:cs_reference_frame('ea8940d7-69ed-436e-9bf9-89f6750f87f8', intergovernmental_consent_baseline).
narrative_ontology:cs_drift_state('ea8940d7-69ed-436e-9bf9-89f6750f87f8', post_enlargement_mobility_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ea8940d7-69ed-436e-9bf9-89f6750f87f8', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, incumbent_domestic_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_job_seekers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, posted_worker_dependents).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, state_consent_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, residual_national_competence_over_welfare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain and actively exercise treaty-reserved authority to condition free movement on quotas, transitional arrangements, work permits, and welfare residency tests. Administers border checks, benefit eligibility rules, and labor market tests that gate mobile workers' access. Can invoke safeguard clauses or renegotiate protocols when domestic pressure rises; effectively sets the terms under which the sibling readings' mobility rights operate in practice.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Domestic labor market institutions (unions, sectoral bodies, incumbent employers organized around national wage floors) receive protection from wage-suppressing inflows via quotas and transitional restrictions. Their leverage depends on the state's continued willingness to invoke consent-based limits; a shift toward integration_primary would erode this protection.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_markets, beneficiary,
    organized, generational, constrained, national).

% National welfare and social insurance funds are shielded from being treated as an open common pool; residency and contribution tests limit near-immediate claims by newly arrived mobile workers, preserving fund solvency assumptions built on a bounded contributor base.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_welfare_systems, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__sovereignty_primary, domestic_welfare_systems).

% Workers already inside the domestic labor market benefit from reduced competition at the wage floor and from welfare systems calibrated to a smaller, more predictable claimant pool. Their gain is diffuse but real: slower wage compression in sectors exposed to cross-border labor supply.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, incumbent_domestic_workers, beneficiary,
    moderate, biographical, constrained, national).

% Citizens of other member states seeking work face permit requirements, quota ceilings, sector-specific restrictions, and delayed or denied welfare access despite formal treaty membership. Their nominal free-movement right is realized only insofar as the destination state consents case-by-case or category-by-category; exit to a more open jurisdiction is possible in theory but often means abandoning accumulated employment, housing, and family ties.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, biographical, trapped, continental).

% Individuals attempting to enter a national labor market for the first time bear the direct cost of labor-market tests and quota administration; job offers can be withdrawn or delayed pending permit clearance, and the uncertainty itself suppresses their bargaining position relative to domestic applicants.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_job_seekers, payer,
    powerless, biographical, constrained, continental).

% Family members of workers posted or admitted under restrictive terms often face separate, more restrictive residency and welfare-access rules than the primary worker, compounding the consent-gate's effects on household stability and dependents' access to schooling and healthcare.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, posted_worker_dependents, payer,
    powerless, biographical, trapped, continental).

% Employers who would hire mobile workers without restriction are not formally consulted when quotas or labor-market tests are set; they absorb administrative burden and lose access to labor supply but have no seat in the state-consent negotiation, which is conducted between governments.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, receiving_country_employers, excluded,
    organized, biographical, constrained, national).

% Federation-level courts and commissions monitor whether member-state restrictions stay within treaty-permitted bounds, adjudicate disputes, and can rule restrictions unlawful — but under this reading they are understood as reviewing the exercise of a retained sovereign power, not as the primary source of the mobility right itself.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_treaty_bodies, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, national_governments).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to join a federation-wide mobility framework while retaining a genuine veto-like capacity over the pace and scope of labor-market and welfare exposure, making federation membership politically sustainable for states with sensitive domestic labor markets.
% TRANSFER_FUNCTION: Moves bargaining power and labor-market protection from mobile/prospective workers to incumbent domestic workers and national welfare systems, via permit regimes, quotas, and residency-based benefit gating administered by the state.
% ABSENT_VOICES: Mobile workers and cross-border job seekers have no direct vote in the national political processes that set quota levels or welfare residency rules; receiving-country employers who would prefer open access are also outside the intergovernmental negotiation that fixes the terms.
% DISAPPEARANCE_RATIONALE: If member-state consent authority over free movement vanished overnight, labor-market tests, quotas, and welfare residency gates would lose their legal basis; mobile workers could enter and claim benefits on parity with nationals immediately, domestic wage floors in exposed sectors would face faster competitive pressure, and national welfare systems would need to recompute solvency assumptions against an open contributor pool — a substantial rearrangement of both migration flows and domestic political coalitions built around protection.
% FOUNDING_PROBLEM: Federation-building required reconciling a shared market with member states whose labor markets and welfare systems were built for closed national populations; without a consent mechanism, states with weaker labor markets or younger welfare systems risked destabilizing wage or benefit shocks and would not have joined or would defect.
% FOUNDING_PROBLEM_CORROBORATION: National governments and organized labor-market bodies attest the founding problem remains live, citing wage-compression and welfare-solvency studies in specific sectors and regions. Independent labor economists and federation-level commission reports (produced by federation_treaty_bodies, an observer seat outside the beneficiary group) attest that in many corridors the empirical wage and fiscal effects of open mobility have been small or positive, suggesting the consent gate now functions substantially as political insurance against perceived rather than measured risk.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58 at interval end) reflects a genuine but asymmetric transfer: national labor markets and welfare systems gain real protection, mobile workers absorb real costs in wages, access, and family stability, and the transfer is mediated through administrative machinery rather than market outcomes. Suppression (0.61) is substantial because the restriction regime depends on active permit administration and benefit-eligibility gatekeeping, not mere participant preference — removing enforcement would immediately change outcomes. Theater ratio (0.32) is moderate-low: labor-market tests do real gatekeeping work, but a growing share of the apparatus (safeguard-clause invocations, symbolic quota announcements) increasingly serves domestic political signaling rather than measurable labor-market protection, which is why theater_ratio rises across the series even as underlying extraction rises modestly.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments sit at the agenda-setter/beneficiary pole: institutional power, arbitrage-grade exit (they can renegotiate or invoke safeguard clauses), low derived d. Mobile workers and cross-border job seekers sit at the target pole: powerless, trapped or constrained exit, high derived d — the consent gate is exactly what stands between them and equal-footing labor market access. Domestic welfare systems and national labor markets are declared non-agent/organized beneficiaries respectively, feeding low d through the beneficiary channel without themselves acting. Receiving-country employers are excluded rather than coordinated: they bear administrative cost and lost labor supply but have no seat in the intergovernmental bargain, which is why their exit_options are constrained despite organized power — the restriction is imposed on them by a negotiation they are not party to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling shared-market membership with states whose labor and welfare systems were built for closed populations) was live at founding and remains contested rather than cleanly dead: some sectors show real ongoing sensitivity, others show the mechanism persisting past its empirical justification. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (federation membership stays viable for labor-sensitive states) while still naming the asymmetric extraction from mobile workers — collapsing it to pure extraction would erase the real accession-sustaining function; collapsing it to pure coordination (rope) would erase the identifiable victims and the active enforcement requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_primary_vs_integration_primary_default,
    'Should free movement be read as a constitutive default that restrictions must narrowly justify (integration_primary), or as a conditional privilege that member-state consent defines the scope of (sovereignty_primary)? This story adopts the latter.',
    'Treaty text and founding-era ratification debates could be examined for which reading the original consenting states understood themselves to be adopting; subsequent treaty-body jurisprudence trend lines (toward broader or narrower permissible restriction) would indicate which reading is gaining structural ground.',
    'Under integration_primary, national labor-market protections would be classified as the extractive element (a snare or tangled_rope from the mobile-worker seat with restrictions as the extraction) rather than as the beneficiary-protecting coordination function this reading treats them as. The beneficiary and victim sets would partially invert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_primary_vs_integration_primary_default, conceptual, 'Which default (sovereignty vs integration) the kernel''s founding instrument actually encodes.').

omega_variable(
    consent_gate_naturalness_vs_construction,
    'Is member-state authority to restrict free movement a genuine irreducible feature of any voluntary federation of sovereign states (a structural mountain-like limit on integration), or a constructed and contestable political choice that could be traded away in future treaty revision?',
    'Comparative federalism analysis: do federations with stronger constitutive mobility rights (e.g., domestic interstate commerce clauses) survive and function without state-level labor-market consent gates? If yes, the sovereignty_primary gate is a contingent choice, not a structural necessity.',
    'If genuinely structural, the tangled_rope classification understates how much of the extraction is actually unavoidable coordination cost of federation under sovereign consent; if contingent, the extraction is more purely a policy choice defensible on protection grounds but not on necessity grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gate_naturalness_vs_construction, conceptual, 'Whether the sovereignty-primary consent gate is structurally necessary to federation or a contestable policy choice.').

omega_variable(
    quota_administration_theater_share,
    'What share of current quota and safeguard-clause activity is genuine labor-market protection versus domestic political signaling with no measurable protective effect?',
    'Sector-by-sector wage and employment impact studies comparing restricted versus liberalized labor corridors within the same federation, controlling for macroeconomic conditions.',
    'A high theater share would support reclassification toward piton in specific sectors (protection function atrophied, machinery maintained for domestic political performance) even while the overall constraint remains tangled_rope elsewhere.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quota_administration_theater_share, empirical, 'Whether quota administration function has partially atrophied into political theater in specific sectors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__sovereignty_primary, theater_ratio, 8, 0.24).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__sovereignty_primary, theater_ratio, 16, 0.27).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__sovereignty_primary, theater_ratio, 24, 0.29).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__sovereignty_primary, theater_ratio, 32, 0.31).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__sovereignty_primary, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__sovereignty_primary, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__sovereignty_primary, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__sovereignty_primary, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__sovereignty_primary, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__sovereignty_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__sovereignty_primary, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__sovereignty_primary, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__sovereignty_primary, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__sovereignty_primary, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__sovereignty_primary, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the federation_membership_treaty kernel, each authored as a separate ε-invariant story per the ε-invariance principle: sovereignty_primary (this story, tangled_rope — national labor markets/welfare beneficiaries, mobile workers as victims, substantial extraction and suppression); integration_primary (constitutive mobility right, restrictions presumptively illegitimate — inverted beneficiary/victim structure); subsidiarity_balance (proportionality-bounded co-equal constraint, expected lower extraction and lower suppression than either pole reading). All three should link to each other via affects_constraints; none averages or hedges across the others' ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
