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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty â Sovereignty-Primary Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty-primary reading of the
 *   federation_membership_treaty kernel. Under this reading, free movement of
 *   workers across a federal union is not an unconditional market right but a
 *   privilege contingent on member state consent. National governments retain
 *   the authority to protect local labor markets and welfare systems through
 *   restrictions, labor-market tests, and non-portability of benefits. The
 *   structural delta is that local labor markets enter the beneficiary set,
 *   mobile workers enter the victim set via restricted access, and national
 *   regulatory autonomy is strongly preserved. The constraint has a genuine
 *   coordination functionâpreserving member state consent prevents
 *   federation breakupâbut it asymmetrically extracts mobility rights from
 *   individual workers. The claim is tangled_rope; the metrics are authored
 *   independently to describe a moderately extractive, actively enforced
 *   arrangement with rising theater as sovereignty claims are increasingly
 *   performative.
 *
 * KEY AGENTS:
 *   - national_governments (agenda_setter/institutional/arbitrage) â administer the treaty reservations and enforce labor market restrictions
 *   - local_labor_markets (beneficiary/organized/constrained) â receive protection from mobile worker competition
 *   - mobile_workers (payer/powerless/constrained) â bear restricted access and welfare exclusion
 *   - pro_integration_institutions (observer/institutional/analytical) â monitor compliance and narrow reservations
 *   - mobile_labor_employers (excluded/powerful/constrained) â would hire mobile workers but are sidelined in policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.6).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty â Sovereignty-Primary Reading").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '56a20b09-c004-4e9f-9efb-7c83ed83312b').
narrative_ontology:cs_kernel_codification('56a20b09-c004-4e9f-9efb-7c83ed83312b', formalized).
narrative_ontology:cs_authority_grounding('56a20b09-c004-4e9f-9efb-7c83ed83312b', lineage).
narrative_ontology:cs_interpretation_layer_present('56a20b09-c004-4e9f-9efb-7c83ed83312b').
narrative_ontology:cs_reading_relation('56a20b09-c004-4e9f-9efb-7c83ed83312b', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('56a20b09-c004-4e9f-9efb-7c83ed83312b', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('56a20b09-c004-4e9f-9efb-7c83ed83312b', foundational, market_access_conditional_on_state_consent).
narrative_ontology:cs_axiom_status(market_access_conditional_on_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('56a20b09-c004-4e9f-9efb-7c83ed83312b', market_access_conditional_on_state_consent, conventional).
narrative_ontology:cs_axiom('56a20b09-c004-4e9f-9efb-7c83ed83312b', foundational, welfare_solidarity_nationally_bounded).
narrative_ontology:cs_axiom_status(welfare_solidarity_nationally_bounded, holdable).
narrative_ontology:cs_axiom_grounding('56a20b09-c004-4e9f-9efb-7c83ed83312b', welfare_solidarity_nationally_bounded, conventional).
narrative_ontology:cs_reference_frame('56a20b09-c004-4e9f-9efb-7c83ed83312b', national_sovereignty_reserved).
narrative_ontology:cs_drift_state('56a20b09-c004-4e9f-9efb-7c83ed83312b', contemporary_integration_stress, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56a20b09-c004-4e9f-9efb-7c83ed83312b', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, state_consent_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, national_welfare_state_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They negotiate and enforce treaty reservations that condition free movement on national consent, invoking safeguard clauses, labor-market tests, and welfare restrictions to protect domestic labor markets and maintain sovereignty over migration flows.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Native workers and established labor-market participants whose wages and employment conditions are shielded from direct competition with mobile workers by national restrictions on market access and welfare portability.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, local_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Citizens of other federation member states seeking work or residence who face legal barriers to entry, restricted welfare eligibility, and precarious residency status contingent on labor-market tests and state discretion.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, national).

% Federation-level courts and commissions that monitor treaty compliance and promote market integration, often issuing rulings that narrow the scope of national reservations.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, pro_integration_institutions, observer,
    institutional, generational, analytical, continental).

% Employers who would hire mobile workers to fill vacancies or reduce costs, but are structurally excluded from the policy conversation when states invoke sovereignty to restrict access and protect native insiders.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_labor_employers, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, local_labor_markets).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continued political adhesion of economically divergent member states to a federal union by preserving their autonomy over labor-market regulation and welfare eligibility, thereby reducing exit pressures that would destabilize the federation.
% TRANSFER_FUNCTION: Transfers labor-market opportunity, residency security, and welfare access from mobile workers to native labor-market participants, and transfers regulatory control over migration flows from the federal level back to national governments.
% ABSENT_VOICES: Mobile workers lack voting rights in host-state elections; pro-integration employers and mobile-worker advocacy coalitions are structurally underrepresented when states invoke sovereignty to restrict access.
% DISAPPEARANCE_RATIONALE: If the conditionality vanished overnight, national governments would lose the legal basis for labor-market restrictions and welfare exclusions; mobile-worker flows would redistribute across the federation, wage equilibria in protected markets would shift, and the political compact holding the federation together would face immediate destabilizing exit pressures from sovereignty-sensitive member states.
% FOUNDING_PROBLEM: How to construct a durable federal market across member states with divergent wealth levels, labor-market institutions, and welfare-state designs without forcing homogenization that would trigger political withdrawal.
% FOUNDING_PROBLEM_CORROBORATION: Comparative federalism scholars and economic historians outside the state beneficiary set attest that early federation architects explicitly confronted this adhesion problem; pro-integration legal scholars and mobile-worker advocates outside the benefiting parties contest that the current restrictions exceed what durability requires and serve protectionist capture instead.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because mobile workers lose significant labor market opportunity and welfare access relative to a free-movement counterfactual. Suppression (0.60) reflects active enforcement of residency permits, labor-market tests, and welfare exclusions. Theater_ratio (0.45) captures the growing gap between sovereignty rhetoric and actual economic dependence on mobile labor in many member states. Accessibility_collapse (0.40) is moderate: full free movement and full national closure are both visible alternatives, but the political viability of either is blocked by the existing compromise. Resistance (0.55) comes from mobile workers, integration institutions, and pro-mobility business lobbies.
 *
 * PERSPECTIVAL GAP:
 *   The national government seat should compute as coordination (they see themselves preserving federation stability and democratic consent), while the mobile worker seat should compute as extraction (they experience conditionality as a barrier to economic opportunity). Local labor markets sit near the beneficiary end. The divergence is structurally grounded in the same treaty text interpreted through sovereignty-preserving lenses.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and local labor markets are beneficiaries: the constraint subsidizes their regulatory control and wage position (low d, damped Ï). Mobile workers are victims: the constraint extracts mobility rights and welfare access (high d, amplified Ï). Pro-integration institutions are observers with analytical exit; employers are excluded but structurally constrained by the political closure of the debate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents pure-extraction mislabeling because its coordination function is genuine: without some sovereignty preservation, member states with divergent labor market institutions would face exit pressures that destabilize the federation. However, the arrangement is not merely coordination because the extraction is asymmetricâmobile workers pay for stability they do not design. Mandatrophy is contested: integrationists argue the founding adhesion problem is largely solved and the restrictions now serve protectionism; sovereignty advocates argue the problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_absolute_or_proportionate,
    'Does the sovereignty-primary reading permit any member state restriction whatsoever, or is authority bounded by proportionality and non-discrimination principles?',
    'Systematic review of treaty safeguard clauses and federation court jurisprudence to determine whether national measures are subject to substantive review or procedural notification only.',
    'If authority is absolute, the constraint functions as a near-snare for mobile workers; if proportionality-bound, the coordination function (federation durability) is more salient and the reading remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_absolute_or_proportionate, conceptual, 'Whether state consent is absolute or proportionality-limited.').

omega_variable(
    labor_protection_or_rent_capture,
    'Do national labor market restrictions genuinely protect vulnerable native workers from wage-undercutting, or do they primarily capture rents for organized labor market insiders at the expense of mobile workers and consumers?',
    'Econometric analysis of wage and employment effects in regulated versus unregulated sectors across member states, controlling for productivity differentials.',
    'If primarily rent capture, extractiveness is higher and the coordination story weakens; if genuine protection, the beneficiary structure is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_protection_or_rent_capture, empirical, 'Whether labor market protection is genuine or rent-seeking.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the constrained exit of mobile workers due solely to legal barriers, or has second-class status become internalized through repeated deferral to state discretion?',
    'Comparative mobility behavior before and after regularization shocks or court rulings expanding rights; persistent under-claiming of rights indicates internalization.',
    'If internalized, effective suppression exceeds the structural measure because mobile workers carry the constraint with them even when legal barriers are temporarily removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized suppression for mobile workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t6, federation_membership_treaty__sovereignty_primary, theater_ratio, 6, 0.26).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__sovereignty_primary, theater_ratio, 12, 0.32).
narrative_ontology:measurement(fede_tr_t18, federation_membership_treaty__sovereignty_primary, theater_ratio, 18, 0.38).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__sovereignty_primary, theater_ratio, 24, 0.42).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t6, federation_membership_treaty__sovereignty_primary, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__sovereignty_primary, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(fede_be_t18, federation_membership_treaty__sovereignty_primary, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__sovereignty_primary, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t6, federation_membership_treaty__sovereignty_primary, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__sovereignty_primary, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(fede_su_t18, federation_membership_treaty__sovereignty_primary, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__sovereignty_primary, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, subsidiarity_balance).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel decomposes into three structurally distinct readings. This story authors the sovereignty-primary reading; siblings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
