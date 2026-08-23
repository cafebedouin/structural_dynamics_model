% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: EU Free Movement Proportionality Balance (Subsidiarity Reading)
 *   domain: political/economic/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the subsidiarity_balance reading of the
 *   federation_membership_treaty kernel. It posits that free movement rights
 *   within a federal or quasi-federal union (the EU) are structurally bounded
 *   by proportionality: host states may restrict mobility to protect
 *   legitimate national interests (public health, welfare system integrity,
 *   labor market stability), but may not eliminate mobility entirely. The
 *   constraint is actively enforced by EU institutions (Commission, ECJ) and
 *   requires member states to justify restrictions through proportionality
 *   review. It generates genuine coordination benefits for mobile labor and
 *   receiving employers, while extracting regulatory autonomy from host
 *   states and exposing static workers to competitive pressure. It is
 *   structurally distinct from the integration_primary reading (which treats
 *   restrictions as presumptively illegitimate) and the sovereignty_primary
 *   reading (which treats state consent as the ultimate constraint). As a
 *   kernel reading, it is authored as a clean Îµ-invariant constraint; the
 *   committer structure is routed to omega variables and cs_structure.
 *
 * KEY AGENTS:
 *   - eu_commission (institutional/analytical): Primary agenda-setter and enforcer of treaty compliance through infringement procedures.
 *   - ecj (institutional/analytical): Judicial observer and agenda-setter via proportionality jurisprudence.
 *   - host_state_governments (institutional/constrained): Primary target â lose blanket regulatory autonomy over mobility, must justify restrictions.
 *   - static_labor_pool (powerless/trapped): Secondary target â bears labor market competition costs without mobility options.
 *   - cross_border_labor (moderate/mobile): Primary beneficiary â gains conditional mobility rights.
 *   - receiving_sector_employers (powerful/mobile): Secondary beneficiary â gains access to expanded labor supply.
 *   - mobility_applicants_denied (powerless/trapped): Target in specific instances where proportionality tests block entry.
 *   - excluded_sovereigntist_factions (organized/constrained): Excluded voice seeking blanket restrictions.
 *   - excluded_integration_advocates (organized/constrained): Excluded voice seeking unconditional mobility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.58).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.52).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "EU Free Movement Proportionality Balance (Subsidiarity Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political/economic/federalism").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '90cf0a96-438b-4ac1-91fe-a75316d42031').
narrative_ontology:cs_kernel_codification('90cf0a96-438b-4ac1-91fe-a75316d42031', formalized).
narrative_ontology:cs_authority_grounding('90cf0a96-438b-4ac1-91fe-a75316d42031', lineage).
narrative_ontology:cs_interpretation_layer_present('90cf0a96-438b-4ac1-91fe-a75316d42031').
narrative_ontology:cs_reading_relation('90cf0a96-438b-4ac1-91fe-a75316d42031', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('90cf0a96-438b-4ac1-91fe-a75316d42031', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('90cf0a96-438b-4ac1-91fe-a75316d42031', foundational, proportionality_as_mobility_limit).
narrative_ontology:cs_axiom_status(proportionality_as_mobility_limit, holdable).
narrative_ontology:cs_axiom_grounding('90cf0a96-438b-4ac1-91fe-a75316d42031', proportionality_as_mobility_limit, conventional).
narrative_ontology:cs_axiom('90cf0a96-438b-4ac1-91fe-a75316d42031', foundational, non_eliminability_of_movement_rights).
narrative_ontology:cs_axiom_status(non_eliminability_of_movement_rights, holdable).
narrative_ontology:cs_axiom_grounding('90cf0a96-438b-4ac1-91fe-a75316d42031', non_eliminability_of_movement_rights, conventional).
narrative_ontology:cs_reference_frame('90cf0a96-438b-4ac1-91fe-a75316d42031', treaty_based_proportionality_balance).
narrative_ontology:cs_drift_state('90cf0a96-438b-4ac1-91fe-a75316d42031', post_enlargement_political_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('90cf0a96-438b-4ac1-91fe-a75316d42031', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, cross_border_labor).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, receiving_sector_employers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, host_state_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, static_labor_pool).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, mobility_applicants_denied).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers who exercise mobility rights to seek employment across member state borders. They gain conditional access to host labor markets and social advantages, subject to proportionality limitations imposed by host states.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, cross_border_labor, beneficiary,
    moderate, biographical, mobile, continental).

% Employers in sectors that rely on mobile labor supply. They benefit from expanded hiring pools and cross-border service provision enabled by the free movement framework.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, receiving_sector_employers, beneficiary,
    powerful, biographical, mobile, national).

% Member state administrations that must justify restrictions on mobility through proportionality review. They retain limited regulatory flexibility but lose the power to impose blanket immigration or welfare restrictions without judicial scrutiny.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_governments, payer,
    institutional, generational, constrained, national).

% Non-mobile workers in receiving regions who face wage competition and labor market pressure from incoming mobile workers but lack the resources or skills to relocate for better opportunities.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, static_labor_pool, payer,
    powerless, biographical, trapped, local).

% Individuals whose applications for cross-border residency or work are denied on proportionality grounds such as public health or welfare system capacity. They bear the direct cost of the constraint's limiting function.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobility_applicants_denied, payer,
    powerless, immediate, trapped, continental).

% Initiates infringement proceedings against member states that impose disproportionate restrictions on mobility. It enforces treaty compliance and shapes the proportionality agenda through administrative and legislative proposals.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, eu_commission, agenda_setter,
    institutional, generational, analytical, continental).

% Adjudicates proportionality disputes between mobile citizens and host states. Its jurisprudence defines the boundaries of legitimate national interest and interprets the treaty constraints.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, ecj, observer,
    institutional, generational, analytical, continental).

% Political groups and movements arguing for blanket member state control over immigration and welfare access. They are structurally excluded from the proportionality framework because their preferred outcome would eliminate mobility rights entirely.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, excluded_sovereigntist_factions, excluded,
    organized, generational, constrained, national).

% Federalist and market-integration advocates who argue for unconditional mobility with minimal state restrictions. They are underrepresented in proportionality doctrine because the framework explicitly permits host state limitations.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, excluded_integration_advocates, excluded,
    organized, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cross-border labor mobility and service provision within a multi-state economic union by establishing a common standard (proportionality) for when host states may restrict movement, preventing both automatic closure and automatic openness.
% TRANSFER_FUNCTION: Transfers regulatory autonomy from host member states to mobile citizens and EU judicial institutions; transfers labor market risk and competitive pressure from mobile capital to static local workers.
% ABSENT_VOICES: Sovereigntist factions demanding blanket immigration controls and federalist factions demanding unconditional mobility rights are both structurally underrepresented in proportionality jurisprudence; national parliaments often dissent from ECJ interpretations but lack authoritative override capacity.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, member states would unilaterally restrict mobility, fragmenting the internal market; labor mobility would likely collapse under political pressure, and the EU legal order would revert to uncoordinated national immigration regimes or trigger disintegration dynamics.
% FOUNDING_PROBLEM: Post-war European economic fragmentation and the need to prevent nationalist closure of labor markets while respecting legitimate state regulatory interests in public order and welfare.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions attest the problem remains live citing successive enlargements and market completion; host state governments and independent federalism scholars outside the beneficiary core attest the problem has transformed into one of political legitimacy and welfare state preservation, corroborating the contested status.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the constraint systematically transfers regulatory autonomy from host states to mobile individuals and EU institutions, and imposes competitive costs on static workers. It is not higher because genuine coordination benefits (labor matching, economic integration) are real and substantial. Suppression (0.52) is moderate because the constraint actively suppresses both blanket state restrictions and unconditional mobility claims through judicial enforcement. Theater ratio (0.25) reflects that proportionality review is a real legal practice, but involves performative balancing rhetoric that sometimes masks political choices. Accessibility collapse (0.45) is incomplete: alternatives exist (treaty withdrawal, revision) but are politically and economically prohibitive. Resistance (0.48) is significant and sustained, coming from host states, populist movements, and static labor. The measurement grid is shared across all three tracked metrics.
 *
 * PERSPECTIVAL GAP:
 *   The EU Commission seat computes the constraint as coordinating a single market through shared rules. The host state government seat computes the same legal structure as extracting democratic self-determination and fiscal sovereignty. The static labor pool experiences extraction through labor market competition without the mobility option that would let them benefit. Mobile workers experience the constraint as enabling rights that are contingent and litigated rather than secure. These divergences are structurally derived from the same constraint facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (cross_border_labor, receiving_sector_employers) are structurally subsidized by the constraint: their directionality is near the beneficiary end (low d), amplified by mobile exit options. Victims (host_state_governments, static_labor_pool, mobility_applicants_denied) bear costs: host states have constrained exit and institutional power that partially damps their d, while static workers and denied applicants with trapped exit sit near the full-target end (high d). The Commission and ECJ, as analytical institutional enforcers, sit near the administrator/beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpost-war economic fragmentation and nationalist closure of marketsâhas been transformed by enlargement and globalisation. The constraint persists because it coordinates a real single market, not because its original problem remains identical. It is not a piton because the coordination function is not atrophied; it is not a scaffold because no sunset clause exists; it is not a snare because the coordination is genuine and not merely cover; it is not a rope because enforcement is active and victim sets are structurally present. The classification as tangled_rope captures the hybrid nature: coordination and asymmetric extraction are co-constitutive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_indeterminacy,
    'Does proportionality review in free movement law constitute a genuine legal constraint on state power, or an indeterminate standard that licenses judicial policy-making?',
    'Quantitative analysis of ECJ outcomes: if proportionality outcomes correlate with policy preferences rather than legal factors, indeterminacy is high; if they show predictable deference patterns, constraint is real.',
    'High indeterminacy would push classification toward snare (extraction via judicial discretion); low indeterminacy supports tangled_rope (real coordination with bounded extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_indeterminacy, empirical, 'Empirical indeterminacy of proportionality doctrine').

omega_variable(
    enlargement_as_transformation,
    'Did the 2004 and 2007 enlargements transform free movement from a coordination mechanism into an extraction mechanism for older member states?',
    'Comparative welfare-state fiscal analysis pre- and post-enlargement, measuring net fiscal transfers and labor market displacement in receiving regions.',
    'If extraction is enlargement-dependent, the constraint may have drifted from rope toward tangled_rope or snare over the interval; if structural, the authored metrics are stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enlargement_as_transformation, empirical, 'Whether post-enlargement dynamics changed the extraction/coordination balance').

omega_variable(
    national_identity_safety_valve,
    'Is the Article 4(2) TEU national identity reservation a genuine legal limit on free movement proportionality, or a theatrical safety valve that absorbs political resistance without altering outcomes?',
    'Case-law census of successful national identity invocations in mobility cases versus total pool; if success rate is negligible, the reservation is theater.',
    'If theatrical, theater_ratio is understated and the constraint''s suppressive function is higher than authored; if genuine, the constraint permits more exit than modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_identity_safety_valve, empirical, 'Empirical efficacy of national identity reservations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t6, federation_membership_treaty__subsidiarity_balance, theater_ratio, 6, 0.18).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__subsidiarity_balance, theater_ratio, 12, 0.22).
narrative_ontology:measurement(fede_tr_t18, federation_membership_treaty__subsidiarity_balance, theater_ratio, 18, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.28).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__subsidiarity_balance, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t6, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(fede_be_t18, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fede_su_t6, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(fede_su_t18, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_treaty kernel. The integration_primary and sovereignty_primary readings instantiate structurally distinct claims with different beneficiary/victim structures and Îµ values. This reading occupies the intermediate position, asserting proportionality as the balancing mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
