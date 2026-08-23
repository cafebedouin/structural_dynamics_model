% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive of Single Market (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint is the integration_primary reading of the
 *   federation_membership_treaty kernel. It treats free movement of workers
 *   (and derived mobility rights) as constitutive of the single market,
 *   rendering national restrictions presumptively illegitimate unless they
 *   meet narrow, judicially supervised justification tests. The reading
 *   generates a structural asymmetry: mobile workers and cross-border firms
 *   benefit from expanded opportunity sets, while static workforces and
 *   host-state governments bear the costs of regulatory disempowerment and
 *   labor-market displacement. Federal institutions (judiciary and executive)
 *   actively enforce the constraint against member-state resistance. The
 *   constraint is claimed as tangled_rope because it carries a genuine
 *   coordination functionâintegrated labor allocation across a multi-state
 *   economyâalongside asymmetric extraction that systematically transfers
 *   autonomy and fiscal capacity away from host states and incumbent workers.
 *
 * KEY AGENTS:
 *   - Mobile workers (moderate/mobile): Primary beneficiariesâgain cross-border job access and equal treatment.
 *   - Cross-border firms (powerful/arbitrage): Secondary beneficiariesâexploit regulatory arbitrage and expanded labor supply.
 *   - Static workforce (moderate/constrained): Primary targetsâbear wage competition and reduced bargaining power with limited exit.
 *   - Host-state governments (institutional/constrained): Primary targetsâlose regulatory autonomy over labor markets and welfare access.
 *   - Federal judiciary (institutional/analytical): Agenda setterâinterprets and enforces the treaty presumption against restrictions.
 *   - Federal commission (institutional/analytical): Agenda setterâbrings infringement actions and monitors compliance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.74).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.86).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.74).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '885e09de-99ee-4bbc-8b17-dfec14c59c26').
narrative_ontology:cs_kernel_codification('885e09de-99ee-4bbc-8b17-dfec14c59c26', fixed_text).
narrative_ontology:cs_authority_grounding('885e09de-99ee-4bbc-8b17-dfec14c59c26', lineage).
narrative_ontology:cs_interpretation_layer_present('885e09de-99ee-4bbc-8b17-dfec14c59c26').
narrative_ontology:cs_reading_relation('885e09de-99ee-4bbc-8b17-dfec14c59c26', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('885e09de-99ee-4bbc-8b17-dfec14c59c26', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('885e09de-99ee-4bbc-8b17-dfec14c59c26', foundational, free_movement_constitutive_of_single_market).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_single_market, holdable).
narrative_ontology:cs_axiom_grounding('885e09de-99ee-4bbc-8b17-dfec14c59c26', free_movement_constitutive_of_single_market, conventional).
narrative_ontology:cs_axiom('885e09de-99ee-4bbc-8b17-dfec14c59c26', foundational, presumption_against_national_restrictions).
narrative_ontology:cs_axiom_status(presumption_against_national_restrictions, holdable).
narrative_ontology:cs_axiom_grounding('885e09de-99ee-4bbc-8b17-dfec14c59c26', presumption_against_national_restrictions, conventional).
narrative_ontology:cs_reference_frame('885e09de-99ee-4bbc-8b17-dfec14c59c26', integrated_single_market).
narrative_ontology:cs_drift_state('885e09de-99ee-4bbc-8b17-dfec14c59c26', post_enlargement_political_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('885e09de-99ee-4bbc-8b17-dfec14c59c26', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_firms).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, static_workforce).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, host_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, market_integration_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal entitlement to seek employment in any member state on equal terms with nationals. Their opportunity set expands as national barriers fall. They do not administer the constraint but are its primary intended beneficiaries.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from an expanded labor pool and the ability to post workers or establish operations without facing national barriers. Can arbitrage regulatory differences across member states.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_firms, beneficiary,
    powerful, biographical, arbitrage, continental).

% Face intensified labor-market competition as mobile workers enter domestic sectors. Experience downward wage pressure and reduced bargaining power. Cannot easily exit the constraint because skills, language, and social ties bind them to the national labor market.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, static_workforce, payer,
    moderate, biographical, constrained, national).

% Lose regulatory autonomy to restrict labor-market access or reserve public-sector jobs for nationals. Must justify any restriction by narrow criteria accepted by federal courts. Bear political and fiscal costs when mobile workers access welfare services.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, host_state_governments, payer,
    institutional, generational, constrained, national).

% Interprets the treaty as mandating free movement and strikes down national restrictions that fail narrow justification tests. Its authority is constituted by the treaty text and prior case law. It actively shapes the constraint's meaning through preliminary rulings.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federal_judiciary, agenda_setter,
    institutional, generational, analytical, continental).

% Brings infringement proceedings against member states that erect barriers to free movement. Proposes legislation to harmonize exceptions and monitors compliance. Its institutional mission is tied to market integration.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federal_commission, agenda_setter,
    institutional, generational, analytical, continental).

narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified economic space across member states by removing labor mobility barriers, enabling cross-border allocation of labor to match supply and demand without sovereign fragmentation.
% TRANSFER_FUNCTION: Moves labor-market access and regulatory autonomy from host states and static workers to mobile workers and cross-border employers; host states lose the ability to restrict entry or discriminate in favor of domestic labor.
% ABSENT_VOICES: Static workers facing wage compression and host-state taxpayers bearing fiscal externalities are underrepresented in federal treaty design compared to mobile workers and exporting firms; their objections are filtered through member-state governments which are simultaneously bound by the treaty.
% DISAPPEARANCE_RATIONALE: If the presumption of free movement and the illegitimacy of national restrictions vanished, member states would re-erect labor-market protections and border controls, the single market in services and labor would fragment into national segments, and the federal legal order would lose one of its constitutive pillars.
% FOUNDING_PROBLEM: Post-war economic fragmentation and protectionist labor-market closure across European states impeded growth and risked conflict through competitive nationalism.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and comparative federalism scholars attest the protectionist fragmentation problem was real at founding. Host-state governments and static-worker unions attest it has mutated into intra-federal competition and fiscal strain. Independent scholarship outside the mobile-worker and cross-border-firm beneficiary set corroborates both the original problem and its contemporary mutation.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the constraint systematically transfers regulatory and fiscal autonomy from host states to mobile factors. Suppression (0.86) is very high because the arrangement's persistence depends on federal institutions preempting and punishing national restrictions; without active enforcement, member states would re-erect barriers. Theater ratio (0.22) is low because enforcement is substantiveâECJ rulings and Commission infringement proceedings have real distributive effects rather than symbolic performance. Accessibility collapse (0.85) is high because legal alternatives to free movement within the single market are foreclosed by treaty supremacy and the direct-effect doctrine. Resistance (0.55) is moderate because member states and static workers consistently push back through opt-outs, derogation claims, and political obstruction, but they rarely prevail.
 *
 * PERSPECTIVAL GAP:
 *   Mobile workers and cross-border firms experience the constraint as a rope: it expands their opportunity set and solves a genuine coordination problem (matching labor to jobs across borders). Host-state governments and static workers experience it as a snare: their policy space and labor-market position are captured by a federal architecture they cannot individually veto. The engine computes this divergence from identical structural data by applying directional scaling to each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers (beneficiary, mobile exit) and cross-border firms (beneficiary, arbitrage exit) sit near the full-beneficiary end of directionality: the constraint subsidizes their mobility and market access. Static workforce (victim, constrained exit) and host-state governments (victim, constrained exit) sit near the full-target end: the constraint extracts regulatory autonomy and labor-market rents from them. The federal judiciary and commission (agenda-setters, analytical exit) sit near symmetric: they administer the constraint without personal cost or benefit, though their institutional authority is constituted by its enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The integration_primary reading resists simple mandatrophy because its founding problemâprotectionist fragmentationâhas not disappeared; rather, it has mutated into intra-federal asymmetry. The constraint would drift toward piton or snare only if the coordination function (market integration) were clearly dead while the extraction persisted. Here, the single market continues to coordinate labor allocation, so the constraint remains tangled_rope: genuine coordination overlaid with asymmetric extraction that has intensified with successive enlargements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Can the single market''s coordination function be preserved while allowing greater host-state labor-market autonomy?',
    'Natural experiments from opt-outs, sectoral exemptions, or differentiated integration models that maintain market access while restoring host-state regulatory space.',
    'If separable, the current constraint is extractive overlay on genuine coordination, confirming tangled_rope. If inseparable, the extraction is the necessary price of coordination, edging classification toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether market integration and host-state autonomy are structurally separable.').

omega_variable(
    founding_problem_liveness,
    'Has the founding problem of protectionist fragmentation been solved, or has it mutated into a new form that justifies the constraint''s persistence?',
    'Historical comparison of pre-treaty barriers with contemporary intra-federal labor-market outcomes; assessment of whether new barriers are political rather than economic.',
    'If the original problem is dead and no comparable replacement exists, mandatrophy flag rises toward piton. If mutated, the constraint may remain tangled_rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether the founding coordination problem remains live or is obsolete.').

omega_variable(
    suppression_scope_ambiguity,
    'Does the high suppression of national restrictions reflect legal supremacy alone, or is it reinforced by political and economic coercion against dissenting member states?',
    'Case studies of member-state defiance and federal retaliation (infringement procedures, funding conditionality, reputational sanctions).',
    'If suppression is purely legal-structural, the constraint''s classification rests on formal authority. If political coercion is substantial, the constraint leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_scope_ambiguity, empirical, 'Whether suppression is legal-structural or politically coercive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 67).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_ip_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fmt_ip_tr_t11, federation_membership_treaty__integration_primary, theater_ratio, 11, 0.12).
narrative_ontology:measurement(fmt_ip_tr_t22, federation_membership_treaty__integration_primary, theater_ratio, 22, 0.14).
narrative_ontology:measurement(fmt_ip_tr_t33, federation_membership_treaty__integration_primary, theater_ratio, 33, 0.16).
narrative_ontology:measurement(fmt_ip_tr_t44, federation_membership_treaty__integration_primary, theater_ratio, 44, 0.18).
narrative_ontology:measurement(fmt_ip_tr_t55, federation_membership_treaty__integration_primary, theater_ratio, 55, 0.2).
narrative_ontology:measurement(fmt_ip_tr_t67, federation_membership_treaty__integration_primary, theater_ratio, 67, 0.22).

% Extraction over time
narrative_ontology:measurement(fmt_ip_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fmt_ip_be_t11, federation_membership_treaty__integration_primary, base_extractiveness, 11, 0.48).
narrative_ontology:measurement(fmt_ip_be_t22, federation_membership_treaty__integration_primary, base_extractiveness, 22, 0.55).
narrative_ontology:measurement(fmt_ip_be_t33, federation_membership_treaty__integration_primary, base_extractiveness, 33, 0.62).
narrative_ontology:measurement(fmt_ip_be_t44, federation_membership_treaty__integration_primary, base_extractiveness, 44, 0.68).
narrative_ontology:measurement(fmt_ip_be_t55, federation_membership_treaty__integration_primary, base_extractiveness, 55, 0.71).
narrative_ontology:measurement(fmt_ip_be_t67, federation_membership_treaty__integration_primary, base_extractiveness, 67, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(fmt_ip_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fmt_ip_su_t11, federation_membership_treaty__integration_primary, suppression_requirement, 11, 0.6).
narrative_ontology:measurement(fmt_ip_su_t22, federation_membership_treaty__integration_primary, suppression_requirement, 22, 0.68).
narrative_ontology:measurement(fmt_ip_su_t33, federation_membership_treaty__integration_primary, suppression_requirement, 33, 0.74).
narrative_ontology:measurement(fmt_ip_su_t44, federation_membership_treaty__integration_primary, suppression_requirement, 44, 0.79).
narrative_ontology:measurement(fmt_ip_su_t55, federation_membership_treaty__integration_primary, suppression_requirement, 55, 0.83).
narrative_ontology:measurement(fmt_ip_su_t67, federation_membership_treaty__integration_primary, suppression_requirement, 67, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_treaty kernel, which decomposes into at least three structurally distinct claims: integration_primary (this file), sovereignty_primary, and subsidiarity_balance. Each reading carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
