% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Constitutive Right (Integration Reading — Expansive ECJ Doctrine)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story authors the integration reading of the
 *   federation_membership_kernel: free movement as a fundamental,
 *   constitutive right of EU citizenship, whose scope the ECJ interprets
 *   expansively to complete the single market and maximize labor mobility and
 *   equal treatment. On this reading's own terms, the doctrine is a genuine
 *   coordination achievement — it dismantled protectionist national
 *   labor-market fragmentation and gave concrete content to EU citizenship.
 *   But assessed by this reading's own lights (not by an endorsed
 *   alternative), the standing arrangement it describes has also, over four
 *   decades of case law, expanded to displace local labor competitively,
 *   shift uncompensated welfare and training costs onto receiving and sending
 *   states respectively, and override national labor-market protections
 *   through judicial reinterpretation rather than treaty amendment or
 *   legislative consent — hence tangled_rope rather than a pure rope or a
 *   mountain of natural constitutional necessity.
 *
 * KEY AGENTS:
 *   - european_court_of_justice: supranational agenda-setter, expansive interpretive authority, no electoral accountability
 *   - mobile_eu_workers: primary beneficiary, arbitrage-grade exit across 27 labor markets
 *   - cross_border_employers: beneficiary, captures lowered labor procurement friction
 *   - displaced_local_labor: primary target, trapped exit, bears wage-competition cost
 *   - receiving_state_welfare_systems: institutional payer, bears uncompensated fiscal cost
 *   - sending_state_public_services: payer, bears externalized brain-drain cost
 *   - national_labor_market_regulators: payer/excluded, loses predictable regulatory authority
 *   - excluded_national_electorates: excluded, no binding channel against ECJ doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.62).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Constitutive Right (Integration Reading — Expansive ECJ Doctrine)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, 'd32dc7fa-962e-4eb1-a6c7-0ae532878d0f').
narrative_ontology:cs_kernel_codification('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', formalized).
narrative_ontology:cs_authority_grounding('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', lineage).
narrative_ontology:cs_interpretation_layer_present('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f').
narrative_ontology:cs_reading_relation('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', foundational, free_movement_constitutive_of_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', free_movement_constitutive_of_citizenship, conventional).
narrative_ontology:cs_axiom('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', secondary, expansive_interpretation_maximizes_integration_value).
narrative_ontology:cs_axiom_status(expansive_interpretation_maximizes_integration_value, holdable).
narrative_ontology:cs_axiom_grounding('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', expansive_interpretation_maximizes_integration_value, instrumental).
narrative_ontology:cs_reference_frame('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', treaty_of_rome_market_completion_mandate).
narrative_ontology:cs_drift_state('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', post_maastricht_citizenship_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d32dc7fa-962e-4eb1-a6c7-0ae532878d0f', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, single_market_integration_project).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, ecj_institutional_authority).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_public_services).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, national_labor_market_regulators).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, eu_citizenship_as_fundamental_status).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, single_market_completion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Treaty free movement provisions expansively through preliminary rulings, progressively extending the scope of who counts as a 'worker,' what counts as 'equal treatment,' and when national residency or contribution requirements constitute unlawful discrimination. Its rulings bind national courts and legislatures; it has no electoral accountability and cannot be overridden except by unanimous Treaty change, which is practically unavailable.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_court_of_justice, agenda_setter,
    institutional, civilizational, analytical, continental).

% Move across member state borders to access higher wages, better welfare benefits, or employment unavailable at home. Free movement plus expansive equal-treatment doctrine gives them access to host-state labor markets and, increasingly, host-state welfare entitlements without full contribution history — a mobility premium unavailable to non-EU migrants or to natives without comparable exit options.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    organized, biographical, arbitrage, continental).

% Draw on an enlarged, wage-flexible labor pool across the Union without the friction of work permits or quota systems. Expansive free movement doctrine directly lowers their labor procurement costs and lets them relocate production or hire seasonally across borders with minimal regulatory friction.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, cross_border_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Compete for jobs and wages against an enlarged cross-border labor supply they had no voice in admitting. Where national labor law once conditioned market access on residency, licensing, or local-hire preference, ECJ rulings have struck these down as free-movement violations. Local workers cannot exit the national labor market as easily as capital or mobile labor can enter it — many lack the language skills, portable qualifications, or savings to relocate themselves.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, biographical, trapped, national).

% Fund unemployment benefits, healthcare, housing subsidies, and family benefits extended to mobile EU citizens under equal-treatment rulings, often before those citizens have paid into the system through work or taxes. National governments retain nominal control over welfare design but ECJ case law on Union citizenship (e.g. extending residence-based entitlement) narrows their room to condition benefits on contribution history or genuine attachment, without any corresponding fiscal transfer mechanism from sending states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Trained doctors, nurses, engineers, and skilled tradespeople who emigrate to higher-wage member states, taking the return on years of publicly funded education and training with them. Sending states bear the training cost and the demographic and service-capacity consequences (rural hospital closures, understaffed schools) while receiving states capture the labor value; free movement doctrine treats this as a feature of integration, not a transfer requiring compensation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_public_services, payer,
    moderate, generational, constrained, national).

% Design licensing regimes, collective bargaining structures, and labor protections calibrated to national wage levels and social models. ECJ proportionality review routinely strikes down measures found to indirectly restrict free movement, even when domestically justified as consumer protection or labor market stability, leaving regulators unable to predict which national rules will survive judicial review.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_labor_market_regulators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, national_labor_market_regulators, excluded).

% The abstract project of an integrated European market and polity is advanced each time free movement doctrine expands, since labor mobility is treated as one of the four foundational freedoms whose completion is the measure of integration's success. Not an actor itself but named for completeness — it collects no rents but is invoked to justify the doctrine's expansion.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, single_market_integration_project, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__integration_reading, single_market_integration_project).

% Vote in national elections that produce labor-market and welfare policy, but cannot vote out the ECJ or amend the Treaty provisions the Court interprets. Where national referenda or legislation have attempted to condition free movement (residency tests, benefit waiting periods), these have frequently been struck down or narrowed by preliminary rulings, leaving the electorate's expressed preferences without a binding channel against the doctrine.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, excluded_national_electorates, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Free movement plus non-discrimination doctrine solves a genuine collective-action problem: without it, twenty-seven national labor markets would fragment behind protectionist barriers, undermining the single market's core premise that labor, like capital and goods, should flow to its most productive use across the Union.
% TRANSFER_FUNCTION: The doctrine moves labor supply from lower-wage sending states to higher-wage receiving states, and moves welfare, wage-competition, and training-cost burdens from mobile workers and their employers onto displaced local labor, receiving-state welfare systems, and sending-state public services — without a corresponding fiscal transfer mechanism to compensate the burdened parties.
% ABSENT_VOICES: Displaced local labor and national electorates are structurally present in national democratic processes but absent from the actual venue where the doctrine is made — ECJ chambers interpreting Treaty text through preliminary references initiated by litigants, not by referendum or legislative debate. Sending-state public service administrators, who bear brain-drain costs, have no standing in the free-movement litigation their consequences flow from at all.
% DISAPPEARANCE_RATIONALE: If expansive ECJ free-movement doctrine were rolled back to a narrower, sovereignty-respecting reading, national labor markets would re-erect eligibility conditions on welfare access, member states would reintroduce contribution or residency thresholds, cross-border employers would face higher compliance costs for hiring across borders, and mobile workers would lose immediate equal-treatment entitlements — a substantial rearrangement of who bears the costs of European integration.
% FOUNDING_PROBLEM: The founding problem was market fragmentation: national barriers to labor mobility (work permits, licensing walls, discriminatory residency rules) prevented the common market from functioning as an integrated economic space and blocked EU citizens from exercising the four freedoms symmetrically.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and integrationist scholars attest the founding problem remains live — residual national barriers to mobility persist and require continued judicial vigilance. Independent labor economists studying wage compression in receiving-state low-skill sectors, and national auditors documenting uncompensated welfare and training-cost transfers, attest from outside the ECJ and Commission that the doctrine's current scope now creates distributive problems the original market-fragmentation rationale does not by itself justify — corroboration exists on both sides of the contest, which is why the status is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — moderate-high, not extreme — because the coordination function (dismantling protectionist market fragmentation) is real and substantial, but four decades of expansive interpretation have layered distributive extraction onto that coordination core without fiscal compensation mechanisms. Suppression is authored at 0.62, higher than extractiveness, because the mechanism by which national labor-market protections are overridden is judicial reinterpretation binding on member states with no legislative override available short of unanimous Treaty change — this is a structurally coercive channel independent of how much net value the doctrine produces. Theater ratio is low (0.22) because the coordination function is substantively performed, not merely staged; the ECJ's docket is functioning judicial review, not theater. Resistance is authored high (0.68) reflecting sustained national political contestation (welfare chauvinism debates, Brexit's free-movement dimension, repeated attempts at benefit-eligibility restriction) that the doctrine has largely withstood rather than accommodated. Accessibility collapse is moderate (0.45): member states retain formal sovereignty over welfare design and labor law, but the practical alternative space for restricting free movement has narrowed substantially as case law accumulates precedent.
 *
 * PERSPECTIVAL GAP:
 *   From the ECJ's own institutional vantage and from mobile workers' vantage, this arrangement is close to a rope: coordination achieved, mobility enabled, discrimination barriers removed. From displaced local labor and receiving-state welfare administrators' vantage, the same legal architecture operates as an enforced transfer they had no voice in authorizing and cannot exit. The engine computes these as different per-seat classifications from the same structural data — this story does not adjudicate between them; it authors the structural facts that produce the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU workers and cross-border employers sit near the beneficiary end: they have arbitrage-grade exit (workers can relocate; employers can restructure hiring across borders) and the doctrine's expansion directly increases their available surplus. Displaced local labor sits at the target end: trapped exit (limited by language, credentials, savings), no voice in the venue where the doctrine is made, and direct wage-competition exposure. Receiving-state welfare systems and sending-state public services are institutional payers with only constrained exit — they cannot leave the EU legal order without extraordinary political cost (see Brexit), and cannot unilaterally reject ECJ rulings without infringement proceedings. National labor market regulators experience directionality asymmetrically depending on which measure is being reviewed: protective measures they design are frequently struck down, converting their agenda-setting role into an effectively reactive one.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (ending labor-market fragmentation) has not become obsolete — cross-border hiring and worker mobility remain economically significant and popular among mobile populations — which is why founding_problem_status is authored as contested rather than dead. This prevents the constraint from being mislabeled as pure extraction: there is a live coordination achievement underneath the extraction layer. But the tangled_rope classification captures that the coordination function no longer bounds the doctrine's scope — expansion has continued well past dismantling fragmentation into territory (uncompensated welfare cost-shifting, override of domestically-justified labor protections) that serves integration-project momentum more than it serves the original market-completion rationale, which is exactly the divergence a tangled rope names.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_right_vs_policy_choice,
    'Is free movement a constitutive element of EU citizenship that admits no bounding by national welfare capacity (as this reading holds), or is it a policy instrument whose scope was always meant to be calibrated against member-state fiscal and labor-market conditions?',
    'Treaty drafting history and founding-era ECJ jurisprudence (Van Gend en Loos through early free-movement cases) versus the trajectory of case law expansion post-Maastricht citizenship provisions; comparison with how other federal systems (US Commerce Clause, Swiss cantonal mobility) resolved analogous tensions.',
    'If free movement is genuinely constitutive rather than a calibrated policy choice, the integration reading''s expansive doctrine is the correct reading and the sibling readings represent illegitimate retrenchment. If it was always meant to be bounded, this reading''s expansion represents doctrinal overreach beyond the kernel''s original scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_right_vs_policy_choice, conceptual, 'Whether free movement''s scope is constitutionally fixed or a bounded policy instrument — the central interpretive fork between this reading and its siblings.').

omega_variable(
    fiscal_compensation_feasibility,
    'Could a fiscal transfer mechanism compensate receiving-state welfare systems and sending-state public services for the costs this reading''s doctrine generates, without requiring the doctrine itself to narrow?',
    'Examine EU cohesion fund mechanisms and whether any have been extended to address free-movement-specific fiscal externalities; model the transfer volumes required against current EU budget constraints and member-state veto dynamics.',
    'If a compensation mechanism is fiscally and politically feasible, the extractive dimension of this reading could in principle be addressed without narrowing the doctrine''s scope, converting the tangled_rope toward a cleaner rope. If infeasible under current EU budgetary and political constraints, the extraction is likely to remain structural for the foreseeable interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_compensation_feasibility, empirical, 'Whether the distributive costs this reading generates are fiscally addressable without doctrinal retreat.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_layer,
    'Should the commitment-system framing here be anchored to the Treaty text itself (fixed_text) or to the ECJ''s own doctrine of Union citizenship as an evolving, self-legitimating body of case law that has increasingly detached its authority from close textual grounding?',
    'Compare ECJ reasoning patterns across decades: early free-movement rulings closely track Treaty text (fixed_text framing fits well); later Union-citizenship rulings (e.g. extending residence-based social benefits) increasingly reason from the ''fundamental status'' language and prior case law rather than fresh textual analysis (favoring a formalized, self-referential kernel framing).',
    'Under the fixed_text framing, drift is more visible and contestable as departure from text; under the formalized/self-referential framing, the doctrine appears more as an internally coherent, closed system whose legitimacy is harder to contest from outside — this changes how the drift_state (practice_drift vs. stable) should be read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_layer, conceptual, 'Whether the CS kernel is best framed as the Treaty text or as the ECJ''s accumulated Union-citizenship jurisprudence layered above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t8, federation_membership_kernel__integration_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(fede_tr_t16, federation_membership_kernel__integration_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(fede_tr_t24, federation_membership_kernel__integration_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(fede_tr_t32, federation_membership_kernel__integration_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(fede_tr_t40, federation_membership_kernel__integration_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fede_be_t8, federation_membership_kernel__integration_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(fede_be_t16, federation_membership_kernel__integration_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(fede_be_t24, federation_membership_kernel__integration_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(fede_be_t32, federation_membership_kernel__integration_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(fede_be_t40, federation_membership_kernel__integration_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fede_su_t8, federation_membership_kernel__integration_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(fede_su_t16, federation_membership_kernel__integration_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(fede_su_t24, federation_membership_kernel__integration_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(fede_su_t32, federation_membership_kernel__integration_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(fede_su_t40, federation_membership_kernel__integration_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'EU free movement rights' per the ε-invariance principle: integration_reading (this file, ε=0.58, tangled_rope), member_sovereignty_reading (bounded-mobility framing, expected lower ε and different victim set centered on fiscal sustainability), and welfare_coordination_reading (anti-social-dumping enforcement with preserved welfare autonomy, expected intermediate ε). Each reading has its own beneficiary/victim structure and its own claimed type; they are linked via affects_constraints rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
