% ============================================================================
% CONSTRAINT STORY: bounded_ownership_vs_capital_scale
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bounded_ownership_vs_capital_scale, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: bounded_ownership_vs_capital_scale
 *   human_readable: 500-Seat Natural-Person Ownership Cap with Prohibited Recursive Corporate Ownership
 *   domain: constitutional_political_economy/monetary_theory/corporate_property_law
 *
 * SUMMARY:
 *   A constitutional-corporate design caps ownership of any enterprise at 500
 *   natural-person seats and bars recursive corporate ownership above that
 *   cap, meaning no holding-company pyramid can reconstruct concentrated
 *   control through layered entities. The declared theory is that capital
 *   scale can be decoupled from ownership scale: enterprises needing capital
 *   far beyond what 500 individuals can supply are meant to raise it through
 *   debt, consociation agreements among multiple capped entities, and bank
 *   intermediation, none of which confer ownership seats. The primary
 *   empirical question this story is built to observe is whether, in
 *   practice, capital-intensive ventures like semiconductor fabs and orbital
 *   infrastructure can actually be financed this way without creditors and
 *   consociation partners informally reconstructing the very concentrated
 *   control the ownership cap was designed to prevent — through covenant
 *   terms, board-observer and veto rights, and default-remedy provisions that
 *   substitute for equity control. Over the measured interval, the evidence
 *   increasingly points toward reconstruction: creditor-governance
 *   instruments accumulate the practical attributes of ownership control even
 *   as the formal seat registry stays compliant with the cap.
 *
 * KEY AGENTS:
 *   - distributed_natural_person_owners: formal beneficiaries of the cap, thin financial capacity relative to project scale
 *   - would_be_concentrated_equity_holders: excluded from proportionate ownership, redirected into debt/consociation instruments
 *   - undercapitalized_capital_intensive_ventures: bear the cost of assembling financing without concentrated equity
 *   - creditor_banks_and_syndicates: informally accumulate governance leverage through covenant and board-control rights
 *   - consociation_partners: pool capacity across capped entities, recreating concentration one level removed
 *   - judiciary_property_termination_organ: adjudicates whether financing structures violate the cap's substance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bounded_ownership_vs_capital_scale, 0.58).
domain_priors:suppression_score(bounded_ownership_vs_capital_scale, 0.62).
domain_priors:theater_ratio(bounded_ownership_vs_capital_scale, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bounded_ownership_vs_capital_scale, extractiveness, 0.58).
narrative_ontology:constraint_metric(bounded_ownership_vs_capital_scale, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bounded_ownership_vs_capital_scale, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bounded_ownership_vs_capital_scale, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bounded_ownership_vs_capital_scale, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bounded_ownership_vs_capital_scale, tangled_rope).
narrative_ontology:human_readable(bounded_ownership_vs_capital_scale, "500-Seat Natural-Person Ownership Cap with Prohibited Recursive Corporate Ownership").
narrative_ontology:topic_domain(bounded_ownership_vs_capital_scale, "constitutional_political_economy/monetary_theory/corporate_property_law").

domain_priors:requires_active_enforcement(bounded_ownership_vs_capital_scale).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bounded_ownership_vs_capital_scale, 'e5ebc756-d850-4e99-9886-0736b5dd3dc9').
narrative_ontology:cs_kernel_codification('e5ebc756-d850-4e99-9886-0736b5dd3dc9', formalized).
narrative_ontology:cs_authority_grounding('e5ebc756-d850-4e99-9886-0736b5dd3dc9', extraction).
narrative_ontology:cs_interpretation_layer_present('e5ebc756-d850-4e99-9886-0736b5dd3dc9').
narrative_ontology:cs_reference_frame('e5ebc756-d850-4e99-9886-0736b5dd3dc9', distributed_natural_person_control_baseline).
narrative_ontology:cs_drift_state('e5ebc756-d850-4e99-9886-0736b5dd3dc9', post_capital_intensive_financing_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5ebc756-d850-4e99-9886-0736b5dd3dc9', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bounded_ownership_vs_capital_scale, distributed_natural_person_owners).
narrative_ontology:constraint_victim(bounded_ownership_vs_capital_scale, would_be_concentrated_equity_holders).
narrative_ontology:constraint_victim(bounded_ownership_vs_capital_scale, undercapitalized_capital_intensive_ventures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bounded_ownership_vs_capital_scale, creditor_banks_and_syndicates).
narrative_ontology:constraint_beneficiary(bounded_ownership_vs_capital_scale, consociation_partners).
narrative_ontology:constraint_victim(bounded_ownership_vs_capital_scale, consociation_partners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each holds a bounded share within the 500-seat cap and cannot be diluted by recursive corporate ownership stacking above them. They collectively retain formal control of the enterprise, but individually most have no capacity to underwrite the capital needs of a fab or orbital platform, so their formal ownership is real but often financially thin relative to the venture's total capitalization.
narrative_ontology:constraint_stakeholder(bounded_ownership_vs_capital_scale, distributed_natural_person_owners, beneficiary,
    organized, generational, constrained, national).

% Sovereign wealth funds, private equity consortia, and strategic industrial investors who would ordinarily take concentrated equity stakes proportionate to the capital they supply are structurally barred from doing so by the seat cap and the recursive-ownership prohibition. They can still supply capital, but only through debt, consociation agreements, or bank intermediation instruments that do not confer ownership seats, which they experience as an artificial ceiling on the control their capital would otherwise command.
narrative_ontology:constraint_stakeholder(bounded_ownership_vs_capital_scale, would_be_concentrated_equity_holders, excluded,
    powerful, biographical, constrained, global).

% Semiconductor fabs, orbital infrastructure operators, and similar ventures that require capital far exceeding what 500 natural-person seats can plausibly self-fund. They must assemble financing entirely from debt, consociation, and bank-intermediated instruments, which raises their cost of capital, lengthens time-to-financing, and in some cases forecloses projects that a concentrated-ownership jurisdiction would fund readily.
narrative_ontology:constraint_stakeholder(bounded_ownership_vs_capital_scale, undercapitalized_capital_intensive_ventures, payer,
    moderate, biographical, trapped, global).

% Because equity concentration is capped, capital-intensive ventures depend on debt and bank intermediation to reach necessary scale. Creditor syndicates supply this capital and, in exchange, negotiate covenants, board-observer rights, veto triggers, and default-remedy provisions that give them de facto governance control without holding ownership seats. They set the practical terms under which the venture operates and collect origination fees, interest spreads, and covenant-protected priority regardless of equity outcomes.
narrative_ontology:constraint_stakeholder(bounded_ownership_vs_capital_scale, creditor_banks_and_syndicates, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bounded_ownership_vs_capital_scale, creditor_banks_and_syndicates, beneficiary).

% Multiple ventures or investor groups pool resources through consociation structures to reach capital scale without breaching the ownership cap on any single entity. They gain access to projects otherwise unreachable, but the consociation agreements themselves become sites of negotiated control allocation, replicating some of the concentration the ownership cap was meant to prevent, one level removed from the cap-holding corporate person.
narrative_ontology:constraint_stakeholder(bounded_ownership_vs_capital_scale, consociation_partners, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(bounded_ownership_vs_capital_scale, consociation_partners, payer).

% Adjudicates disputes over whether particular covenant structures, board-control arrangements, or creditor-governance rights constitute an informal reconstruction of prohibited concentrated ownership. Its rulings determine whether the cap's substance survives contact with sophisticated financing instruments or is hollowed out by contractual workarounds it lacks the technical capacity to fully police.
narrative_ontology:constraint_stakeholder(bounded_ownership_vs_capital_scale, judiciary_property_termination_organ, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bounded_ownership_vs_capital_scale, diffuse).
narrative_ontology:fixing_cost_class(bounded_ownership_vs_capital_scale, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single natural person or recursively-owned corporate entity from accumulating enough ownership seats to capture governance of large enterprises, preserving a genuinely distributed base of formal control across the economy and blocking the re-concentration of productive property that plagued prior corporate forms.
% TRANSFER_FUNCTION: Moves governance leverage away from concentrated equity holders and toward two other locations simultaneously: formally, toward the 500 distributed natural-person seat-holders; informally, toward creditor syndicates and consociation partners who reconstruct control through covenant and board-observer rights rather than ownership, extracting fees and priority claims along the way.
% ABSENT_VOICES: Sovereign and institutional capital pools that would supply financing directly in exchange for proportionate equity control are not permitted a seat at the ownership table at all; their objection — that the cap misprices the risk they bear by denying them commensurate control — is heard only indirectly, through the covenant terms they negotiate as creditors rather than as owners.
% DISAPPEARANCE_RATIONALE: Distributed owners and the judiciary would say the world rearranges catastrophically — recursive ownership stacking would resume within a generation and the seat cap's entire coordination function would be lost. Creditor syndicates and consociation partners would say comparatively little changes for them operationally, since they already hold the practical governance levers through covenant and board-control rights; only the formal ownership register would shift, not the actual locus of control.
% FOUNDING_PROBLEM: Historical episodes of recursive corporate ownership (holding companies owning holding companies) allowed small groups to pyramid control over vast productive capacity while formally diffusing liability, producing both economic concentration and accountability gaps that regulators repeatedly failed to unwind after the fact.
% FOUNDING_PROBLEM_CORROBORATION: Distributed owners and the judiciary attest the founding problem — pyramided recursive control evading accountability — remains live and the cap is actively preventing its recurrence. Independent financial-structure analysts outside both the beneficiary and creditor camps attest that the problem has re-emerged in a different form: covenant-based and consociation-based control now performs much of the function recursive ownership used to perform, suggesting the cap has redirected rather than eliminated concentration, a claim creditor syndicates themselves do not contest but do not volunteer either.
narrative_ontology:disappearance_verdict(bounded_ownership_vs_capital_scale, contested).
narrative_ontology:founding_problem_status(bounded_ownership_vs_capital_scale, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bounded_ownership_vs_capital_scale, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-08',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(bounded_ownership_vs_capital_scale, 'none', 1).
narrative_ontology:epsilon_provenance(bounded_ownership_vs_capital_scale, 0.58, 'claude-sonnet-5', 'c2_monetary_architecture_2026_20260808_170220', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bounded_ownership_vs_capital_scale_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bounded_ownership_vs_capital_scale, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bounded_ownership_vs_capital_scale_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.38 to 0.58) as creditor syndicates and consociation structures mature into standardized covenant packages that give them de facto control disproportionate to any capital they'd receive under a pure debt-pricing model. Theater ratio also rises (0.20 to 0.40): compliance with the seat cap becomes increasingly formal — the ownership register is scrupulously maintained even as governance substance migrates into instruments the cap does not reach, producing a growing gap between what the register shows and what actually decides the venture's direction. Suppression rises moderately (0.45 to 0.62) as the judiciary and legislature invest more enforcement effort defending the cap's letter against increasingly sophisticated workarounds, without necessarily reaching its spirit. Accessibility collapse is moderate (0.5): capital-intensive ventures genuinely have alternative financing architectures available in principle, but the cap's design increasingly narrows practical options toward the instruments most prone to informal reconstruction. Resistance is high (0.7): both excluded equity holders and the judiciary actively contest the current equilibrium from opposite directions, which is itself evidence this is not a settled mountain but a live, actively defended structure.
 *
 * PERSPECTIVAL GAP:
 *   From the distributed-owner seat, the constraint reads as a working rope: formal control stays genuinely dispersed, recursive pyramiding is blocked, and the coordination function the cap was built for is intact. From the creditor-syndicate and would-be-concentrated-equity-holder seats, the same structure reads as a tangled rope at best — a formally distributed ownership layer sitting atop an informally concentrated control layer they now occupy, having been pushed there by the cap itself. The judiciary's seat sees both truths simultaneously and cannot fully resolve them, because covenant sophistication outruns the interpretive doctrine available to test it.
 *
 * DIRECTIONALITY LOGIC:
 *   Distributed natural-person owners are coded as beneficiaries: the cap subsidizes their formal control position relative to what unrestricted markets would produce, even though their financial exposure to the venture may be thin. Would-be concentrated equity holders and undercapitalized ventures are coded as victims: the cap forecloses their preferred financing/control structure and imposes higher transaction costs and slower capital formation. Creditor syndicates occupy an unusual position — nominally payers of nothing and holders of no equity, they are structurally the ones who benefit most from the cap's redirection of control-seeking capital into debt instruments, since debt intermediation is their core business; they are coded as agenda-setter/beneficiary because they set the practical terms of governance without bearing the formal costs the cap was designed to impose on concentrated owners.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pyramided recursive ownership evading accountability — has not disappeared, but the mechanism by which concentration reasserts itself has migrated from equity structure to credit structure. This is precisely the situation the tangled_rope classification is designed to catch: a genuine coordination function (blocking recursive equity pyramids) persists and is not mere theater, but it now coexists with a substantial, actively enforced extraction channel (creditor-governance capture) that rides on the same structure. Classifying this as a pure rope would miss the informally reconstructed concentration; classifying it as a pure snare would miss the real and continuing distribution of formal ownership among 500 natural persons. Only the hybrid classification holds both facts without collapsing one into the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_control_equivalence_threshold,
    'At what point does a package of covenants, board-observer rights, and default-remedy triggers become functionally equivalent to the concentrated equity control the seat cap was designed to prevent?',
    'Comparative case analysis of financing structures for comparable capital-intensive ventures (fabs, orbital platforms) under this regime versus jurisdictions permitting concentrated equity, coded for actual decision-rights exercised rather than formal instrument type.',
    'If covenant packages are found to be functionally equivalent to equity control in a substantial share of large financings, the tangled_rope classification is strongly supported and the cap''s coordination function is largely nominal for capital-intensive sectors; if functionally distinct, the cap is closer to a genuine rope with isolated extraction incidents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_control_equivalence_threshold, empirical, 'Whether creditor-governance rights reconstruct prohibited concentration in substance.').

omega_variable(
    consociation_as_recursive_ownership_analog,
    'Does consociation among multiple capped entities functionally recreate the recursive corporate ownership the prohibition targets, one structural layer removed?',
    'Trace control allocation within consociation agreements: if a small subset of consociation partners consistently holds disproportionate decision rights relative to their nominal capital contribution across multiple ventures, this indicates recursive-ownership-equivalent concentration operating through the consociation form.',
    'If consociation reliably reconstructs concentration, the recursive-ownership prohibition has a structural loophole the cap''s designers did not close, materially weakening the coordination story relative to the extraction story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consociation_as_recursive_ownership_analog, empirical, 'Whether consociation structures are a workaround for the recursive-ownership ban.').

omega_variable(
    capital_scale_decoupling_feasibility,
    'Is capital scale genuinely decouplable from ownership scale via debt, consociation, and bank intermediation for the most capital-intensive ventures, or does that decoupling claim depend on informally reconstructing ownership-equivalent control as its price of feasibility?',
    'Track financing outcomes for a cohort of semiconductor fab and orbital infrastructure projects: measure whether projects that successfully close financing do so with governance rights approximating equity control, versus projects that fail to close financing due to the absence of such rights being available under the cap''s instruments.',
    'If financing consistently succeeds only when creditor-governance rights approximate equity control, the decoupling claim is empirically false as stated and the cap''s founding theory needs revision; if financing succeeds without such rights in a meaningful share of cases, the decoupling claim holds and the extraction observed is incidental rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_scale_decoupling_feasibility, empirical, 'Whether the packet''s central claim — capital scale decoupled from ownership scale — survives contact with real capital-intensive financing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bounded_ownership_vs_capital_scale, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boun_tr_t0, bounded_ownership_vs_capital_scale, theater_ratio, 0, 0.2).
narrative_ontology:measurement(boun_tr_t4, bounded_ownership_vs_capital_scale, theater_ratio, 4, 0.25).
narrative_ontology:measurement(boun_tr_t8, bounded_ownership_vs_capital_scale, theater_ratio, 8, 0.3).
narrative_ontology:measurement(boun_tr_t12, bounded_ownership_vs_capital_scale, theater_ratio, 12, 0.34).
narrative_ontology:measurement(boun_tr_t16, bounded_ownership_vs_capital_scale, theater_ratio, 16, 0.37).
narrative_ontology:measurement(boun_tr_t20, bounded_ownership_vs_capital_scale, theater_ratio, 20, 0.39).
narrative_ontology:measurement(boun_tr_t24, bounded_ownership_vs_capital_scale, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(boun_be_t0, bounded_ownership_vs_capital_scale, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(boun_be_t4, bounded_ownership_vs_capital_scale, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(boun_be_t8, bounded_ownership_vs_capital_scale, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(boun_be_t12, bounded_ownership_vs_capital_scale, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(boun_be_t16, bounded_ownership_vs_capital_scale, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(boun_be_t20, bounded_ownership_vs_capital_scale, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(boun_be_t24, bounded_ownership_vs_capital_scale, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(boun_su_t0, bounded_ownership_vs_capital_scale, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(boun_su_t4, bounded_ownership_vs_capital_scale, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(boun_su_t8, bounded_ownership_vs_capital_scale, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(boun_su_t12, bounded_ownership_vs_capital_scale, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(boun_su_t16, bounded_ownership_vs_capital_scale, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(boun_su_t20, bounded_ownership_vs_capital_scale, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(boun_su_t24, bounded_ownership_vs_capital_scale, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bounded_ownership_vs_capital_scale, resource_allocation).
narrative_ontology:boltzmann_floor_override(bounded_ownership_vs_capital_scale, 0.12).
narrative_ontology:affects_constraint(bounded_ownership_vs_capital_scale, monetary_organ_issuance_authority).
narrative_ontology:affects_constraint(bounded_ownership_vs_capital_scale, judiciary_property_termination_doctrine).

% DUAL FORMULATION NOTE:
% This story is downstream of the broader constitutional kernel future_claims_present_resources: the ownership cap and recursive-ownership prohibition are themselves an answer to how a proposed future capital-intensive venture may legitimately command present resources without ownership-based concentration, and they interact with whichever monetary-issuance reading is operative (deliberative, endogenous-credit, physical-backing, or catallactic) because the availability and pricing of debt/consociation instruments depends on which issuance regime governs credit creation. This story does not itself adjudicate the issuance kernel; it assumes credit and debt instruments exist and are prices at some determinate rate, and asks only whether ownership-cap-compliant financing can be assembled without informally reconstructing prohibited concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bounded_ownership_vs_capital_scale, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
