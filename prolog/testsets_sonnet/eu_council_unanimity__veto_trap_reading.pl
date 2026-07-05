% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity as Veto-Extraction Mechanism
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This story reads the EU Council's unanimity requirement not as a
 *   sovereignty safeguard or a consensus-legitimacy device, but as a
 *   structural vulnerability: because a single dissent is sufficient to
 *   block, and blocking costs the dissenter almost nothing while imposing
 *   delay and dilution costs on the entire majority, unanimity converts any
 *   member state's marginal vote into a repeated extraction instrument.
 *   Hungary's conditioning of sanctions renewals and accession-chapter
 *   approvals on unrelated concessions, and comparable episodes involving
 *   other member states across budget and tax dossiers, are the observable
 *   instances of this reading. This is a distinct constraint from the
 *   sovereignty_guarantor_reading (which treats the same rule as legitimate
 *   protection of non-consenting states) and the diplomatic_capital_reading
 *   (which treats it as legitimacy-building consensus machinery) — the three
 *   readings share a kernel (the unanimity requirement itself) but
 *   instantiate structurally different constraints with different
 *   beneficiary/victim sets and different epsilon values, per the
 *   ε-invariance principle. This file models only the veto_trap_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.78).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.62).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity as Veto-Extraction Mechanism").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '803f9e26-d650-4772-a704-688cdb886fc0').
narrative_ontology:cs_kernel_codification('803f9e26-d650-4772-a704-688cdb886fc0', formalized).
narrative_ontology:cs_authority_grounding('803f9e26-d650-4772-a704-688cdb886fc0', extraction).
narrative_ontology:cs_interpretation_layer_present('803f9e26-d650-4772-a704-688cdb886fc0').
narrative_ontology:cs_reading_relation('803f9e26-d650-4772-a704-688cdb886fc0', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('803f9e26-d650-4772-a704-688cdb886fc0', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('803f9e26-d650-4772-a704-688cdb886fc0', foundational, blocking_cost_asymmetry_enables_extraction).
narrative_ontology:cs_axiom_status(blocking_cost_asymmetry_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('803f9e26-d650-4772-a704-688cdb886fc0', blocking_cost_asymmetry_enables_extraction, empirically_contingent).
narrative_ontology:cs_axiom('803f9e26-d650-4772-a704-688cdb886fc0', secondary, unrelated_concession_linkage_is_illegitimate_bargaining).
narrative_ontology:cs_axiom_status(unrelated_concession_linkage_is_illegitimate_bargaining, holdable).
narrative_ontology:cs_axiom_grounding('803f9e26-d650-4772-a704-688cdb886fc0', unrelated_concession_linkage_is_illegitimate_bargaining, conventional).
narrative_ontology:cs_reference_frame('803f9e26-d650-4772-a704-688cdb886fc0', founding_consent_protection_framework).
narrative_ontology:cs_drift_state('803f9e26-d650-4772-a704-688cdb886fc0', post_2010_enlargement_and_sanctions_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('803f9e26-d650-4772-a704-688cdb886fc0', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_commission).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, third_country_partners_awaiting_accession_or_sanctions).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, sovereign_equality_of_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a single vote equal in formal weight to the largest member states on any unanimity-gated dossier (sanctions renewal, accession, tax harmonization, multiannual budget). Signals willingness to block the entire package unless a side-concession, opt-out, rebate, or unrelated policy trade is delivered. Because the decision rule requires zero dissent, this state's threat is fully credible at negligible cost to itself: it need not build a blocking coalition, only decline to move. Extracts concessions repeatedly across dossiers, each time trading its single vote for a transfer that would not survive open majority bargaining.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, beneficiary,
    moderate, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter).

% Represent, collectively, the overwhelming preference of the Council on most blocked dossiers, yet cannot act without the holdout's assent. Must either delay indefinitely, exclude the item from EU competence entirely, or purchase the holdout's consent with side-payments funded from the shared budget or through carve-outs that dilute the policy for everyone. Their numerical and economic weight confers no proportional bargaining power under the unanimity rule; exit from the arrangement (leaving the Union, or moving the dossier outside EU treaty structures via intergovernmental workarounds) is costly and slow.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_states, payer,
    organized, generational, constrained, continental).

% Drafts proposals knowing any single Council holdout can extract concessions before adoption; increasingly pre-negotiates side-deals or waters down proposals in anticipation of blocking behavior, which both wastes institutional capacity and normalizes extraction as a cost of doing business. Cannot bypass unanimity on treaty-designated policy areas (CFSP sanctions, taxation, own resources, treaty change) without treaty reform, which itself requires unanimity.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_commission, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, eu_commission, agenda_setter).

% Candidate countries awaiting accession-chapter approval, or non-EU actors subject to (or exempted from) sanctions packages, have no seat in the Council and no mechanism to object to a single member state stalling their file for unrelated leverage. Their fate is a bargaining chip in a negotiation they cannot enter.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, third_country_partners_awaiting_accession_or_sanctions, excluded,
    powerless, biographical, trapped, regional).

% Legal and procedural staff who see the full pattern of blocking behavior across dossiers and years; can document the recurring extraction pattern in institutional memoranda but have no authority to alter the unanimity requirement, which is fixed in the treaties and changeable only by the same unanimity rule it critiques.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, council_secretariat_and_treaty_architects, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its cover story, unanimity coordinates 27 sovereign states around decisions that touch core sovereignty (foreign policy, taxation, treaty change) by requiring universal buy-in before binding action — preventing a majority from committing a dissenting state to obligations it never agreed to.
% TRANSFER_FUNCTION: Moves budgetary side-payments, policy carve-outs, unrelated concessions (visa liberalization, rebates, appointments), and delay costs from the coalition majority and the Commission to whichever member state holds out on a given dossier; the transfer recurs dossier by dossier, with the identity of the extracting state varying by issue.
% ABSENT_VOICES: Candidate states, sanctioned or exempted third countries, and EU citizens in the majority-preference states have no formal voice in the bilateral side-negotiation that resolves a blocking episode; the concession is negotiated between the Commission/Presidency and the holdout alone, then presented to the wider Council as a fait accompli.
% DISAPPEARANCE_RATIONALE: If unanimity were replaced by qualified majority voting on the currently unanimity-gated dossiers overnight, blocking states would lose the ability to extract dossier-specific concessions; sanctions renewals, accession decisions, and tax dossiers would move on majority-preference timelines; the side-payment and carve-out economy that currently absorbs significant Council negotiating capacity would largely disappear, and several long-blocked accession files would likely advance within a single Council cycle.
% FOUNDING_PROBLEM: Post-war European integration needed a decision rule that would not force any founding or acceding state into supranational obligations it had not consented to, given fears of majoritarian domination by larger states over core sovereignty matters (foreign policy, taxation, treaty change).
% FOUNDING_PROBLEM_CORROBORATION: Smaller member states and constitutional courts (notably in rulings on EU competence limits) continue to attest the sovereignty-protection problem is live. Independent political-economy analyses (European Council on Foreign Relations reporting, academic work on Council voting patterns) and Commission officials speaking on background attest that in practice the rule now functions primarily as leverage for whichever state holds the marginal vote on a given dossier, with sovereignty protection invoked post hoc to justify extraction that has little to do with the original consent concern.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high and rising (0.45 to 0.78 over the interval) because the pattern of dossier-specific holdouts has become more frequent and more institutionally anticipated over the period modeled — Commission staff increasingly pre-negotiate around expected blocking rather than treating it as exceptional. Theater ratio is moderate and rising (0.22 to 0.42): a meaningful share of Council process now consists of managing the anticipated blocking dynamic (informal consultations, pre-summit shuttle diplomacy) rather than substantive policy deliberation. Suppression is authored as structural, not merely reputational: the majority literally cannot act without the holdout's consent on treaty-designated dossiers, and treaty reform to change the rule itself requires unanimity, creating a closed loop.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking state's seat, each veto is an exercise of legitimate sovereign consent-withholding, indistinguishable in form from the sovereignty_guarantor_reading. From the coalition majority's seat, the same act is a toll paid to move policy the overwhelming majority already supports. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate which seat is 'right' — it authors the veto_trap reading as its own constraint with its own ε.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking_member_state is the structural beneficiary under this reading: it bears essentially no cost for withholding consent (its own preferred status quo persists during the block) while it can extract asymmetric concessions from parties who bear the cost of delay. The coalition_majority_states and the Commission are targets: their preferences are structurally subordinated to the holdout's regardless of numerical or economic weight, and their exit options (treaty reform, enhanced cooperation, intergovernmental workarounds) are themselves gated by unanimity or are slow and legally fraught. Third-country partners are excluded entirely from the negotiation that determines their fate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting non-consenting states from majoritarian coercion on core sovereignty matters — was real at the Treaty of Rome era and remains partially live for genuine constitutional-order questions (treaty change, new own-resources categories). But this reading isolates a distinct phenomenon: the founding problem has become detached from the extraction it now enables on dossiers (routine sanctions renewals, accession-chapter technical approvals) where sovereignty implication is thin and the primary function observed is leverage extraction. Classifying this as tangled_rope rather than pure snare preserves the fact that unanimity does still coordinate genuine consent on some dossiers even as, on others, it operates as pure extraction — the tangled_rope type requires both functions to coexist, which they do here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_protection_vs_extraction_boundary,
    'For any given blocked dossier, is the holdout state genuinely protecting a sovereignty interest implicated by the proposal, or is it using the unanimity rule opportunistically to extract concessions on an unrelated matter?',
    'Case-by-case analysis of whether the concessions ultimately extracted are substantively related to the blocked dossier''s content (supports sovereignty_guarantor_reading) or unrelated (supports veto_trap_reading) — e.g., conditioning sanctions renewal on unrelated cohesion-fund disbursement is a clean instance of the unrelated pattern.',
    'If most historical blocking episodes show unrelated concessions extracted, this reading''s high ε is well-supported; if most show substantively related sovereignty concerns, the sovereignty_guarantor_reading better describes the empirical pattern and this reading''s applicability narrows to a minority of dossiers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_protection_vs_extraction_boundary, empirical, 'Whether blocking episodes are substantively sovereignty-protective or opportunistically extractive.').

omega_variable(
    reading_coexistence_within_single_dossier,
    'Can a single blocking episode simultaneously instantiate the veto_trap_reading and the sovereignty_guarantor_reading — i.e., is the same act genuine sovereignty assertion AND extraction, rather than one or the other?',
    'This is a conceptual question about whether the three readings partition the space of blocking episodes or overlap. The ε-invariance principle requires treating them as distinct constraints regardless, but the mapping from real-world episodes to readings may not be exclusive.',
    'If episodes routinely instantiate multiple readings simultaneously, the decomposition into three separate constraint stories remains structurally correct (each story evaluates a distinct claim) but no single episode should be read as evidence exclusively for one story''s ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_within_single_dossier, conceptual, 'Whether the three kernel readings are mutually exclusive or overlapping in their application to real blocking episodes.').

omega_variable(
    treaty_reform_lock_in,
    'Does the fact that unanimity can only be reformed by unanimity constitute independent evidence that the rule has become self-protecting extraction infrastructure, or is this simply the standard (and defensible) design feature of any foundational constitutional rule?',
    'Comparative analysis against other constitutional systems'' amendment thresholds — if EU unanimity''s self-entrenchment is markedly harder to escape than comparable federal or confederal amendment rules, that supports the veto_trap reading; if comparable, it supports the sovereignty_guarantor reading.',
    'Affects how much weight the rising extractiveness trend (measured 0.45 to 0.78) should be attributed to the rule''s design versus to contingent political factors of the specific interval studied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_reform_lock_in, conceptual, 'Whether self-entrenchment of the unanimity rule is diagnostic of extraction lock-in or standard constitutional design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(eu_c_tr_t4, eu_council_unanimity__veto_trap_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(eu_c_tr_t8, eu_council_unanimity__veto_trap_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__veto_trap_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(eu_c_tr_t16, eu_council_unanimity__veto_trap_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__veto_trap_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(eu_c_be_t4, eu_council_unanimity__veto_trap_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(eu_c_be_t8, eu_council_unanimity__veto_trap_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__veto_trap_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(eu_c_be_t16, eu_council_unanimity__veto_trap_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__veto_trap_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(eu_c_su_t4, eu_council_unanimity__veto_trap_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(eu_c_su_t8, eu_council_unanimity__veto_trap_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__veto_trap_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(eu_c_su_t16, eu_council_unanimity__veto_trap_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(eu_c_su_t24, eu_council_unanimity__veto_trap_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.1).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_sanctions_renewal_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_accession_process).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the eu_council_unanimity kernel (veto_trap_reading, sovereignty_guarantor_reading, diplomatic_capital_reading). Each reading names a different beneficiary/victim structure and carries a different ε: sovereignty_guarantor_reading has no named victim and low ε (genuine coordination, consent protection); diplomatic_capital_reading has diffuse beneficiaries across all member states and moderate ε (consensus-building function, real but costly); this veto_trap_reading names a concentrated, dossier-varying beneficiary and concrete victims, with high and rising ε reflecting extraction. All three are the same treaty provision read three structurally distinct ways, per the BGS decomposition pattern — not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__veto_trap_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
