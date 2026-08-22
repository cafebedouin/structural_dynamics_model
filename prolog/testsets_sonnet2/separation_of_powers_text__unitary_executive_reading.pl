% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading of Article II Vesting Clause
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   The unitary executive reading holds that Article II's vesting clause
 *   ('The executive Power shall be vested in a President') commits ALL
 *   executive power, undivided, to the President, making any statutory
 *   insulation of executive officers from at-will removal an unconstitutional
 *   fragmentation of that power. Under this reading, independent multi-member
 *   agencies like the FTC, NLRB, and Federal Reserve — designed by Congress
 *   with for-cause removal protections specifically to insulate technical and
 *   adjudicatory functions from presidential political cycles — are standing
 *   constitutional violations rather than legitimate structural choices. This
 *   is one reading of the separation_of_powers_text kernel among three: the
 *   formalist reading holds boundaries are strict but locates the violation
 *   in congressional delegation of legislative power rather than in agency
 *   independence per se; the functionalist reading treats overlapping
 *   authority as permissible so long as an intelligible principle constrains
 *   it. This story concerns only the unitary_executive_reading; the ε value,
 *   beneficiary/victim structure, and classification here are NOT averaged
 *   with or hedged against the sibling readings, which are separate
 *   constraint files.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.61).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.52).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading of Article II Vesting Clause").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '89d4635f-7020-4272-9ab8-2ffb7cbb9e17').
narrative_ontology:cs_kernel_codification('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', fixed_text).
narrative_ontology:cs_authority_grounding('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', lineage).
narrative_ontology:cs_interpretation_layer_present('89d4635f-7020-4272-9ab8-2ffb7cbb9e17').
narrative_ontology:cs_reading_relation('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', foundational, executive_power_is_indivisible).
narrative_ontology:cs_axiom_status(executive_power_is_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', executive_power_is_indivisible, deontological).
narrative_ontology:cs_axiom('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', foundational, removal_power_is_inherent_and_absolute).
narrative_ontology:cs_axiom_status(removal_power_is_inherent_and_absolute, holdable).
narrative_ontology:cs_axiom_grounding('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', removal_power_is_inherent_and_absolute, conventional).
narrative_ontology:cs_axiom('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', secondary, for_cause_protection_is_constitutional_fragmentation).
narrative_ontology:cs_axiom_status(for_cause_protection_is_constitutional_fragmentation, holdable).
narrative_ontology:cs_axiom_grounding('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', for_cause_protection_is_constitutional_fragmentation, conventional).
narrative_ontology:cs_reference_frame('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', unitary_vesting_clause_original_meaning).
narrative_ontology:cs_drift_state('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', post_administrative_state_expansion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('89d4635f-7020-4272-9ab8-2ffb7cbb9e17', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, sitting_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_office_of_the_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, at_will_removal_theorists).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_trade_commission).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, national_labor_relations_board).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_reserve_board).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, civil_service_tenured_officials).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, unitary_executive_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, presidential_accountability_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, holds unified control of all executive power including at-will removal authority over any officer performing executive functions. Directs litigation strategy through the Solicitor General to press removal-power claims into court, appoints agency heads with the expectation of at-will control, and gains direct political accountability leverage over regulatory bodies that previously operated at arm's length.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, sitting_president, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, sitting_president, agenda_setter).

% Gains centralized policy coordination authority as independent agencies are folded into a hierarchical chain of command; can compel rulemaking priorities and enforcement postures across agencies previously insulated by for-cause removal protections.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_office_of_the_president, beneficiary,
    institutional, generational, arbitrage, national).

% Legal scholars and originalist litigators whose doctrinal project is vindicated each time a court adopts the unitary reading; their scholarship and litigation strategy (Federalist Society-adjacent networks, Article II maximalist briefs) gains authoritative force and citation currency.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, at_will_removal_theorists, beneficiary,
    organized, civilizational, analytical, national).

% Structured by Congress for 110 years as a multi-member body with staggered terms and for-cause removal protection specifically to insulate antitrust and consumer-protection enforcement from single-executive political pressure. Under this reading, that structure is constitutionally infirm; commissioners can be removed at will, collapsing the independence the statute was built to secure. The agency cannot exit its statutory design or relocate its function.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_trade_commission, payer,
    institutional, generational, trapped, national).

% Adjudicates labor disputes between employers and unions and is designed to be insulated from the political preferences of whichever party controls the presidency at a given moment. Under this reading its members serve at the President's pleasure, exposing labor-law adjudication to shifts in executive political alignment every four years.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, national_labor_relations_board, payer,
    institutional, generational, trapped, national).

% Monetary policy independence from short-term electoral pressure is its central design premise, built on the theory that a President seeking reelection has incentives to inflate the currency. Under an unabridged unitary reading, Fed governors would be removable at will, threatening the credibility of long-horizon monetary commitments even where courts have signaled reluctance to extend the doctrine this far.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_reserve_board, payer,
    institutional, civilizational, trapped, national).

% Career officials in executive-function roles below cabinet level who rely on statutory removal protections to perform their duties (rulemaking, enforcement, adjudication) without fear of at-will dismissal for political reasons. Their professional security is directly threatened if the doctrine extends past named agency heads to broader civil-service protections; exit means leaving federal service.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, civil_service_tenured_officials, payer,
    moderate, biographical, constrained, national).

% Courts historically served as the arbiter balancing congressional structural choices against executive control claims (Humphrey's Executor, Morrison v. Olson). This reading recasts that balancing function as judicial error to be corrected rather than a legitimate interpretive tradition, sidelining decades of doctrine that treated multi-member independent commissions as constitutionally permissible.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary_administrative_law_doctrine, excluded,
    institutional, civilizational, analytical, national).

% Historically exercised its Article I structuring power to create independent agencies with removal protections as a coordination solution to the problem of politically insulated technical and adjudicatory functions. This reading would nullify that structuring choice retroactively across dozens of statutes without requiring new legislation, would object strongly if consulted, but its institutional voice is not treated as authoritative under this reading's textual theory.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, sitting_president).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The vesting clause reading solves a genuine coordination problem: it identifies a single accountable locus for executive action, so that when executive power is exercised, voters and courts can trace responsibility to one elected official rather than diffuse, low-visibility administrators.
% TRANSFER_FUNCTION: Moves de facto policy control and removal leverage over regulatory, labor, and monetary functions from multi-member, tenure-protected boards to the President's office; moves interpretive authority over decades of structural design choices from Congress and the judiciary's precedent to the unitary theory's textual claim.
% ABSENT_VOICES: Congress, whose Article I structuring choices this reading nullifies without new legislation, and the line of judicial precedent (Humphrey's Executor, Morrison v. Olson) treating independent agencies as constitutionally permissible, are both treated as errors to correct rather than parties with a claim to be heard on their own structural design choices.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned, independent agencies would retain their for-cause removal protections, agency heads could not be dismissed for policy disagreement alone, and monetary and labor-adjudication functions would continue operating at arm's length from presidential political cycles — the current partial adoption in cases like Seila Law and Collins v. Yellen would need to be walked back, materially changing agency governance.
% FOUNDING_PROBLEM: The vesting clause and its unitary reading were developed to solve a felt problem of unaccountable administrative power: agencies exercising coercive regulatory authority while insulated from any electorally accountable officer, creating a fourth branch of government answerable to no one at the ballot box.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and executive-branch litigators attest the accountability problem is live and worsening as agency rulemaking has expanded. Independent agency defenders, administrative law scholars outside the unitary-executive project, and much of the historical judiciary (pre-Seila Law precedent) attest that for-cause removal protections were themselves the coordination solution to a different, equally live problem — capture of technical and adjudicatory functions by presidential political cycles — and that the unitary reading trades one accountability problem for a different insulation problem rather than solving anything cleanly.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 and rising over the interval (0.20 in 1935 to 0.61 in 2024) because the doctrine's practical force has grown from a marginal academic position (Myers v. United States dicta, 1926) into a live and increasingly successful litigation strategy (Seila Law v. CFPB 2020, Collins v. Yellen 2021), each success stripping removal protection from another category of independent agency and concentrating executive control. Suppression is moderate (0.52) and also rising, reflecting the doctrine's dependence on active litigation and judicial reinterpretation to displace nearly a century of settled agency design (Humphrey's Executor, 1935) rather than on any change in the underlying text. Theater ratio is comparatively low (0.28) because the doctrinal machinery does perform real coordination work — presidential accountability is a genuine value — even as it also displaces genuine institutional insulation functions; this is exactly the tangled-rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting President and the Executive Office are direct structural beneficiaries: at-will removal authority is transferred to them from agencies that previously held it independently, and this reading validates that transfer as constitutionally compelled rather than merely policy-preferred. At-will removal theorists benefit reputationally and professionally each time courts adopt their framework. The named independent agencies (FTC, NLRB, Fed) are the direct structural targets — their statutory design is precisely what the doctrine invalidates, and they are institutionally trapped: an agency cannot restructure its own removal protections or exit the constitutional argument being made about it. Civil service officials sit at moderate power with only constrained exit (leaving federal service), bearing diffuse professional-security costs if the doctrine's logic extends beyond named agency heads.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being mislabeled as pure extraction (snare) or pure coordination (rope). The coordination function is real: identifying a single accountable executive locus is a genuine solution to a genuine diffusion-of-responsibility problem in a sprawling administrative state. But the same structure, applied consistently, extracts institutional independence from bodies Congress specifically designed to be insulated from exactly the kind of political-cycle pressure the doctrine reintroduces. Both the coordination benefit and the extraction cost run through the identical mechanism — the vesting-clause reading itself — which is the tangled_rope signature rather than either pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_unitary,
    'Is the unitary_executive_reading the correct interpretation of the vesting clause kernel, or do the formalist_reading and functionalist_reading better capture the founders'' structural intent regarding executive power and administrative independence?',
    'This is not empirically resolvable — it depends on contested methods of constitutional interpretation (original public meaning vs. structural inference vs. living-constitutionalist functionalism) that this framework does not adjudicate. Each reading is authored as its own constraint with its own stable epsilon; no resolution mechanism converts one reading into another.',
    'Adopting the formalist_reading instead would locate the constitutional violation in congressional delegation of legislative power to agencies rather than in agency-head removal protections, producing a different victim set (Congress and delegating statutes, not independent agencies) and a different beneficiary structure. Adopting the functionalist_reading would treat current independent-agency design as presumptively constitutional, making this constraint''s claimed extraction largely disappear under that reading''s own lights.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_unitary, conceptual, 'Which of the three kernel readings is structurally correct is a live jurisprudential dispute, not a fact this story adjudicates.').

omega_variable(
    removal_power_scope_boundary,
    'Does the unitary theory, if adopted, extend only to principal officers heading multi-member independent agencies, or does its logic necessarily reach all executive-function civil servants with statutory tenure protection?',
    'Track how courts apply Seila Law and Collins v. Yellen to subsequent cases involving inferior officers and civil-service protections (e.g., Merit Systems Protection Board cases); a stopping point that holds at principal officers only would cabin the doctrine, while extension to civil-service tenure generally would validate the broader reading.',
    'If the doctrine stops at agency heads, the victim set stays limited to named independent agencies. If it extends further, civil_service_tenured_officials becomes a much larger and more consequential victim class, substantially raising effective extraction at scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(removal_power_scope_boundary, empirical, 'Whether the doctrine''s logical scope is bounded to agency heads or extends to the broader civil service is not yet settled by case law.').

omega_variable(
    accountability_vs_insulation_tradeoff,
    'Does concentrating removal power in the President actually produce better executive accountability outcomes than the for-cause protections it displaces, or does it merely substitute one form of unaccountability (agency capture by technocrats) for another (agency capture by presidential political cycles)?',
    'Comparative institutional analysis of agency performance and independence outcomes across jurisdictions and time periods with differing removal-protection regimes; also longitudinal study of Fed and FTC decision quality pre- and post-erosion of removal protections.',
    'If accountability outcomes genuinely improve, the coordination-function claim strengthens relative to the extraction claim. If outcomes merely shift the capture problem rather than resolving it, the tangled_rope''s extraction component is the dominant real-world effect and the coordination story functions primarily as legitimating cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_vs_insulation_tradeoff, empirical, 'Whether unitary control produces a net accountability gain or merely relocates the capture problem is an open empirical question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1935, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(sepa_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1935, 0.2).
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(sepa_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1935, 0.15).
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.27).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement(sepa_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, functionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the separation_of_powers_text kernel, each authored as a separate constraint story per the ε-invariance principle. formalist_reading targets congressional delegation of legislative authority to agencies (a different victim set: Congress and delegating statutes). functionalist_reading treats overlapping authority under an intelligible-principle standard as presumptively legitimate, producing near-zero extraction under its own lights for the same agency structures this reading treats as substantially extractive. All three share the same textual kernel (the Article II vesting clause and Article I structuring power) but instantiate structurally distinct constraints with distinct ε values, beneficiary/victim sets, and classifications. Link all three via affects_constraints; do not average or reconcile their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
