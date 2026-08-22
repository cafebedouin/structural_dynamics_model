% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Reading of Separation of Powers (Delegation via Intelligible Principle)
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   This story instantiates the functionalist reading of the
 *   separation-of-powers kernel: the constitutional text is read as
 *   tolerating overlapping legislative, executive, and judicial functions so
 *   long as Congress supplies an 'intelligible principle' to guide agency
 *   discretion. Under this reading the modern administrative state (EPA, SEC,
 *   FDA, NLRB, and similar bodies) is constitutionally sound, and the
 *   coordination problem it solves — governing a technically complex society
 *   through a legislature that cannot specify every regulatory detail — is
 *   treated as genuine and largely successfully addressed via deference
 *   doctrines. This is NOT a description of the formalist reading (which
 *   would treat the same delegations as unconstitutional transfers of
 *   legislative power) or the unitary-executive reading (which would treat
 *   independent-agency insulation from removal as a violation). Those are
 *   separate constraints with their own ε values, authored separately, linked
 *   here by network edges.
 *
 * KEY AGENTS:
 *   - administrative_agencies: primary beneficiary and de facto agenda-setter within delegated space (institutional/arbitrage)
 *   - congress: delegating beneficiary, retains oversight (institutional/arbitrage)
 *   - president_and_executive_office: partial beneficiary, partial payer of reduced direct control (institutional/constrained)
 *   - regulated_entities_facing_agency_discretion: primary payer, bears compliance and discretion costs (powerful/constrained)
 *   - litigants_challenging_agency_action: payer via near-unwinnable nondelegation litigation (moderate/constrained)
 *   - public_beneficiaries_of_regulation: diffuse beneficiary of regulatory protections (powerless/trapped)
 *   - formalist_judges_and_scholars: excluded dissenting voice (organized/mobile)
 *   - reviewing_courts: analytical observer/enforcer of the intelligible-principle standard (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.31).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.28).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Reading of Separation of Powers (Delegation via Intelligible Principle)").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '9692e140-2b16-4ef9-82a1-efec379d7db8').
narrative_ontology:cs_kernel_codification('9692e140-2b16-4ef9-82a1-efec379d7db8', fixed_text).
narrative_ontology:cs_authority_grounding('9692e140-2b16-4ef9-82a1-efec379d7db8', lineage).
narrative_ontology:cs_interpretation_layer_present('9692e140-2b16-4ef9-82a1-efec379d7db8').
narrative_ontology:cs_reading_relation('9692e140-2b16-4ef9-82a1-efec379d7db8', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9692e140-2b16-4ef9-82a1-efec379d7db8', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('9692e140-2b16-4ef9-82a1-efec379d7db8', foundational, delegation_permissible_with_intelligible_principle).
narrative_ontology:cs_axiom_status(delegation_permissible_with_intelligible_principle, holdable).
narrative_ontology:cs_axiom_grounding('9692e140-2b16-4ef9-82a1-efec379d7db8', delegation_permissible_with_intelligible_principle, conventional).
narrative_ontology:cs_axiom('9692e140-2b16-4ef9-82a1-efec379d7db8', foundational, overlapping_branch_function_constitutionally_tolerable).
narrative_ontology:cs_axiom_status(overlapping_branch_function_constitutionally_tolerable, holdable).
narrative_ontology:cs_axiom_grounding('9692e140-2b16-4ef9-82a1-efec379d7db8', overlapping_branch_function_constitutionally_tolerable, instrumental).
narrative_ontology:cs_reference_frame('9692e140-2b16-4ef9-82a1-efec379d7db8', new_deal_administrative_settlement).
narrative_ontology:cs_drift_state('9692e140-2b16-4ef9-82a1-efec379d7db8', post_major_questions_doctrine_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9692e140-2b16-4ef9-82a1-efec379d7db8', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulated_industry_compliance_departments).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, public_beneficiaries_of_regulation).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_entities_facing_agency_discretion).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, litigants_challenging_agency_action).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president_and_executive_office).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, president_and_executive_office).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, chevron_style_deference_legitimacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, administrative_state_constitutionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise rulemaking, adjudicative, and enforcement functions delegated by Congress under broad statutory mandates (e.g. 'protect public health,' 'ensure fair competition'). Their legitimacy rests on courts reading separation of powers as tolerating overlapping function so long as Congress supplies an 'intelligible principle.' They set the operative rules within their delegated space and are rarely displaced once established.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, administrative_agencies, agenda_setter).

% Delegates complex technical and ongoing regulatory judgment to agencies rather than legislating every detail, preserving its capacity to address novel problems without constant floor votes. Retains oversight, appropriations, and override power. The functionalist reading is what lets Congress delegate broadly without each statute being struck down as an unconstitutional transfer of legislative power.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congress, agenda_setter).

% Directs much of the administrative apparatus but shares control with independent agencies insulated by for-cause removal protections and with congressional oversight committees. Benefits from a functional executive branch that can act through expert agencies, but pays a cost in reduced direct control compared to a unitary-executive arrangement.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president_and_executive_office, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, president_and_executive_office, payer).

% Must comply with agency rules promulgated under broad delegated authority, often with limited advance notice of how discretion will be exercised. Can challenge rules in court but bear the burden of overcoming deference doctrines that presume the agency's interpretation is permissible. Exit means relocating operations or accepting compliance costs, not escaping the regulatory framework itself.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_entities_facing_agency_discretion, payer,
    powerful, biographical, constrained, national).

% Bring nondelegation or ultra vires challenges to agency action and face a judiciary that, under the functionalist reading, upholds delegations so long as an intelligible principle exists — a low bar met in nearly every case since 1935. Their litigation costs are real; their win rate on pure delegation theory is near zero.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, litigants_challenging_agency_action, payer,
    moderate, biographical, constrained, national).

% Receive the substantive protections (clean air, safe food, financial stability rules) that flexible delegation makes administratively feasible. Have no direct role in agency rulemaking beyond notice-and-comment participation, but depend structurally on the functionalist framework continuing to permit responsive regulation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, public_beneficiaries_of_regulation, beneficiary,
    powerless, generational, trapped, national).

% Argue from outside the functionalist consensus that intelligible-principle review has become a rubber stamp and that the framework has hollowed out Article I's vesting of 'all legislative powers' in Congress alone. Their objections surface in dissents and academic literature and periodically gain traction (e.g. major questions doctrine revival) but do not currently command a majority reading of the kernel.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_judges_and_scholars, excluded,
    organized, civilizational, mobile, national).

% Apply the intelligible-principle test and deference doctrines to adjudicate disputes between agencies and regulated parties. Their continued application of a permissive standard is what operationalizes the functionalist reading; a doctrinal shift by the courts would itself constitute a kernel reading change, not merely enforcement drift.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, reviewing_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__functionalist_reading, diffuse).
narrative_ontology:fixing_cost_class(separation_of_powers_text__functionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a legislature that cannot anticipate or specify every technical contingency to establish policy goals and delegate implementation to expert agencies, enabling government to respond to complex, evolving problems (environmental science, financial markets, public health) without either legislative paralysis or constitutionally-barred delegation.
% TRANSFER_FUNCTION: Moves practical policymaking authority from elected legislators to appointed agency officials, and moves the burden of demonstrating unconstitutional delegation from the government (which must show some principle) onto challengers (who must show the principle is absent) — a burden nearly impossible to meet under current doctrine.
% ABSENT_VOICES: Formalist judges and scholars who read Article I's vesting clause as prohibiting any transfer of legislative power are not part of the operative doctrinal consensus; their view surfaces in dissents (e.g. Gundy v. United States) and in academic commentary but has not commanded a majority since the 1930s, though it has gained renewed traction via the major questions doctrine.
% DISAPPEARANCE_RATIONALE: If the functionalist reading were displaced by a strict formalist or unitary-executive reading, the entire modern administrative state — environmental, financial, labor, and health regulation built on broad delegations — would face wholesale constitutional challenge; independent agencies could be restructured or abolished, and Congress would need to legislate at a level of specificity it has not exercised in nearly a century.
% FOUNDING_PROBLEM: The New Deal-era problem of governing a complex industrial economy through a legislature structurally incapable of specifying technical regulatory detail in statutory text, combined with the need to avoid the pre-1937 Court's willingness to strike down broad delegations as unconstitutional.
% FOUNDING_PROBLEM_CORROBORATION: Administrative law scholars across the ideological spectrum (including formalist critics who oppose the doctrine) agree the underlying governance problem — legislative incapacity to specify technical regulatory rules — remains live; the dispute is over whether the functionalist solution is constitutionally sound, not whether the problem it addresses persists. Empirical accounts of regulatory complexity in agencies like EPA and SEC, produced by administrative-state critics and defenders alike, corroborate the problem's continuation independent of either side's preferred remedy.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-moderate (0.31 at 2025) because, from the functionalist reading's own lights, delegation to agencies is a genuine coordination solution to a real governance capacity problem, not primarily a rent-extraction device — this is markedly lower than the ε a formalist or unitary-executive reading would author for the same underlying arrangement. Suppression is comparatively low (0.28) because the doctrine formally preserves judicial review, congressional oversight, and notice-and-comment participation as live checks, even though the intelligible-principle bar is easy to clear in practice. Theater ratio is modest (0.22) reflecting that some judicial review of delegation has become largely formal affirmance rather than substantive scrutiny, a mild but real drift documented in the rising trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute agencies and Congress as near-Rope from their own structural position (arbitrage exit, institutional power, genuine delegated coordination function) while regulated entities facing agency discretion compute closer to a tangled-rope or snare experience (constrained exit, bearing costs through a structure that formally exists to coordinate but functionally extracts compliance burden without proportionate voice). This divergence is exactly the seat-level split the framework is built to surface, and it is a fact about this reading, not a defect in it.
 *
 * DIRECTIONALITY LOGIC:
 *   Agencies and Congress sit near the beneficiary end: agencies gain operative rulemaking power without needing constitutional amendment; Congress gains delegative flexibility while retaining a formal disclaiming distance from unpopular technical rules. Regulated entities and unsuccessful litigants sit toward the target end: they absorb compliance costs and face a doctrine engineered to uphold the delegations they challenge. The public beneficiaries of regulation are true low-d beneficiaries structurally, but their trapped exit options and powerless status mean they cannot act on that benefit to negotiate terms — they receive the good but do not co-author the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative incapacity to regulate a complex economy through exhaustive statutory specification) remains live by the corroboration of both defenders and formalist critics of the doctrine — this blocks a mandatrophy verdict under this reading. A mandatrophy reading would require showing the problem is dead while the delegative machinery persists; instead all sides agree agencies still perform functions Congress genuinely cannot perform directly, which is why this story authors a rope-type claim rather than a piton or snare, despite the real and rising extraction and suppression trend lines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intelligible_principle_meaningfulness,
    'Does the intelligible-principle test provide a real constraint on congressional delegation, or has it become a rubber-stamp standard that no delegation has failed since 1935 (Panama Refining, Schechter Poultry)?',
    'Empirical survey of nondelegation challenges since 1935 and their outcomes; analysis of whether any modern statute has been struck down on pure delegation grounds versus surviving under increasingly broad readings of ''intelligible principle.''',
    'If the test is genuinely toothless, the functionalist reading''s claim that meaningful judicial constraint persists is weaker than authored, and effective extraction (via unchecked discretion) may be higher than the authored ε suggests — this would strengthen a mandatrophy or drift-toward-piton argument for the doctrine''s judicial-review component specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_meaningfulness, empirical, 'Whether the intelligible-principle standard is a real or merely formal constraint.').

omega_variable(
    kernel_reading_selection_uncertainty,
    'Is the functionalist reading the correct account of what Article I''s vesting clauses mean, or is it a doctrinal accommodation that departed from original constitutional meaning under New Deal-era political pressure?',
    'Historical and textual analysis is fundamentally contested between formalist and functionalist interpretive methodologies; no single empirical test resolves which reading is ''correct'' — this is a live interpretive dispute among the kernel''s readings themselves, not a fact awaiting discovery.',
    'If the formalist reading is treated as authoritative, the entire modern administrative state (and this constraint''s low-ε coordination framing) would be reclassified as extractive constitutional evasion rather than genuine coordination; this is precisely why the readings are authored as separate constraints rather than resolved into one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_uncertainty, conceptual, 'Which kernel reading (functionalist, formalist, unitary-executive) is the structurally correct account of separation of powers — an irreducibly contested interpretive question, not an empirical one.').

omega_variable(
    major_questions_doctrine_erosion,
    'Does the recent judicial revival of the major questions doctrine (limiting agency authority on matters of ''vast economic and political significance'' absent clear congressional authorization) represent a partial retreat from the functionalist reading toward formalism, or a compatible refinement within it?',
    'Track post-2022 major-questions-doctrine case outcomes and whether courts treat it as a clear-statement rule operating within functionalism or as a signal of doctrinal reversion toward formalist limits on delegation.',
    'If the doctrine represents genuine formalist encroachment, this reading''s ε and suppression trajectories may need revision upward for the current period, and the functionalist reading''s dominance may itself be a temporally bounded state rather than a stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_doctrine_erosion, empirical, 'Whether recent doctrine signals drift from the functionalist reading toward the formalist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__functionalist_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(sepa_tr_t1953, separation_of_powers_text__functionalist_reading, theater_ratio, 1953, 0.12).
narrative_ontology:measurement(sepa_tr_t1971, separation_of_powers_text__functionalist_reading, theater_ratio, 1971, 0.14).
narrative_ontology:measurement(sepa_tr_t1989, separation_of_powers_text__functionalist_reading, theater_ratio, 1989, 0.17).
narrative_ontology:measurement(sepa_tr_t2007, separation_of_powers_text__functionalist_reading, theater_ratio, 2007, 0.19).
narrative_ontology:measurement(sepa_tr_t2025, separation_of_powers_text__functionalist_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__functionalist_reading, base_extractiveness, 1935, 0.18).
narrative_ontology:measurement(sepa_be_t1953, separation_of_powers_text__functionalist_reading, base_extractiveness, 1953, 0.2).
narrative_ontology:measurement(sepa_be_t1971, separation_of_powers_text__functionalist_reading, base_extractiveness, 1971, 0.23).
narrative_ontology:measurement(sepa_be_t1989, separation_of_powers_text__functionalist_reading, base_extractiveness, 1989, 0.26).
narrative_ontology:measurement(sepa_be_t2007, separation_of_powers_text__functionalist_reading, base_extractiveness, 2007, 0.28).
narrative_ontology:measurement(sepa_be_t2025, separation_of_powers_text__functionalist_reading, base_extractiveness, 2025, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__functionalist_reading, suppression_requirement, 1935, 0.15).
narrative_ontology:measurement(sepa_su_t1953, separation_of_powers_text__functionalist_reading, suppression_requirement, 1953, 0.17).
narrative_ontology:measurement(sepa_su_t1971, separation_of_powers_text__functionalist_reading, suppression_requirement, 1971, 0.19).
narrative_ontology:measurement(sepa_su_t1989, separation_of_powers_text__functionalist_reading, suppression_requirement, 1989, 0.22).
narrative_ontology:measurement(sepa_su_t2007, separation_of_powers_text__functionalist_reading, suppression_requirement, 2007, 0.25).
narrative_ontology:measurement(sepa_su_t2025, separation_of_powers_text__functionalist_reading, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the separation_of_powers_text kernel. The formalist_reading authors a higher ε for the same delegation arrangements (treating them as unconstitutional legislative transfer accomplished through doctrinal workaround). The unitary_executive_reading authors a different victim structure (independent agencies as the extractive beneficiary displacing presidential control, rather than agencies as legitimate coordination). All three share the same constitutional text as their kernel but diverge in claimed_type, ε, and beneficiary/victim sets because each reading evaluates a structurally different claim about what the text permits. Network edges here mark structural influence (this reading's dominance affects the resource availability and legitimacy conditions the sibling readings must contend with in litigation and legislative practice) — not equivalence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
