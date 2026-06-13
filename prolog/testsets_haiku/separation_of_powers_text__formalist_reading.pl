% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Strict Separation of Powers (Formalist Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The formalist reading of separation of powers asserts that Article I, II,
 *   and III of the Constitution establish impermeable institutional
 *   boundaries: Congress holds all legislative power; the President holds all
 *   executive power; the judiciary holds all adjudicative power. On this
 *   reading, any delegation of rulemaking authority from Congress to
 *   administrative agencies violates the Constitution's structural command.
 *   This constraint describes the doctrinal and institutional position that
 *   asserts this boundary as a discoverable feature of constitutional text —
 *   not as an interpretive choice among alternatives. The claimed type is
 *   mountain because the formalist position frames the separation as
 *   structurally inevitable from the text, not as a constructed institutional
 *   preference. The metrics, however, reflect substantial extractiveness and
 *   suppression — the suppression required to prevent alternatives (delegated
 *   regulatory authority, executive rulemaking, agency independence) and the
 *   extractiveness for those who benefit from delegations being
 *   unconstitutional (Congress and judiciary, who retain power rather than
 *   delegating it). The gap between the mountain claim and the high
 *   extractiveness/suppression metrics is the FSM candidate: is this really a
 *   natural boundary, or is it a constructed doctrine that benefits Congress
 *   and the judiciary?
 *
 * KEY AGENTS:
 *   - Congress: the legislative branch, structured as the holder of all legislative authority under formalism; gains power relative to functionalist readings where some legislative authority can be delegated.
 *   - Administrative agencies: created by Congress through statutes, they operate under formalism as performing only executive functions, not legislative ones; victim set because formalism forbids them to exercise delegated rulemaking authority.
 *   - Executive branch (non-delegated): the President, insofar as executive power can be exercised without delegations; unclear whether the President gains or loses under formalism relative to functionalism.
 *   - Judiciary: the courts, particularly the Supreme Court, that adjudicate constitutional boundaries; gain power as the ultimate arbiter of what counts as legislative vs. executive action.
 *   - Regulated parties (businesses, individuals): subject to agency rulemaking; victim set if agency rules are struck down as unconstitutional delegations, because the rules' legal status becomes uncertain.
 *   - Functionalist legal tradition: an interpretive position (not a seat) that reads separation of powers flexibly; suppressed by formalism as an alternative reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.82).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.88).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.77).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, mountain).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Strict Separation of Powers (Formalist Reading)").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional/political").

domain_priors:emerges_naturally(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '72db56be-27bf-49ca-8cd2-3f11154b65cb').
narrative_ontology:cs_kernel_codification('72db56be-27bf-49ca-8cd2-3f11154b65cb', fixed_text).
narrative_ontology:cs_authority_grounding('72db56be-27bf-49ca-8cd2-3f11154b65cb', lineage).
narrative_ontology:cs_interpretation_layer_present('72db56be-27bf-49ca-8cd2-3f11154b65cb').
narrative_ontology:cs_reading_relation('72db56be-27bf-49ca-8cd2-3f11154b65cb', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('72db56be-27bf-49ca-8cd2-3f11154b65cb', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('72db56be-27bf-49ca-8cd2-3f11154b65cb', foundational, legislative_power_nondelegable).
narrative_ontology:cs_axiom_status(legislative_power_nondelegable, holdable).
narrative_ontology:cs_axiom_grounding('72db56be-27bf-49ca-8cd2-3f11154b65cb', legislative_power_nondelegable, deontological).
narrative_ontology:cs_axiom('72db56be-27bf-49ca-8cd2-3f11154b65cb', foundational, institutional_boundaries_impermeable).
narrative_ontology:cs_axiom_status(institutional_boundaries_impermeable, holdable).
narrative_ontology:cs_axiom_grounding('72db56be-27bf-49ca-8cd2-3f11154b65cb', institutional_boundaries_impermeable, conventional).
narrative_ontology:cs_reference_frame('72db56be-27bf-49ca-8cd2-3f11154b65cb', strict_separation_boundary).
narrative_ontology:cs_drift_state('72db56be-27bf-49ca-8cd2-3f11154b65cb', contemporary_administrative_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('72db56be-27bf-49ca-8cd2-3f11154b65cb', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, judicial_authority).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_delegated_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, executive_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, judiciary).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_branch).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulated_parties).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, constitutional_structure_immutability).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, legislative_exclusivity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the formalist reading, Congress is the exclusive holder of legislative power. It sets statutes and cannot delegate legislative authority to agencies. It therefore retains the power to legislate details of regulatory policy, though practical pressures push Congress to authorize agencies to fill in details. Formalism allows Congress to claim constitutional fidelity while the judiciary enforces the boundary against delegation challenges, protecting Congress's power from erosion.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Created by Congress through statutes, agencies are structured as executive bodies without legislative authority under formalism. They can only execute and interpret Congress's statutes; any rulemaking is treated as implementation, not legislation. The formalist constraint forbids them from exercising delegated legislative authority, even though Congress authorizes them by statute to do so. They must operate under constant threat that the courts will strike down their rules as unconstitutional delegations.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    organized, generational, constrained, national).

% The President and executive departments navigate formalism as a constraint on their capacity to govern through delegated rulemaking. They delegate authority to agencies expecting those agencies to exercise it; formalism treats such delegation as unconstitutional. The President gains some power by controlling executive agencies directly (unitary executive pressure), but loses regulatory capacity by being forbidden to delegate.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, executive_branch, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, executive_branch, beneficiary).

% The courts, especially the Supreme Court, are the ultimate arbiters of constitutional boundaries under formalism. They retain the power to police the delegation doctrine and strike down agency rules deemed unconstitutional. This role gives the judiciary structural advantage in refusing delegations, because delegation challenges are adjudicated by courts enforcing the formalist boundary.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, judiciary, beneficiary,
    institutional, generational, analytical, national).

% Businesses, individuals, and organizations subject to agency regulation face uncertainty under formalism: the rules they comply with are potentially unconstitutional delegations. Formalism creates legal instability and regulatory risk, because agency rules can be challenged in court and struck down, leaving the regulated parties without clear authority for the regulations that governed them.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_parties, payer,
    moderate, biographical, mobile, national).

% An interpretive position and scholarly tradition that reads separation of powers as permitting delegated authority under intelligible principles. Functionalism is excluded from official doctrine by formalism; it remains a live scholarly and judicial position but is suppressed as controlling authority by the formalist constraint.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_legal_tradition, excluded,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(separation_of_powers_text__formalist_reading, functionalist_legal_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, congress).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, strict separation prevented Executive power from concentrating in the hands of a single person. In the modern administrative state, formalism claimed to prevent Congressional abdication by forbidding delegation. The contemporary coordination problem (if any) is: how can Congress legislate comprehensively on complex regulatory matters (finance, labor, environment) without delegating to expert bodies? Formalism offers one answer: Congress must legislate in detail. Functionalism offers another: Congress can delegate with an intelligible principle and the courts defer to agency interpretation.
% TRANSFER_FUNCTION: Power flows from administrative agencies and the executive to Congress and the judiciary. Formalism forbids agencies from exercising legislative authority; only Congress can legislate. This transfers rulemaking power back to Congress (requiring it to legislate in detail) and to the judiciary (which must police the delegation boundary). Functionalism would permit agencies to retain rulemaking authority. The transfer under formalism also moves regulatory instability to the regulated parties: their reliance on agency rules is precarious because the rules can be struck down as unconstitutional.
% ABSENT_VOICES: Functionalist scholars and executive branch advocates for regulatory capacity are excluded from the formalist constraint's official endorsement. They remain present in academic legal writing and in executive branch arguments but are suppressed by the formalist doctrine enforced by courts. Independent agency boards and commissions (FTC, SEC, NLRB) are also excluded in the sense that they would argue for their own independent authority but are structured under formalism as merely executive bodies without legislative power.
% DISAPPEARANCE_RATIONALE: If the formalist constraint vanished overnight, Congress could openly delegate legislative authority to agencies without constitutional obstacle. Regulatory agencies would exercise rulemaking power with explicit delegation and no fear of constitutional invalidation. The regulatory state would expand in scope and speed. Congress would shed responsibility for detailed legislation and shift it to expert agencies. The judiciary would lose its role as constitutional policeman over delegation. This is a live, contested change to constitutional arrangement that numerous seats want to prevent or enable.
% FOUNDING_PROBLEM: The Founders sought to prevent concentrated executive power and tyranny by separating legislative, executive, and adjudicative functions. Madison's concern in Federalist 47-48 was that the same hands accumulating all powers was the definition of tyranny. In the 1930s-1940s, formalism resurfaced as a constraint on New Deal agencies, treating their rulemaking as unconstitutional delegation.
% FOUNDING_PROBLEM_CORROBORATION: The formalist legal tradition attests the founding problem is live: the regulatory state represents executive-legislative cooperation that threatens to erode separation by letting agencies legislate. Functionalist scholars, the Executive, and many regulated parties attest the founding problem is substantially addressed by institutional checks (judicial review, Congressional oversight, presidential control). Legislative history from the Administrative Procedure Act (1946) shows Congress deliberately authorizing delegation with guidance, suggesting Congress solved the problem through statute rather than constitutional boundary. The institutionalized administrative state has persisted for 80+ years without collapse into tyranny, suggesting the founding danger is not live or is contained by mechanisms other than formalism.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(separation_of_powers_text__formalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(separation_of_powers_text__formalist_reading),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint systematically transfers power from agencies and the executive to Congress and the judiciary, and this transfer is not justified by a coordination problem (the pre-delegation system worked; the constraint blocks a functioning alternative). Suppression is even higher (0.88 at interval end) because the constraint's persistence depends on actively preventing the functionalist reading from gaining judicial adoption — the suppression is the enforcement machinery that keeps formalism in place against the functionalist challenge. Theater is moderate (0.41): some of the formalism is genuinely about constitutional interpretation (the performance of reading the text), but an increasing share is about defending institutional territory (protecting Congress and judiciary's power stakes) rather than about discovering boundaries from the text. The measurement series shows extraction and suppression intensifying over the interval (1960s-present): as delegated authority grew and functionalism gained scholarly and executive support, the suppression required to maintain formalism increased, suggesting the constraint is not a natural law but an enforced institutional choice. The theater ratio plateaus at 0.41 (does not reach piton levels of 0.5+) because formalism retains genuine constitutional-interpretation components; it is not pure performance, but the performance component is rising.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence is fundamental. Congress and the judiciary experience formalism as discovering constitutional boundaries that protect their proper role and prevent Executive overreach. Administrative agencies, the Executive, and functionalist scholars experience the same doctrine as an institutional cage enforcing a distributive choice that benefits the political branches (Congress) and the adjudicatory branch (judiciary) while suppressing the administrative state's capacity to coordinate regulatory policy. The engine should compute formalism as either a mountain (if the natural-law claim holds) or a tangled_rope (if the constraint is genuinely a coordination problem between Congress and the agencies, with Congress choosing to refuse delegation and the judiciary enforcing that refusal to extract power). From Congress's seat, it should register as coordination (Congress sets rules, agencies execute, boundary is constitutionally mandated). From an agency's seat, it should register as extraction (agencies are forbidden to perform functions they are otherwise authorized to perform by statute, and this prohibition benefits Congress).
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and the judiciary are beneficiaries (they retain power under formalism that they would lose under functionalism). Administrative agencies, the Executive's delegated authority, and functionalist legal traditions are victims (they would gain power or legitimacy under functionalism but are suppressed by formalism). Regulated parties are indirect victims: they depend on regulatory stability, and the unconstitutionality of delegations threatens rules they rely on. The directionality for Congress and the judiciary is d near 0.0 (full beneficiary — power flows to them). The directionality for agencies and executive delegated authority is d near 1.0 (full target — power and legitimacy flow away from them). This is why the seat divergence is structural: from Congress's seat, formalism is the Constitution's natural command; from an agency's seat, formalism is an institutional constraint that the judiciary enforces to preserve congressional and judicial power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was to prevent executive power from consolidating into absolutism (Madison, Federalist 47-48: "accumulated powers of the same hands... may justly be pronounced the very definition of tyranny"). In the late 18th century, this was a live danger because the Executive was a single person with direct commanding authority. By the late 20th century, the danger had mutated: the threat is not presidential absolutism but congressional abdication — Congress delegates its own authority to avoid hard votes and accountability. Formalism's response is to deny the delegation, treating it as unconstitutional. But this response forbids a coordination mechanism (Congress+agencies solving regulatory problems at scale) that Congress itself repeatedly chooses to use. The mandate-function gap is whether formalism is a structural protection against a permanent danger (executive consolidation) or an increasingly theatrical invocation of a 1787 concern that the 20th-century regulatory state has neutralized through institutional checks and committee oversight. The theater ratio's gradual rise (from 0.25 to 0.41) suggests the mandate is becoming partially obsolete but the doctrine persists, not because the founding danger is live, but because Congress and the judiciary benefit from refusing the delegation and blaming the Constitution for the refusal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is the constitutional boundary between legislative and executive power a structural feature of political reality that emerges necessarily from the separation principle, or a constructed interpretive choice that benefits institutional actors who benefit from strict enforcement?',
    'Comparative constitutional analysis across systems with different separation readings; historical examination of whether formalist boundaries predate the Constitution or were imposed retroactively by specific jurists and interest groups.',
    'If the boundary is genuinely natural (structurally inevitable), the constraint is a mountain. If it is a constructed choice benefiting Congress and the judiciary (institutional preservation against executive consolidation), FSM fires and reclassifies to tangled_rope: Congress and judiciary benefit from delegations'' unconstitutionality; administrative agencies and executive bear costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Whether strict separation is discovered natural law or authored institutional strategy.').

omega_variable(
    intelligible_principle_standard_contestation,
    'What counts as a sufficient intelligible principle to guide delegated authority? The formalist reading treats all delegation as forbidden; the functionalist reading permits delegation with an intelligible principle. Where is the boundary between what amounts to a principle (permissible under functionalism) and what amounts to a blank check (forbidden under formalism)?',
    'Specification of the formalist''s standard for legislative guidance (what textual markers, precedent patterns, or doctrinal rules define the boundary); empirical analysis of how agencies actually operate under stated standards versus how they would operate with no standards.',
    'A clear, workable boundary standard would make formalism enforceable and allow some coordination function to survive. Lack of a workable boundary would establish formalism as purely suppressive (no coordination gain, only extraction for those who benefit from delegations'' unconstitutionality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_standard_contestation, empirical, 'Whether the intelligible principle standard is coherent and enforceable.').

omega_variable(
    regulatory_capacity_coordination_vs_extraction,
    'Is the suppression of delegated regulatory authority a cost-free shift of power to Congress and judiciary, or does it eliminate coordination functions that no other institutional structure can perform at the same scale and responsiveness?',
    'Historical analysis of pre-delegation regulatory capacity (could Congress actually legislate in detail on interstate commerce, financial systems, labor standards without delegation?); comparative analysis of non-delegating systems and their regulatory output and responsiveness.',
    'If delegation is pure extraction with no coordination loss, formalism is a snare masquerading as a mountain. If delegation is genuine coordination (Congress and judiciary cannot perform the function at scale), then suppressing it imposes real costs on the system; formalism is either a mountain with collateral damage, or a tangled_rope enforced by those who benefit from delegations being forbidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_coordination_vs_extraction, empirical, 'Whether suppressing delegation eliminates coordination functions or merely redistributes power.').

omega_variable(
    reading_contestation_in_kernel,
    'This constraint is the formalist reading of separation-of-powers doctrine. The sibling readings (functionalist and unitary executive) claim the same constitutional text but instantiate different constraints with different ε values and victim sets. Is there one correct reading of the Constitution''s separation-of-powers text, or are all three readings live positions held by different institutional actors and interpretive traditions simultaneously?',
    'Legal history and doctrine analysis: do the three readings coexist in contemporary jurisprudence with no single reading commanding consensus, or has one reading been formally superseded? Analysis of the authority structure: do different courts, branches, and legal communities hold different readings as legitimate doctrine?',
    'If all three coexist, they are three constraints (three stories), linked via network.affects_constraints, not one constraint viewed from three angles. If one has been formally overridden within the judiciary''s own doctrine, that axiom in the formalist reading carries status=''overridden''. If the readings represent a stable institutional dispute across different seats (Congress favors functionalism, judiciary splits, Executive resists formalism), the dispute itself is the salient structural fact, not the truth of any one reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_in_kernel, conceptual, 'Whether the kernel has one correct reading or three live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sepa_tr_t0, observed).
narrative_ontology:measurement(sepa_tr_t5, separation_of_powers_text__formalist_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(sepa_tr_t5, observed).
narrative_ontology:measurement(sepa_tr_t10, separation_of_powers_text__formalist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(sepa_tr_t10, observed).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__formalist_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(sepa_tr_t15, observed).
narrative_ontology:measurement(sepa_tr_t20, separation_of_powers_text__formalist_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(sepa_tr_t20, observed).
narrative_ontology:measurement(sepa_tr_t25, separation_of_powers_text__formalist_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(sepa_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(sepa_be_t0, observed).
narrative_ontology:measurement(sepa_be_t5, separation_of_powers_text__formalist_reading, base_extractiveness, 5, 0.73).
narrative_ontology:measurement_basis(sepa_be_t5, observed).
narrative_ontology:measurement(sepa_be_t10, separation_of_powers_text__formalist_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(sepa_be_t10, observed).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__formalist_reading, base_extractiveness, 15, 0.81).
narrative_ontology:measurement_basis(sepa_be_t15, observed).
narrative_ontology:measurement(sepa_be_t20, separation_of_powers_text__formalist_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement_basis(sepa_be_t20, observed).
narrative_ontology:measurement(sepa_be_t25, separation_of_powers_text__formalist_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(sepa_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(sepa_su_t0, observed).
narrative_ontology:measurement(sepa_su_t5, separation_of_powers_text__formalist_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement_basis(sepa_su_t5, observed).
narrative_ontology:measurement(sepa_su_t10, separation_of_powers_text__formalist_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement_basis(sepa_su_t10, observed).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__formalist_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement_basis(sepa_su_t15, observed).
narrative_ontology:measurement(sepa_su_t20, separation_of_powers_text__formalist_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(sepa_su_t20, observed).
narrative_ontology:measurement(sepa_su_t25, separation_of_powers_text__formalist_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement_basis(sepa_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% The separation-of-powers kernel decomposes into three constraint stories (formalist, functionalist, unitary executive readings), each with its own ε value, victim set, and structural position. They are not three views of one constraint; they are three constraints that contest the same constitutional text and each instantiates different institutional consequences. The formalist reading (this story) establishes strict boundaries and forbids delegation. The functionalist reading (sibling) permits delegation with intelligent principles. The unitary executive reading (sibling) permits delegation only through the President. Network edges link them for contamination analysis: if one reading gains judicial adoption, the others' institutional support erodes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
