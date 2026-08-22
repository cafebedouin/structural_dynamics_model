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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Separation of Powers: Categorical Delegation Prohibition
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   Under the formalist reading of the separation of powers, the
 *   Constitution's vesting of legislative power in Congress and executive
 *   power in the President creates a categorical, impermeable boundary.
 *   Congress cannot delegate legislative authority to administrative
 *   agencies; any statutes purporting to grant rule-making power to agencies
 *   are ultra vires. This reading interprets the Vesting Clauses as strict
 *   structural limits, not flexible grants. The constraint operates to
 *   suppress agency rule-making, redirect regulatory authority to Congress,
 *   and extract the de facto legislative power agencies currently wield. The
 *   claim/metric gap is intentional: the formalist reading CLAIMS this is a
 *   mountain (an unchangeable structural limit derived from the
 *   Constitution's text and the Framers' intent). The authored metrics
 *   describe a highly extractive, aggressively enforced constraint that
 *   concentrates power in Congress and suppresses executive/administrative
 *   alternatives—exactly the profile of a tangled_rope with strong
 *   beneficiaries (Congress, formalist doctrine) and clear victims (agencies,
 *   regulated advocates). The engine will compute whether this divergence
 *   reflects a false-summit effect (a constraint benefiting identifiable
 *   parties and masquerading as natural law) or structural fidelity to the
 *   reading's own premises.
 *
 * KEY AGENTS:
 *   - Congress: institutional beneficiary and de facto agenda-setter; under formalist reading, recaptures rule-making authority previously delegated to agencies.
 *   - Regulatory agencies: institutional victim; all rule-making authority delegated to them becomes constitutionally void; reduced to execution-only role.
 *   - Executive branch administrators: institutional victims; lose delegated policy-making discretion; cannot adapt regulations without statutory amendment.
 *   - Regulated industry: beneficiary (reduced adaptive regulation); faces lower compliance burden but loses technical-expertise input from agency processes.
 *   - Public-interest advocates: victims (environmental, consumer, labor); depend on agencies for translating broad mandates into protective rules; must lobby Congress directly.
 *   - Courts: agenda-setter/enforcer; strike down delegated agency authority; interpret Vesting Clauses as categorical prohibitions.
 *   - Constitutional formalists: vindicated doctrine (non-agent); the intellectual tradition grounding formalist reading benefits from constraint's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.81).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.88).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Separation of Powers: Categorical Delegation Prohibition").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '4276900a-53c8-4482-ab71-b18f6db38ba1').
narrative_ontology:cs_kernel_codification('4276900a-53c8-4482-ab71-b18f6db38ba1', fixed_text).
narrative_ontology:cs_authority_grounding('4276900a-53c8-4482-ab71-b18f6db38ba1', extraction).
narrative_ontology:cs_interpretation_layer_present('4276900a-53c8-4482-ab71-b18f6db38ba1').
narrative_ontology:cs_reading_relation('4276900a-53c8-4482-ab71-b18f6db38ba1', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('4276900a-53c8-4482-ab71-b18f6db38ba1', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('4276900a-53c8-4482-ab71-b18f6db38ba1', foundational, legislative_power_vesting_categorical).
narrative_ontology:cs_axiom_status(legislative_power_vesting_categorical, holdable).
narrative_ontology:cs_axiom_grounding('4276900a-53c8-4482-ab71-b18f6db38ba1', legislative_power_vesting_categorical, deontological).
narrative_ontology:cs_axiom('4276900a-53c8-4482-ab71-b18f6db38ba1', foundational, delegation_constitutionally_impermissible).
narrative_ontology:cs_axiom_status(delegation_constitutionally_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('4276900a-53c8-4482-ab71-b18f6db38ba1', delegation_constitutionally_impermissible, empirically_contingent).
narrative_ontology:cs_reference_frame('4276900a-53c8-4482-ab71-b18f6db38ba1', vesting_clause_absolute_separation).
narrative_ontology:cs_drift_state('4276900a-53c8-4482-ab71-b18f6db38ba1', contemporary_post_new_deal_administrative_state, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('4276900a-53c8-4482-ab71-b18f6db38ba1', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, constitutional_formalists).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulatory_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_industry).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_branch_administrators).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, public_interest_advocates).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, legislative_power_vesting_clause_supremacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, structural_limits_on_delegation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains exclusive legislative authority under formalist reading; cannot delegate without violating Constitution, so Congress maintains its rule-making monopoly. Acts as agenda-setter by enacting statutes (even delegating statutes are Congressional acts). Benefits by preserving its institutional power and avoiding erosion to executive/agency authority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congress, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, congress, agenda_setter).

% All rule-making authority delegated to them becomes constitutionally void under formalist constraint. Cannot promulgate binding regulations beyond executing statute text. Trapped: cannot exit administrative law or escape Constitution's constraint on their power.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulatory_agencies, payer,
    institutional, generational, trapped, national).

% Lose delegated policy-making authority. Must operate within strict statutory limits; cannot adapt regulations to new circumstances without Congressional amendment. Constrained by formalist reading; cannot claim executive authority to fill regulatory gaps.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, executive_branch_administrators, payer,
    institutional, generational, constrained, national).

% Faces reduced adaptive regulation when agencies are stripped of rule-making authority. Regulatory landscape collapses to statute text only; industry gains predictability and reduced compliance burden. Lobbies Congress directly rather than navigating agency notice-and-comment. Mobility allows exit to less-regulated jurisdictions if formalist constraint persists.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industry, beneficiary,
    powerful, biographical, mobile, national).

% Environmental, consumer protection, labor advocates depend on agencies to translate broad statutory mandates into detailed protective rules. Under formalism, all technical rules must be in statute; Congress cannot delegate standard-setting to agencies. Advocates are constrained: administrative process is foreclosed; must convince Congress directly on every technical detail.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, public_interest_advocates, payer,
    organized, biographical, constrained, national).

% Enforce formalist separation of powers by striking down delegated agency authority. Interpret Vesting Clauses as categorical prohibitions. Courts are the interpretive gatekeepers; they maintain the constraint through judicial review and constitutional interpretation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% The scholarly and judicial tradition asserting strict, categorical separation of powers. The constraint's operation vindicates formalist doctrine; the doctrine gains coherence, institutional support, and juridical authority from the constraint's enforcement. Non-agent entity kept for narrative completeness; does not enter beneficiary derivation but benefits from constraint operation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, constitutional_formalists, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(separation_of_powers_text__formalist_reading, constitutional_formalists).

% Judges holding functionalist or living-constitution readings are excluded from rule-making (applying their interpretive framework). Judicial precedent and formalist constitutional doctrine trap them; they cannot articulate functionalist arguments at the bench or overturn formalist precedent without institutional change.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_judges, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, congress).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes reliable, predictable boundaries between branches: Congress makes rules (via statutes), executive executes them, courts interpret them. Prevents concentration of power by making each branch depend on others for its statutory grants. Ensures democratic accountability by requiring elected bodies (Congress) to make binding policy decisions, not delegate to unelected bureaucrats.
% TRANSFER_FUNCTION: Transfers rule-making authority FROM agencies (and executive discretion) TO Congress; transfers the cost of regulation FROM industry (via adaptive agency rules) TO public advocates (who must convince Congress for every technical standard); transfers institutional leverage FROM executive branch TO legislative branch. Moves de facto policy-making power back to Congress explicitly accountable at ballot.
% ABSENT_VOICES: Regulatory scientists and technical experts embedded in agencies cannot speak meaningfully in rule-making; their expertise must be filtered through Congressional committees (which have limited technical capacity). Citizens who depend on rapid adaptive response to new harms (emerging environmental toxins, novel financial instruments, pandemic response) are structurally excluded; Congress cannot respond at technical-standard speeds. International coordination bodies are excluded: no agency authority to negotiate and implement international environmental or labor treaties administratively.
% DISAPPEARANCE_RATIONALE: If formalist separation-of-powers constraint vanished and delegation became categorically permitted (functionalist reading dominates), regulatory agencies would exercise massive rule-making authority. Regulatory volume, scope, and adaptive capacity would expand; industries would face detailed, context-sensitive regulation; Congress would lose its monopoly on formal rule-making; the speed and scope of regulatory governance would shift from statutory-text-only to agency-adaptive. The modern administrative state's architecture depends entirely on this constraint's absence.
% FOUNDING_PROBLEM: The Constitution's text vests legislative power in Congress and executive power in President. The Framers intended to prevent tyranny through separated powers, each checking the others. If Congress delegates its legislative authority to executive agencies, the separation collapses and executive power swells unchecked—tyranny risk returns.
% FOUNDING_PROBLEM_CORROBORATION: Formalist scholars (Randy Barnett, originalist jurists including Scalia in earlier writings, Gorsuch in recent dissents) attest the problem is live: delegation remains unconstitutional under original public meaning of the Vesting Clauses. Functionalist scholars and the federal judiciary majority attest the founding problem is obsolete: modern governance requires delegation; the intelligible principle doctrine solves the accountability problem; the founding concern about tyranny is addressed through Congressional oversight and delegation bounds, not prohibition. Political scientists and administrative law experts from OUTSIDE the constitutionalist tradition attest that formalist constraint is unworkable: it would collapse the entire modern regulatory state and return governance to 18th-century statutory scope.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the formalist constraint redistributes real institutional power: it strips de facto legislative authority from agencies and returns it to Congress, where it can be more easily concentrated or weaponized against specific industries or constituencies. The extraction is not rent in a narrow sense but structural authority. Suppression is higher still (0.88) because the constraint's persistence depends on actively suppressing functionalist and living-constitution judicial interpretations—courts must enforce the categorical prohibition even where it produces governance gridlock. Theater is moderate (0.42): formalist justifications invoke constitutional text and structural principles (real), but the effect of collapsing adaptive regulation serves industry interests and Congress's appetite for concentrated control (functional outcome drives the structure). The measurement series projects rising extraction and suppression over 40 years (representing, under formalist ascendancy, the increasing judicial enforcement of strict delegation bounds), with theater_ratio stabilizing after initial uptake—once courts establish the boundary, enforcement becomes routine rather than novel.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's seat, the constraint is genuine coordination (structure that clarifies authority, prevents executive overreach, ensures democratic accountability). From agencies' and advocates' seats, the same structure operates as forced extraction (stripped authority, foreclosed policy capacity). Regulated industry sees it as coordination benefit (predictable boundaries, reduced compliance variation). The formalist reading itself claims the structure is natural law (constitutional text and structure), but the authored metrics describe it as actively enforced, suppressing alternatives—this gap is the engine's target. The measurement series show rising suppression_requirement over the interval, which would be anomalous if the constraint truly operated as natural law (natural laws do not require increasing enforcement effort—if anything, enforcement should stabilize or decline as compliance becomes habitual). Rising suppression_requirement signals that the boundary is contested and requires active judicial work to maintain, which is consistent with a tangled_rope falsely clothed in natural-law language.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and formalist doctrine are the beneficiaries: d near 0.0 (full benefit). Regulatory agencies and executive administrators are the targets: d near 1.0 (extraction). Regulated industry sits near symmetric to beneficiary (benefits from reduced regulation, absorbs cost of direct Congressional lobbying instead of agency process). Public advocates sit clearly target-side (d > 0.8): they bear the cost of agency suppression (reduced adaptive regulation) and must navigate a rule-making path (Congress) with much lower capacity for technical input. Courts occupy an agenda-setter position: they enforce the boundary, but they also experience the constraint (judicial discretion to interpret delegation broadly is suppressed). The directionality profile is asymmetric enough that different seats should compute different types: Congress and industry compute beneficiary-side; agencies and advocates compute target-side. The engine's per-seat computation is exactly where the seat-divergence emerges.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing tyranny through separated powers) remains contested: formalists say it is live (delegation violates the text), functionalists say it is dead (modern practice shows delegation is constitutional), and law-and-economics tradition says it is obsolete relative to governance demands. The disappearance_verdict is world_rearranges: if the formalist constraint vanished, regulatory capacity would expand massively and the administrative state's architecture would fundamentally shift. This mismatch (live/contested problem + world_rearranges verdict) does NOT flag mandatrophy directly—the problem is still real. But the measurement series and the rising suppression_requirement suggest the constraint requires increasing active enforcement to maintain, which is consistent with a constraint whose original function (preventing executive overreach in an 18th-century context) persists only rhetorically while the active work of suppression now serves institutional self-interest (Congress preserving its rule-making monopoly, courts preserving separation doctrine as a stable hermeneutic frame). The theater_ratio's climb from 0.25 to 0.42 suggests that the justificatory work (structural necessity, constitutional fidelity) takes up a larger fraction of the constraint's operation over time—a signal of creeping theatricality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_power_grab,
    'Is the formalist separation of powers a genuine structural limit derived from constitutional text and democratic theory, or a mechanism for Congress to concentrate rule-making authority against executive/agency encroachment?',
    'Historical analysis of Congressional action on agency rule-making: does Congress reverse or cap agency authority when formalist judges rule delegation unconstitutional, or does Congress legislate around the ruling by enacting new statutes that redelegate the authority? Evidence of re-delegation after formalist victories would suggest the constraint serves institutional self-interest, not principled constitutional boundary-keeping.',
    'If resolution finds re-delegation, the constraint reclassifies from natural-law mountain toward snare or tangled_rope (beneficiaries retain extracted authority through legislative maneuver, victims remain suppressed). If resolution finds Congress respects the boundary, the formalist reading gains coherence as genuine structural principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_power_grab, empirical, 'Whether formalist separation of powers constrains Congress''s behavior or merely redirects its authority-exercise.').

omega_variable(
    functionalist_vs_formalist_reconciliation,
    'Can the functionalist reading (intelligible principle delegation) and the formalist reading (categorical prohibition) coexist within a single coherent constitutional framework, or do they logically foreclose each other?',
    'Jurisprudential analysis: can a court hold that delegation is categorically prohibited AND that intelligible principle delegation is constitutionally permissible? Or does accepting intelligible principle as constitutional necessarily abandon categorical prohibition? Legal scholars and jurists differ; the resolution depends on whether the Vesting Clauses are read as absolute or as subject to evolved interpretation.',
    'If they genuinely foreclose each other (binary choice between formalism and functionalism), then the kernel exhibits true choice-point contestation. If they can coexist (both held by different courts or jurisdictions), then the contest is inter-institutional rather than logically forced. This affects whether the engine flags the kernel as irresolvable or merely contentious.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functionalist_vs_formalist_reconciliation, conceptual, 'Whether formalist and functionalist readings of separation of powers are logically reconcilable or mutually foreclosing.').

omega_variable(
    suppression_mechanism_structural_or_institutional,
    'Is the high suppression (0.88) in the formalist constraint structural (the constraint itself prevents alternatives from being articulated) or institutional (formalist judges actively suppress functionalist arguments at the bench)?',
    'Jurisprudential process audit: are functionalist arguments rejected on structural grounds (the Vesting Clauses admit no intelligible principle doctrine) or on institutional grounds (the formalist majority refuses to hear them)? Post-institutional-change test: if the court composition shifts toward functionalism, do formalist constraints disappear from opinions, or do they persist?',
    'If suppression is structural, the high value (0.88) is justified and reflects genuine categorical foreclosure. If suppression is institutional, the high value signals that the constraint is maintained through judicial gatekeeping rather than constitutional necessity—a sign of theater and institutional self-interest masquerading as constitutional principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_institutional, empirical, 'Whether high suppression in formalist reading reflects structural limits or institutional enforcement asymmetry.').

omega_variable(
    functionalist_reading_interdependence,
    'Does the formalist reading''s ε depend on how the functionalist reading is framed or measured, or is formalist ε invariant across readings?',
    'Compare the formalist reading''s measured ε (0.81 extraction, 0.88 suppression) against the functionalist reading''s measured ε when both are authored independently (separate JSON files). Do the readings converge on the same extraction/suppression values (invariant per constraint-identity principle), or do they diverge substantially (suggesting that ε is reading-indexed, not constraint-indexed)?',
    'Per OQ-26 and DP-001, ε should be reading-indexed (a well-founded abolitionist and welfarist reading assign different ε to the same standing arrangement). This omega tests whether the formalist/functionalist split violates the ε-invariance principle or instantiates it correctly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalist_reading_interdependence, conceptual, 'Whether formalist-reading ε is independent of functionalist-reading framing (ε-invariance test for kernel decomposition).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sepa_tr_t0, projected).
narrative_ontology:measurement(sepa_tr_t5, separation_of_powers_text__formalist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(sepa_tr_t5, projected).
narrative_ontology:measurement(sepa_tr_t10, separation_of_powers_text__formalist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(sepa_tr_t10, projected).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__formalist_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(sepa_tr_t15, projected).
narrative_ontology:measurement(sepa_tr_t20, separation_of_powers_text__formalist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(sepa_tr_t20, projected).
narrative_ontology:measurement(sepa_tr_t25, separation_of_powers_text__formalist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(sepa_tr_t25, projected).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__formalist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(sepa_tr_t30, projected).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__formalist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(sepa_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(sepa_be_t0, projected).
narrative_ontology:measurement(sepa_be_t5, separation_of_powers_text__formalist_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(sepa_be_t5, projected).
narrative_ontology:measurement(sepa_be_t10, separation_of_powers_text__formalist_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(sepa_be_t10, projected).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__formalist_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(sepa_be_t15, projected).
narrative_ontology:measurement(sepa_be_t20, separation_of_powers_text__formalist_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(sepa_be_t20, projected).
narrative_ontology:measurement(sepa_be_t25, separation_of_powers_text__formalist_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(sepa_be_t25, projected).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__formalist_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(sepa_be_t30, projected).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__formalist_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(sepa_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(sepa_su_t0, projected).
narrative_ontology:measurement(sepa_su_t5, separation_of_powers_text__formalist_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement_basis(sepa_su_t5, projected).
narrative_ontology:measurement(sepa_su_t10, separation_of_powers_text__formalist_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement_basis(sepa_su_t10, projected).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__formalist_reading, suppression_requirement, 15, 0.84).
narrative_ontology:measurement_basis(sepa_su_t15, projected).
narrative_ontology:measurement(sepa_su_t20, separation_of_powers_text__formalist_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement_basis(sepa_su_t20, projected).
narrative_ontology:measurement(sepa_su_t25, separation_of_powers_text__formalist_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(sepa_su_t25, projected).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__formalist_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement_basis(sepa_su_t30, projected).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__formalist_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(sepa_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__formalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the separation_of_powers_text kernel. The formalist reading interprets the Vesting Clauses as categorical prohibitions on legislative delegation. The functionalist_reading interprets separation of powers as flexible framework permitting intelligible-principle delegation. The unitary_executive_reading interprets all executive power as vesting solely in the President, making independent agencies unconstitutional. Each reading instantiates a distinct constraint with distinct ε, beneficiary/victim structure, and classification. They are linked as a constraint family: the formalist reading forecloses the functionalist reading (logically incompatible core premises), coexists with the unitary executive reading (both suppress agency authority from different grounds), and influences the unitary executive reading (by narrowing the space in which executive discretion can operate). The family should be analyzed together to show how a single constitutional commitment (separation of powers) generates multiple, partially incompatible instantiations depending on reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
