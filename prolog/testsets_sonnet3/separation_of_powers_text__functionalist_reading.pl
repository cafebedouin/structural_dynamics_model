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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Reading: Separation of Powers as Flexible Coordination Framework
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   This story instantiates the functionalist reading of the
 *   separation-of-powers kernel: the view, dominant in operative federal
 *   doctrine since roughly 1935 (Schechter Poultry notwithstanding — the
 *   intelligible-principle standard has upheld nearly every delegation
 *   since), that Article I, II, and III authority may overlap functionally so
 *   long as no branch is rendered subordinate and Congress supplies an
 *   'intelligible principle' to guide agency discretion. Under this reading
 *   the modern administrative state — agencies that write rules
 *   (legislative-like), adjudicate disputes (judicial-like), and enforce
 *   compliance (executive-like) within a single body — is constitutionally
 *   legitimate coordination, not usurpation. This is NOT the formalist
 *   reading (which holds the boundaries are strict and impermeable, treating
 *   the same delegations as unconstitutional) nor the unitary-executive
 *   reading (which holds all executive power vests exclusively in the
 *   President and treats independent, insulated agencies as themselves
 *   violating separation of powers). Each of those is a separate constraint
 *   story with its own ε, beneficiary structure, and classification; they are
 *   linked here only by network reference, not folded into this one.
 *
 * KEY AGENTS:
 *   - administrative_agencies: institutional beneficiary/agenda_setter — exercise combined functions legitimized by this reading
 *   - congress: institutional beneficiary/agenda_setter — delegates broadly without needing to draft technical detail
 *   - president_and_executive_office: institutional beneficiary — shares functional coordination rather than holding exclusive unitary control
 *   - regulated_entities_facing_agency_discretion: moderate-power payer — bears compliance and discretion costs with limited practical recourse
 *   - litigants_challenging_agency_action: moderate-power payer — nondelegation challenges rarely succeed under this doctrine
 *   - public_beneficiaries_of_regulation: powerless beneficiary — receives the substantive regulatory output the framework preserves
 *   - formalist_judges_and_scholars: excluded organized voice — objects from outside the operative standard
 *   - judiciary: institutional observer/agenda_setter — calibrates the doctrine's tolerance for overlap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.32).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.28).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Reading: Separation of Powers as Flexible Coordination Framework").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'bd2c9277-d85c-4266-a944-88516879b881').
narrative_ontology:cs_kernel_codification('bd2c9277-d85c-4266-a944-88516879b881', fixed_text).
narrative_ontology:cs_authority_grounding('bd2c9277-d85c-4266-a944-88516879b881', lineage).
narrative_ontology:cs_interpretation_layer_present('bd2c9277-d85c-4266-a944-88516879b881').
narrative_ontology:cs_reading_relation('bd2c9277-d85c-4266-a944-88516879b881', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd2c9277-d85c-4266-a944-88516879b881', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('bd2c9277-d85c-4266-a944-88516879b881', foundational, functional_overlap_permissible_absent_subordination).
narrative_ontology:cs_axiom_status(functional_overlap_permissible_absent_subordination, holdable).
narrative_ontology:cs_axiom_grounding('bd2c9277-d85c-4266-a944-88516879b881', functional_overlap_permissible_absent_subordination, conventional).
narrative_ontology:cs_axiom('bd2c9277-d85c-4266-a944-88516879b881', foundational, intelligible_principle_suffices_for_valid_delegation).
narrative_ontology:cs_axiom_status(intelligible_principle_suffices_for_valid_delegation, holdable).
narrative_ontology:cs_axiom_grounding('bd2c9277-d85c-4266-a944-88516879b881', intelligible_principle_suffices_for_valid_delegation, instrumental).
narrative_ontology:cs_reference_frame('bd2c9277-d85c-4266-a944-88516879b881', post_new_deal_administrative_settlement).
narrative_ontology:cs_drift_state('bd2c9277-d85c-4266-a944-88516879b881', contemporary_major_questions_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bd2c9277-d85c-4266-a944-88516879b881', '').
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
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, chevron_style_deference_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise delegated rulemaking, adjudicatory, and enforcement authority under statutes that supply only an 'intelligible principle' rather than detailed rules. This reading legitimizes their combined quasi-legislative, quasi-executive, and quasi-judicial functions as necessary coordination in a complex regulatory state, rather than as constitutionally suspect blending of powers.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, administrative_agencies, agenda_setter).

% Delegates broad rulemaking authority to agencies via general statutory standards, avoiding the political cost and technical burden of writing detailed rules itself. Retains oversight, appropriations, and override authority as its channel of continuing influence rather than direct rule-drafting.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congress, agenda_setter).

% Directs agency priorities through appointments, OMB review, and executive orders, sharing functional control over policy implementation with Congress and the agencies themselves rather than holding exclusive unitary control. Benefits from flexible boundaries that let the administration coordinate policy across agencies without triggering strict separation challenges.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president_and_executive_office, beneficiary,
    institutional, generational, mobile, national).

% Must comply with agency rules and adjudications made under broad delegated standards, often with limited advance notice of how discretion will be exercised. Judicial review is deferential (arbitrary-and-capricious, Chevron-style deference where still applied), so the practical avenue to contest an unfavorable interpretation is narrow; compliance costs are borne regardless of the delegation's constitutional pedigree.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_entities_facing_agency_discretion, payer,
    moderate, biographical, constrained, national).

% Bring nondelegation or ultra vires challenges to agency action and encounter a doctrinal environment where the intelligible-principle standard has, since 1935, upheld nearly every delegation challenged. Their exit option — litigation to a favorable separation-of-powers ruling — is real but has succeeded so rarely that it functions as a formality rather than a live check.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, litigants_challenging_agency_action, payer,
    moderate, biographical, constrained, national).

% Receive the substantive output of the regulatory state this reading preserves — environmental, financial, workplace-safety, and consumer protections that would be far harder for Congress to legislate and update line-by-line. Cannot exit the arrangement individually; their interest is in the regulatory state functioning, not in the specific doctrinal justification.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, public_beneficiaries_of_regulation, beneficiary,
    powerless, generational, trapped, national).

% Argue from outside the operative doctrine that broad delegation and combined-function agencies violate the constitutional structure regardless of practical necessity. Their view shapes academic and some appellate debate and occasionally surfaces in concurrences, but does not currently control controlling doctrine, so their objection is present in discourse but excluded from the operative legal standard.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_judges_and_scholars, excluded,
    organized, generational, analytical, national).

% Reviews delegation and agency-action challenges under the intelligible-principle standard and deference doctrines, itself an agenda-setter insofar as its doctrinal choices calibrate how much functional overlap the framework tolerates, while also functioning as the analytical seat assessing the arrangement's constitutionality.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, judiciary, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, judiciary, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Congress to set broad policy goals and delegate technical implementation to expert agencies, and allows the President to coordinate policy execution across those agencies, without every regulatory decision requiring fresh legislation or triggering a strict-separation veto — solving the real problem that a legislature cannot write, and continuously rewrite, the technical detail a modern regulatory state requires.
% TRANSFER_FUNCTION: Moves rulemaking, adjudicatory, and enforcement discretion from the legislature (and, on the formalist view, from an undivided executive) to agencies operating under broad statutory standards and judicial deference doctrines; the practical cost of that discretion is borne by regulated entities and challengers who face a low probability of prevailing in a separation-of-powers challenge.
% ABSENT_VOICES: Formalist judges and scholars who hold that combined agency functions and broad delegation are constitutionally impermissible regardless of practical benefit are present in legal discourse but structurally outside the doctrine actually applied by reviewing courts; litigants raising nondelegation challenges are heard but almost never prevail.
% DISAPPEARANCE_RATIONALE: If the functionalist reading were displaced by strict formalism, most federal agencies' combined rulemaking/adjudicatory/enforcement functions would become constitutionally vulnerable, forcing either agency restructuring, a much narrower delegation practice, or a constitutional-crisis-scale reallocation of governance authority back to Congress — the modern regulatory state as currently organized depends on this reading holding.
% FOUNDING_PROBLEM: The founding problem this reading solves is the practical impossibility of a legislature directly managing a technically complex, fast-moving administrative state through statute alone, combined with the need for the executive to coordinate implementation without either branch appearing to arrogate the other's constitutional function.
% FOUNDING_PROBLEM_CORROBORATION: Beyond the agencies and Congress that benefit from the doctrine, independent administrative-law scholars, the Administrative Conference of the United States, and comparative-governance researchers studying peer democracies attest that some mechanism for technical delegation is functionally necessary in a modern regulatory economy; formalist critics dispute the CONSTITUTIONAL necessity of solving it this particular way, but generally do not dispute that a coordination problem of this kind exists.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.32 at 2025) because the functionalist reading's own account of itself is coordination: it exists to solve a genuine problem (a legislature cannot micromanage a technical regulatory economy) and the overlap it permits is bounded by intelligible-principle review and deference doctrines that at least nominally constrain agency discretion. It is not zero, and it has crept upward over the interval (0.18 to 0.32), because deference doctrines have in practice made judicial review of delegation nearly toothless, shifting real power to agencies with declining practical check. Suppression is lower than a snare or tangled-rope reading would show (0.28) because litigants retain a live, if low-probability, avenue to challenge agency action, and Congress retains override authority it periodically exercises. Theater ratio is modest (0.22) reflecting that judicial review of delegation, while rarely dispositive, is not purely performative — it still occasionally produces real limits (e.g., major questions doctrine as a partial formalist counter-pressure within otherwise functionalist doctrine).
 *
 * DIRECTIONALITY LOGIC:
 *   Agencies, Congress, and the President are beneficiaries: the framework lets each avoid costs it would otherwise bear (Congress avoids drafting burden, agencies gain operative authority, the President gains coordinated implementation capacity) — d sits toward the beneficiary end for all three. Regulated entities and challengers are targets: they bear compliance costs and litigation costs under a standard that rarely rules in their favor, so d sits toward the target end, moderated by their moderate power and constrained-but-real exit (they can lobby, litigate, or relocate operations, unlike a fully trapped agent). Public beneficiaries of regulation are structurally powerless but are net beneficiaries of the regulatory output this reading preserves, which is why they are coded beneficiary despite trapped exit options — their directionality reflects benefit-received, not power held.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislature cannot micromanage technical governance) remains live by nearly universal non-partisan administrative-law assessment, which is what keeps this from being a mandatrophy case: the coordination function this reading protects has not gone dead even though the deference doctrines that implement it have hardened over time in ways that increasingly favor agency discretion over meaningful review. If a future measurement showed the founding problem going dead (e.g., if technical governance were fully automatable or Congress recovered capacity to legislate at the necessary granularity) while the delegation regime persisted unchanged, that would be the signal for reclassification toward extraction-dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functionalist_reading_stability,
    'Is the functionalist reading''s current doctrinal dominance a stable equilibrium, or is it being eroded by a resurgent formalist/unitary-executive judicial coalition (e.g., major questions doctrine, Seila Law, recent nondelegation concurrences)?',
    'Track the Supreme Court''s disposition of delegation and removal-power cases over the next decade; a rising rate of formalist or unitary-executive rulings against agency structure would indicate the functionalist reading is losing ground as controlling doctrine even where it remains the historically dominant reading.',
    'If formalist or unitary-executive readings gain controlling force, the beneficiary structure authored here (agencies, Congress, President as joint beneficiaries of shared function) would need to be re-evaluated against a narrower operative doctrine; ε for THIS reading would not change (it remains a claim about how much this reading extracts when it IS the operative standard) but the reading''s real-world applicability window would shrink.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalist_reading_stability, empirical, 'Whether the functionalist reading''s current doctrinal dominance is stable or eroding.').

omega_variable(
    coordination_vs_capture_in_deference,
    'Does Chevron-style (or its successors'') deference to agency interpretation represent genuine functional coordination (agencies have comparative expertise) or a mechanism by which agencies capture interpretive authority that properly belongs to courts or Congress?',
    'Compare outcomes in policy domains with high vs. low agency technical expertise relative to reviewing courts; genuine coordination should track expertise gaps, while capture should show deference persisting even where courts have comparable competence.',
    'If deference tracks capture rather than expertise, the extraction component of this reading is understated in the current authoring and the trajectory toward tangled_rope classification would strengthen; if it tracks expertise, the coordination framing is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture_in_deference, conceptual, 'Whether agency deference reflects genuine expertise-based coordination or interpretive capture.').

omega_variable(
    kernel_framing_choice,
    'Is the separation-of-powers kernel best modeled as a single contested text with three readings (as authored here), or does the functionalist reading itself further decompose into distinct claims (e.g., nondelegation tolerance vs. combined-function tolerance vs. removal-power tolerance) that could show materially different ε values under closer analysis?',
    'Test whether nondelegation challenges, combined-function challenges, and removal-power challenges show measurably different success rates and different degrees of doctrinal settledness; if they diverge sharply, they may warrant separate constraint stories under the ε-invariance principle rather than being bundled into one functionalist reading.',
    'If the sub-claims diverge, this single functionalist_reading story may itself require further decomposition into e.g. functionalist_reading_nondelegation and functionalist_reading_removal_power, each with its own ε; as currently authored, this story treats the three doctrinal strands as sufficiently unified in structure and history to share one ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the functionalist reading itself requires further ε-invariant decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__functionalist_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement_basis(sepa_tr_t1935, observed).
narrative_ontology:measurement(sepa_tr_t1955, separation_of_powers_text__functionalist_reading, theater_ratio, 1955, 0.12).
narrative_ontology:measurement_basis(sepa_tr_t1955, observed).
narrative_ontology:measurement(sepa_tr_t1975, separation_of_powers_text__functionalist_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement_basis(sepa_tr_t1975, observed).
narrative_ontology:measurement(sepa_tr_t1995, separation_of_powers_text__functionalist_reading, theater_ratio, 1995, 0.17).
narrative_ontology:measurement_basis(sepa_tr_t1995, observed).
narrative_ontology:measurement(sepa_tr_t2015, separation_of_powers_text__functionalist_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement_basis(sepa_tr_t2015, observed).
narrative_ontology:measurement(sepa_tr_t2025, separation_of_powers_text__functionalist_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(sepa_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__functionalist_reading, base_extractiveness, 1935, 0.18).
narrative_ontology:measurement_basis(sepa_be_t1935, observed).
narrative_ontology:measurement(sepa_be_t1955, separation_of_powers_text__functionalist_reading, base_extractiveness, 1955, 0.2).
narrative_ontology:measurement_basis(sepa_be_t1955, observed).
narrative_ontology:measurement(sepa_be_t1975, separation_of_powers_text__functionalist_reading, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement_basis(sepa_be_t1975, observed).
narrative_ontology:measurement(sepa_be_t1995, separation_of_powers_text__functionalist_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(sepa_be_t1995, observed).
narrative_ontology:measurement(sepa_be_t2015, separation_of_powers_text__functionalist_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement_basis(sepa_be_t2015, observed).
narrative_ontology:measurement(sepa_be_t2025, separation_of_powers_text__functionalist_reading, base_extractiveness, 2025, 0.32).
narrative_ontology:measurement_basis(sepa_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__functionalist_reading, suppression_requirement, 1935, 0.15).
narrative_ontology:measurement_basis(sepa_su_t1935, observed).
narrative_ontology:measurement(sepa_su_t1955, separation_of_powers_text__functionalist_reading, suppression_requirement, 1955, 0.18).
narrative_ontology:measurement_basis(sepa_su_t1955, observed).
narrative_ontology:measurement(sepa_su_t1975, separation_of_powers_text__functionalist_reading, suppression_requirement, 1975, 0.2).
narrative_ontology:measurement_basis(sepa_su_t1975, observed).
narrative_ontology:measurement(sepa_su_t1995, separation_of_powers_text__functionalist_reading, suppression_requirement, 1995, 0.23).
narrative_ontology:measurement_basis(sepa_su_t1995, observed).
narrative_ontology:measurement(sepa_su_t2015, separation_of_powers_text__functionalist_reading, suppression_requirement, 2015, 0.26).
narrative_ontology:measurement_basis(sepa_su_t2015, observed).
narrative_ontology:measurement(sepa_su_t2025, separation_of_powers_text__functionalist_reading, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement_basis(sepa_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__functionalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the separation_of_powers_text kernel. formalist_reading treats the same constitutional text as establishing strict, impermeable branch boundaries under which nondelegation has real teeth and combined agency functions are presumptively unconstitutional — a materially higher-ε, higher-suppression reading from the standpoint of agencies and Congress (who would lose delegated authority) but lower-ε from the standpoint of regulated entities (who would face less discretionary agency power). unitary_executive_reading treats the text as vesting all executive power exclusively in the President, making independent agencies themselves the extraction target — a reading with a wholly different victim set (independent agency officials and Congress's oversight structure) than this functionalist reading. All three share the same underlying text and interval but are authored as separate constraints per the ε-invariance principle: measuring the same clause by three different doctrinal lenses yields three different ε values, hence three stories, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
