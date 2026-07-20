% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure Contract â Academic Freedom Reading
 *   domain: higher education governance / labor economics / institutional theory
 *
 * SUMMARY:
 *   This constraint story instantiates the academic_freedom_reading of the
 *   tenure_contract kernel. It models the permanent faculty appointment as a
 *   mechanism that coordinates truth-seeking by shielding researchers from
 *   political and institutional retaliation. The constraint is read as a
 *   tangled rope: it solves a genuine coordination problem (high-risk inquiry
 *   requires credible protection) while asymmetrically extracting influence
 *   from external political actors who are blocked from using employment
 *   threats to steer research. Students are diffuse beneficiaries through
 *   knowledge production, and university administrations enforce the
 *   contract. The claim is tangled_rope; metrics are authored independently
 *   to describe a moderately extractive, actively enforced constraint with
 *   rising theater and suppression requirements under political polarization.
 *
 * KEY AGENTS:
 *   - tenured_faculty: Primary beneficiary (organized/generational) â receives employment security and autonomy
 *   - students: Secondary beneficiary (moderate/biographical) â receives downstream research and pedagogical benefits
 *   - external_political_actors: Primary payer (powerful/immediate) â bears the cost of blocked oversight levers
 *   - university_administration: Agenda setter (institutional/generational) â enforces the contract and absorbs political pressure
 *   - non_tenure_track_faculty: Excluded (moderate/biographical) â absent from governance despite shared labor context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.56).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.68).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure Contract â Academic Freedom Reading").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher education governance / labor economics / institutional theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, 'd561da70-7396-43dd-ad53-9581f3598ae2').
narrative_ontology:cs_kernel_codification('d561da70-7396-43dd-ad53-9581f3598ae2', formalized).
narrative_ontology:cs_authority_grounding('d561da70-7396-43dd-ad53-9581f3598ae2', lineage).
narrative_ontology:cs_interpretation_layer_present('d561da70-7396-43dd-ad53-9581f3598ae2').
narrative_ontology:cs_reading_relation('d561da70-7396-43dd-ad53-9581f3598ae2', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d561da70-7396-43dd-ad53-9581f3598ae2', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('d561da70-7396-43dd-ad53-9581f3598ae2', foundational, permanent_protection_required_for_high_risk_inquiry).
narrative_ontology:cs_axiom_status(permanent_protection_required_for_high_risk_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('d561da70-7396-43dd-ad53-9581f3598ae2', permanent_protection_required_for_high_risk_inquiry, instrumental).
narrative_ontology:cs_axiom('d561da70-7396-43dd-ad53-9581f3598ae2', foundational, peer_review_selects_research_merit_not_demographic_fit).
narrative_ontology:cs_axiom_status(peer_review_selects_research_merit_not_demographic_fit, holdable).
narrative_ontology:cs_axiom_grounding('d561da70-7396-43dd-ad53-9581f3598ae2', peer_review_selects_research_merit_not_demographic_fit, empirically_contingent).
narrative_ontology:cs_reference_frame('d561da70-7396-43dd-ad53-9581f3598ae2', public_knowledge_protection_framework).
narrative_ontology:cs_drift_state('d561da70-7396-43dd-ad53-9581f3598ae2', contemporary_political_polarization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d561da70-7396-43dd-ad53-9581f3598ae2', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive permanent appointment protections that insulate research and teaching from political or institutional retaliation. They accept institutional loyalty and lower mobility in exchange for academic freedom, enabling inquiry that carries personal or political risk.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, generational, constrained, national).

% Benefit from faculty who can pursue controversial or long-term research without fear of dismissal. They do not administer the tenure contract but receive downstream knowledge and pedagogical stability.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, biographical, mobile, national).

% Seek to steer research agendas or suppress inconvenient findings through funding conditions, legislative pressure, or public campaigns. Tenure blocks the employment-threat lever, forcing them to use costlier or slower influence channels.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    powerful, immediate, constrained, national).

% Administers tenure review, awards permanent appointments, and bears the long-term financial and governance obligations. They enforce the contract against external political pressure and internal challenges, serving as the buffer between faculty and interventionists.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Teach and research under term contracts without tenure protections. They are largely absent from tenure governance and peer-review committees that award permanent status, though their working conditions are indirectly shaped by the institution's long-term tenure obligations.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, non_tenure_track_faculty, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of truth-seeking under political and institutional pressure: individual researchers cannot bear the personal cost of high-risk inquiry without a binding, credible commitment that their livelihood will survive displeasure. Tenure makes the commitment credible and pools the risk institutionally.
% TRANSFER_FUNCTION: Moves employment security and autonomy from the institution to the researcher, while moving the cost of political influence and short-term pressure away from the researcher toward external political actors, who must find costlier levers.
% ABSENT_VOICES: Non-tenure-track faculty and contingent academic labor are largely excluded from tenure governance and peer-review committees; they would argue that the contract's protections are hoarded within a shrinking share of the academic workforce, but they are not in the room when tenure is justified as a truth-seeking mechanism.
% DISAPPEARANCE_RATIONALE: If tenure vanished, researchers would immediately face employment risk for controversial findings, shifting inquiry toward safer, fundable topics. Universities would reorganize around short-term contracts, and political actors would gain direct leverage over faculty through budget and employment threats.
% FOUNDING_PROBLEM: Research and teaching are vulnerable to political retaliation and institutional whim; without credible protection, rational researchers self-censor, producing socially valuable but institutionally risky knowledge only when personal cost is low.
% FOUNDING_PROBLEM_CORROBORATION: Faculty associations and academic freedom organizations attest the problem is live, citing contemporary legislative attacks on curriculum and research topics. External political actors and some university governing boards attest the problem is overstated or obsolete. Independent higher-education historians and sociologists corroborate that political pressure on faculty has intensified cyclically, supporting the live-status reading from outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.56 at interval end) is moderate: the constraint genuinely coordinates academic freedom but actively suppresses cheaper political influence channels, which constitutes extraction from political actors. Suppression (0.68) is higher than extraction because the constraint's persistence depends on active institutional resistance to legislative and donor pressure. Theater ratio (0.38) reflects growing procedural formalism in tenure review that partially displaces the original protective function. Accessibility collapse (0.62) captures the dominance of the tenure model in US research universities relative to alternative governance arrangements. Resistance (0.55) reflects active political campaigns against tenure in multiple jurisdictions. The temporal series share a single grid and show rising extraction and suppression as political polarization intensifies, while theater accumulates from bureaucratic drift.
 *
 * PERSPECTIVAL GAP:
 *   The tenured faculty seat experiences the constraint as protective coordination (low effective extraction, high subsidy), while the external political actor seat experiences it as an active barrier to democratic oversight (high effective extraction). The administration sits between, with directionality near symmetric: it pays the long-term financial obligation and absorbs political friction, while receiving institutional stability and reputation benefits. The engine computes this divergence from the structural role declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Faculty are declared beneficiaries with constrained exit (institutionally bound but protected), yielding low directionality toward the constraint. Students are beneficiaries with mobile exit, yielding very low directionality. External political actors are declared payers with constrained exit (they cannot easily bypass tenure without costly systemic change), yielding high directionality. Administration is agenda_setter with constrained exit (bound by norms and competitive markets), yielding mid-range directionality. No overrides are needed because the derivation chain captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â protecting inquiry from retaliation â remains contested but structurally live. The R5 genealogy interview prevents mislabeling this as a piton: the administration could theoretically dismantle tenure, but the cost is prohibitive and the beneficiaries are concentrated and organized, not diffuse. It prevents mislabeling as a snare because the coordination function is genuine: tenure does not merely extract from political actors; it produces a credence good (unbiased research) that is difficult to generate under direct political control. The tangled_rope classification captures both halves without collapsing into either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_oversight_suppression_necessity,
    'Is the active suppression of political oversight levers structurally necessary for truth-seeking, or does it overreach by insulating faculty from legitimate democratic accountability?',
    'Comparative analysis of jurisdictions with and without tenure-style protections, measuring research risk-taking and political interference rates.',
    'If oversight suppression is necessary, the coordination half dominates; if overreach, the extraction half dominates and the constraint slides toward snare for political actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_oversight_suppression_necessity, conceptual, 'Whether blocking political employment threats is essential coordination or extractive overreach.').

omega_variable(
    tenure_causation_of_risk_taking,
    'Does tenure causally increase high-risk inquiry, or do researchers self-censor due to reputational and funding pressures regardless of employment security?',
    'Natural experiment comparing pre-tenure and post-tenure publication portfolios; also compare tenure-track vs. contract researchers in similar fields.',
    'If tenure does not increase risk-taking, the coordination story is cover for extraction by faculty; if it does, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_causation_of_risk_taking, empirical, 'Empirical test of tenure''s claimed coordination benefit.').

omega_variable(
    kernel_reading_sibling_boundary,
    'This constraint is the academic_freedom_reading of the tenure_contract kernel. How would adopting the institutional_extraction_reading or demographic_reproduction_reading change the beneficiary/victim structure?',
    'Cross-reading comparison of stakeholder directionality and extraction flows; the sibling readings reallocate victimhood to contingent labor and demographic out-groups.',
    'If the kernel is better captured by sibling readings, this reading''s low-epsilon characterization of faculty is incorrect and the constraint is more extractive overall.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_boundary, conceptual, 'Committer uncertainty about whether this reading or its siblings better fit the tenure kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__academic_freedom_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__academic_freedom_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__academic_freedom_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__academic_freedom_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__academic_freedom_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__academic_freedom_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__academic_freedom_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__academic_freedom_reading, base_extractiveness, 32, 0.53).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__academic_freedom_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__academic_freedom_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__academic_freedom_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__academic_freedom_reading, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel decomposes into three structurally distinct constraints: academic_freedom_reading (coordination via truth-seeking protection), institutional_extraction_reading (rent extraction by early winners), and demographic_reproduction_reading (demographic gatekeeping). Each has distinct epsilon, beneficiaries, and victims. This reading models tenure as a coordination mechanism with asymmetric extraction directed at political oversight actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
