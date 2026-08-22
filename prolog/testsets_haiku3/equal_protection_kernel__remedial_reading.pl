% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Race-Conscious Affirmative Action
 *   domain: constitutional_law/civil_rights/education
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause construes the
 *   constitutional text to permit state action that considers race when
 *   narrowly tailored to remedy documented historical exclusion or achieve
 *   compelling educational diversity. Under this reading, universities may
 *   employ race-conscious admissions criteria; historically excluded racial
 *   groups become beneficiaries of affirmative consideration; rejected
 *   applicants who would have been admitted under race-blind processes bear a
 *   distributional cost; and the state incurs an obligation to document the
 *   remedial or diversity purpose and monitor narrow tailoring. This is ONE
 *   reading of a contested constitutional kernel (the Equal Protection Clause
 *   itself). It coexists with the colorblind reading (no racial
 *   classifications ever permitted) and influences the antisubordination
 *   reading (focus on hierarchy rather than classification per se). The
 *   claim-metric gap is structural and deliberate: the remedial reading is
 *   CLAIMED as a tangled-rope arrangement (genuine coordination of remedial
 *   purpose + historical diversity benefit, genuine beneficiary groups) while
 *   the authored metrics reflect moderate extraction (asymmetric benefit to
 *   excluded groups, reduced admission slots for majority applicants) and
 *   modest suppression (the reading requires active enforcement via judicial
 *   review and institutional commitment to overcome political opposition).
 *   The engine's per-seat computation will show how the constraint operates
 *   differently from majority and excluded perspectives.
 *
 * KEY AGENTS:
 *   - Historically excluded racial groups: Primary beneficiaries of affirmative consideration and corrective admission criteria; experience the reading as expansive of opportunity.
 *   - Universities: Agenda-setters and operational beneficiaries; authorized to conduct race-conscious admissions and achieve diversity; maintain institutional autonomy but face narrow-tailoring scrutiny.
 *   - Rejected majority-race applicants: Payers bearing the distributional cost of lost admission slots; modestly mobile (can attend alternative institutions or challenge via litigation).
 *   - State legislatures and courts: Institutional agenda-setters enforcing narrow-tailoring doctrine and managing the boundary between permissible remedial action and impermissible classification.
 *   - Colorblind-reading adherents: Excluded from this reading's legitimacy framework; contest the core premise and advocate for absolute color-blindness.
 *   - Civil rights organizations: Observers supporting and defending the reading through advocacy and litigation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.31).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.24).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Remedial Reading: Race-Conscious Affirmative Action").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/civil_rights/education").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '8e22c0ef-bc6d-432e-8f27-ada9fed5a48c').
narrative_ontology:cs_kernel_codification('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', fixed_text).
narrative_ontology:cs_authority_grounding('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', lineage).
narrative_ontology:cs_interpretation_layer_present('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c').
narrative_ontology:cs_reading_relation('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', foundational, race_consciousness_justified_by_remedial_purpose).
narrative_ontology:cs_axiom_status(race_consciousness_justified_by_remedial_purpose, holdable).
narrative_ontology:cs_axiom_grounding('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', race_consciousness_justified_by_remedial_purpose, deontological).
narrative_ontology:cs_axiom('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', foundational, historical_exclusion_grounds_corrective_action).
narrative_ontology:cs_axiom_status(historical_exclusion_grounds_corrective_action, holdable).
narrative_ontology:cs_axiom_grounding('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', historical_exclusion_grounds_corrective_action, deontological).
narrative_ontology:cs_reference_frame('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', equal_protection_permits_remedial_differentiation).
narrative_ontology:cs_drift_state('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', contemporary_conservative_court_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8e22c0ef-bc6d-432e-8f27-ada9fed5a48c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_achieving_diversity).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants_from_majority_race).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, equal_protection_permits_remedial_race_consciousness).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, historical_subordination_justifies_corrective_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups with documented histories of educational exclusion and systemic discrimination gain affirmative consideration in university admissions under this reading. The benefit is substantive access to elite educational institutions previously closed to their ancestors, with compounding lifetime effects on wealth, social mobility, and professional networks. Exit from the constraint means losing the affirmative consideration; the underlying exclusion that made the remedy necessary persists.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Universities gain legitimacy, institutional prestige, and educational mission enhancement by assembling racially diverse student bodies under this reading. They are authorized to conduct race-conscious admissions in furtherance of a judicially recognized compelling state interest (educational diversity). Operationally, they maintain significant autonomy in defining and pursuing diversity metrics and can exit by switching to race-blind processes (though at reputational and diversity-outcome cost).
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_achieving_diversity, beneficiary,
    institutional, generational, arbitrage, national).

% Applicants from groups not identified as historically excluded face reduced admission probability when universities employ race-conscious admissions. Under this reading, their individual rejection can be justified by the remedial purpose even when their credentials exceed admitted candidates from historically excluded groups. Their exit options include applying to other institutions, pursuing alternative credentials, or challenging the practice via litigation—options available but costly in time and resources.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants_from_majority_race, payer,
    moderate, biographical, mobile, national).

% Under this reading, state institutions bear the authority and obligation to document historical exclusion, define the remedial or diversity purpose, and police the narrow tailoring of race-conscious measures. They enforce the constraint through judicial review, legislative authorization, and institutional policy. They manage the tension between competing individual and group-level constitutional values.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Parties committed to an absolute color-blind interpretation of equal protection—that the Constitution forbids all racial classifications regardless of purpose—are structurally excluded from this reading's legitimacy framework. They contest the reading's foundational axioms and would argue for race-blind admissions and invalidation of affirmative action plans. They cannot participate in defending the remedial reading from within its own framework; their participation requires rejecting its core premise.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, colorblind_reading_adherents, excluded,
    institutional, generational, constrained, national).

% Organizations advancing civil rights monitor and defend race-conscious remedial measures through litigation, advocacy, and expert testimony. They observe and support the reading's operation, providing genealogy and evidence of historical exclusion and documenting diversity benefits. They carry no formal institutional role but influence the reading's legitimacy and persistence through public argument and legal action.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_organizations, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__remedial_reading, universities_achieving_diversity).
narrative_ontology:fixing_cost_class(equal_protection_kernel__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the permissibility of race-conscious state action with the constitutional command of equal protection by constructing a middle path: it permits remedial race consciousness (addressing documented historical harm) without endorsing race as a categorical entitlement. The coordination problem is structural: how to honor historical subordination without ossifying racial categories forever, and how to achieve diversity benefits without abandoning equal protection altogether.
% TRANSFER_FUNCTION: Moves admission slots and social mobility pathways from majority-race applicants to historically excluded-group applicants when universities employ race-conscious selection. The transfer is mediated through university admissions decisions and justified as the mechanism for achieving the remedy and diversity benefit. It is asymmetric: majority applicants bear the cost of reduced admission probability while excluded groups receive the benefit of affirmative consideration.
% ABSENT_VOICES: Rejected applicants from majority races who lack legal standing to mount sophisticated constitutional challenges; populations in non-selective institutions or outside the university admissions system entirely who experience different distributions of benefit and burden; members of historically excluded groups who oppose race-conscious remedies on principle. Colorblind-reading adherents (institutional actors in courts and legislatures) are partially excluded from this reading's legitimacy framework—they contest the premises rather than sitting outside it.
% DISAPPEARANCE_RATIONALE: If this reading's authorization for race-conscious admissions vanished and institutions were forced to adopt race-blind processes, university diversity composition would shift measurably (reduced representation of historically excluded groups), institutional prestige hierarchies would reorganize (those committed to diversity would compete differently), litigation incentives would change, and the political and educational landscape would realign around new selection criteria. The remedial reading is not a natural-law constraint; its disappearance would rearrange institutional practice and outcomes.
% FOUNDING_PROBLEM: American educational institutions practiced explicit racial exclusion and discrimination for centuries; universities systematically denied admission to qualified applicants from historically oppressed racial groups. Even after formal discrimination was prohibited, structural barriers and historical underinvestment in predominantly minority communities perpetuated educational access disparities. The remedial reading was constructed to permit targeted corrective action addressing documented historical harm while maintaining a constitutional equal-protection framework.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the beneficiary institutions document the founding problem with extensive evidence (Derrick Bell, Michelle Alexander, Thomas Sugrue, Harvard Civil Rights-Civil Liberties Law Review scholarship). Universities and civil rights organizations assert the problem persists in attenuated form. The colorblind-reading camp contests whether the current problem is severe enough to justify race-conscious remedies, and whether the remedies are proportionate to documented harm. The founding problem—historical exclusion—is uncontested; the remedial reading's claim is that it justifies present-day race-conscious action, which is contested.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).
:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31 terminal) because the constraint operates as a mixed beneficiary-payer transfer: historically excluded groups receive substantive mobility gains while majority applicants face reduced admission probability, but the transfer is justified by remedial purpose, not pure rent collection. The asymmetry persists (beneficiaries and payers remain distinct), but the justification framework (remedying historical harm) lowers extractiveness from what a pure zero-sum redistribution would show. Suppression is low (0.24) because the reading relies on persuasion and judicial legitimacy rather than coercion—institutional commitment to the reading depends on internalized constitutional interpretation, not force. Theater is modest (0.18) because the diversity mission is substantive (universities genuinely value and pursue diversity) though some institutional practice is performative (symbolic diversity commitments without resource backing). Accessibility_collapse is moderate (0.42) because alternatives persist: majority applicants can attend other institutions, challenge the practice in court, or advocate for color-blind policy—the reading does not collapse all alternatives, though selective-institution diversity does create path-dependent disadvantage. Resistance is substantial (0.68) because the reading faces significant institutional and political opposition: colorblind-reading adherents, majoritarian political coalitions, and recent Court majority doctrine all resist the remedial reading, generating continuous justificatory labor and litigation pressure. The measurement series shows extractiveness rising from 0.18 to ~0.31 as universities operationalize the reading and institutional stakes increase, then stabilizing; suppression rises modestly as opposition hardens; theater remains modest and flat (the diversity mission does not become theatricalized in this interval). All three metrics share a single time grid (every metric authored at every time point: 0, 8, 16, 24, 32, 40, 50).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and payer seats will compute dramatically different type classifications. From the perspective of historically excluded groups, the remedial reading appears as a genuine rope (coordination that benefits them, no alternative exit, justified redistribution of opportunity). From the perspective of majority-race rejected applicants, the reading appears more extractive and constraining (asymmetric cost they bear, mobility option exists but is costly). From the institutional seat (universities), the reading appears as manageable tangled rope requiring narrow-tailoring enforcement but sustainable through legitimacy. The colorblind-reading constituency views the entire structure as impermissible and would compute it as enforced extraction dressed in remedial language. The engine computes each seat's directionality from the base properties and stakeholder structure; the divergence is the measurement the committer frame exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups show low directionality (d near 0.2–0.3, beneficiary end) because they benefit substantively from affirmative consideration, their exit is constrained (losing the affirmative benefit means accepting reduced opportunity), and the reading's beneficiary declaration makes them primary recipients. Rejected majority applicants show moderate-to-high directionality (d near 0.6–0.7, toward target) because they bear the distributional cost, have some exit options (other institutions, litigation, policy advocacy) but those options are costly, and they are declared as victims of the slot redistribution. Universities show low-to-moderate directionality (d near 0.25–0.35) as agenda-setters and secondary beneficiaries—they benefit from diversity and institutional prestige, they have arbitrage options (can shift to race-blind admissions or relocate to jurisdictions allowing race-conscious practices), and they are not victims of the arrangement. The reading's structural relationship to each seat is asymmetric: the beneficiary-victim split is constitutive. No directionality override is required; the structural derivation produces accurate per-seat values from the base properties and exit-option declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading avoids mandatrophy because it carries a live founding problem (documented historical exclusion) and a live functional purpose (achieving diversity and corrective opportunity access). However, a mandatrophy risk exists: if the reading persists primarily as institutional habit or symbolic commitment while actual remedial function attenuates (diversity numbers maintained through market changes rather than active affirmative measures, documented remedial purpose becomes pro forma rather than substantive), then the reading could degrade into a piton. The resistance measurement (0.68, substantially high) and theater measurement (0.18, modest) together suggest the reading is not yet theatricalized, but the omega variable on narrow-tailoring erosion addresses the trajectory risk. The founding-problem-status declaration (contested) reflects that colorblind-reading adherents deny the reading's premise and contend that contemporary educational access disparities do not justify race-conscious correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_tailoring_erosion,
    'Does the narrow-tailoring requirement remain a meaningful constraint on race-conscious admissions, or does it function primarily as ceremonial review while universities retain operational freedom?',
    'Comparative institutional analysis: court decisions striking down race-conscious programs as not narrowly tailored; institutional drift in documented remedial purposes; survey evidence of whether universities actively police their own narrow tailoring or treat it as compliance theater.',
    'If narrow tailoring becomes erosive (ceremonial rather than functional), the remedial reading degrades toward piton status: the architectural legitimacy (remedial purpose, narrow tailoring) persists but the enforced function (actual connection between race-conscious means and documented remedial end) atrophies, leaving mainly the extraction (slot redistribution) and institutional habit. A robust narrow-tailoring enforcement would maintain tangled-rope status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_erosion, empirical, 'Whether narrow-tailoring doctrine remains a live constraint on remedial race-consciousness or becomes ceremonial.').

omega_variable(
    foundational_remedial_purpose_contestation,
    'Is ''remedy for historical exclusion'' a sufficiently singular and defensible foundational normative claim, or does it occlude disagreement about causation, responsibility, and what counts as a relevant historical harm?',
    'Genealogy of historical-exclusion claims: whose history counts; whether contemporary populations are harmed by historical exclusion suffered by ancestors; whether present-day affirmative action remedies historical exclusion or redistributes present resources on contested bases. Legal and historical scholarship; court opinions analyzing remedial purposes.',
    'The remedial reading''s core axiom (race-consciousness permitted when remedying documented historical exclusion) depends on consensus about what historical harms ground present corrective action. If that consensus fragments—if different parties define ''historical exclusion'' and ''relevant population'' incompatibly—the reading loses its unifying force and devolves into a vehicle for interest-group contest. This would shift the reading''s classification from tangled_rope (asymmetric but justified) toward snare (zero-sum extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_remedial_purpose_contestation, conceptual, 'The constitutive ambiguity of ''documented historical exclusion'' as the reading''s legitimating reference.').

omega_variable(
    kernel_reading_instability,
    'Can the remedial reading coexist indefinitely with the colorblind reading, or does sustained contestation eventually force a constitutional resolution that favors one reading over the other, making the other structurally impossible within the same legal system?',
    'Constitutional history and doctrine evolution: Court decisions closing the remedial-reading space; legislative supermajority consensus shifting toward either colorblindness or antisubordination; formal constitutional amendment.',
    'The remedial reading currently coexists_with both siblings (no reading yet forecloses the others). But if the doctrinal struggle resolves in the Court''s favor toward absolute colorblindness (as recent majority opinions suggest), the remedial reading would shift from ''coexists_with'' to ''foreclosed_by'' the colorblind reading, and the constraint story itself would become an artifact of historical rather than live doctrine. This addresses whether the remedial reading is a stable constitutional equilibrium or a transitional phase in the kernel''s doctrinal evolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_instability, conceptual, 'The temporal stability of the remedial reading within a contested kernel under ongoing constitutional dispute.').

omega_variable(
    diversity_vs_remedial_purpose_divergence,
    'Are ''remedying historical exclusion'' and ''achieving educational diversity'' the same foundational purpose, or do they justify race-conscious action on different normative grounds that could diverge?',
    'Institutional and legal analysis: cases where remedial purpose has been met but diversity-outcome pressures continue; cases where diversity interests drive admissions decisions while remedial documentation is pro forma. Comparative study of institutions justifying race-consciousness on remedial vs. diversity grounds.',
    'If the two purposes diverge—if remedial purpose becomes attenuated while diversity pressure remains—the reading could fragment into remedial-reading and diversity-reading variants, each with different victim sets and extraction profiles. Diversity-only justification without remedial grounding would weaken the reading''s constitutional foundation and shift it toward snare (pure distribution-maximization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_vs_remedial_purpose_divergence, conceptual, 'Whether the remedial reading''s dual normative grounds (historical remedy and educational diversity) remain unified or diverge operationally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__remedial_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t8, equal_protection_kernel__remedial_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(equa_tr_t8, observed).
narrative_ontology:measurement(equa_tr_t16, equal_protection_kernel__remedial_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement_basis(equa_tr_t16, observed).
narrative_ontology:measurement(equa_tr_t24, equal_protection_kernel__remedial_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(equa_tr_t24, observed).
narrative_ontology:measurement(equa_tr_t32, equal_protection_kernel__remedial_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement_basis(equa_tr_t32, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_kernel__remedial_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t50, equal_protection_kernel__remedial_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(equa_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__remedial_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t8, equal_protection_kernel__remedial_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(equa_be_t8, observed).
narrative_ontology:measurement(equa_be_t16, equal_protection_kernel__remedial_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(equa_be_t16, observed).
narrative_ontology:measurement(equa_be_t24, equal_protection_kernel__remedial_reading, base_extractiveness, 24, 0.29).
narrative_ontology:measurement_basis(equa_be_t24, observed).
narrative_ontology:measurement(equa_be_t32, equal_protection_kernel__remedial_reading, base_extractiveness, 32, 0.3).
narrative_ontology:measurement_basis(equa_be_t32, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_kernel__remedial_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t50, equal_protection_kernel__remedial_reading, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(equa_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__remedial_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t8, equal_protection_kernel__remedial_reading, suppression_requirement, 8, 0.18).
narrative_ontology:measurement_basis(equa_su_t8, observed).
narrative_ontology:measurement(equa_su_t16, equal_protection_kernel__remedial_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement_basis(equa_su_t16, observed).
narrative_ontology:measurement(equa_su_t24, equal_protection_kernel__remedial_reading, suppression_requirement, 24, 0.22).
narrative_ontology:measurement_basis(equa_su_t24, observed).
narrative_ontology:measurement(equa_su_t32, equal_protection_kernel__remedial_reading, suppression_requirement, 32, 0.24).
narrative_ontology:measurement_basis(equa_su_t32, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_kernel__remedial_reading, suppression_requirement, 40, 0.24).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t50, equal_protection_kernel__remedial_reading, suppression_requirement, 50, 0.24).
narrative_ontology:measurement_basis(equa_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel decomposes into three structurally distinct constraint stories corresponding to three live constitutional readings. Each reading instantiates a different constraint with different beneficiary/victim sets, different ε values, and different types. The remedial_reading (this story) permits race-conscious state action when justified by remedial purpose or diversity interest; it coexists with colorblind_reading (no racial classifications ever permitted) and influences antisubordination_reading (focus on hierarchy rather than classification). The network links among the three stories model the constraint family as a kernel with multiple readings competing in doctrine and politics. Each story is authored with independent claim/metric pairs; the engine computes per-seat classifications for each reading, and the corpus enables analysis of how the same constitutional clause generates different constraint structures across different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__remedial_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
