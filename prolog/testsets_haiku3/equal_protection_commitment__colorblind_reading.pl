% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Color-Blind Equal Protection Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates Justice Harlan's color-blind reading of the
 *   Fourteenth Amendment's equal protection clause: the Constitution forbids
 *   any state use of racial classification, regardless of purpose or remedial
 *   intent. Under this reading, race-conscious admissions, hiring, and
 *   contracting programs constitute unconstitutional harm to individuals
 *   excluded on the basis of race. The reading is currently ascendant in U.S.
 *   constitutional law (post-Students for Fair Admissions v. Harvard, 2023)
 *   and is claimed as the authentic interpretation of the Constitution's
 *   text. This story models the constraint as the reading itself reads it: as
 *   a natural constitutional limit (color-blind principle) that emerges from
 *   constitutional text, not as a constructed allocation of benefit. The
 *   claim/metric gap reflects the tension: the color-blind reading asserts
 *   the principle is natural law (emerges_naturally: true); the authorized
 *   metrics show moderate-to-high extractiveness (0.42) and substantial
 *   resistance (0.71), indicating the reading's operation depends on active
 *   doctrinal maintenance and excludes competing readings. The engine will
 *   compute whether this mountain claim survives the metric structure.
 *
 * KEY AGENTS:
 *   - individuals_denied_race_conscious_programs: powerless, identity-locked victims of the state's racial classification
 *   - implementing_institutions: institutional agenda-setters executing race-conscious programs; the state actors the reading targets
 *   - beneficiaries_of_race_conscious_programs: moderate-power beneficiaries of current programs; positioned ambiguously under this reading as both access-winners and evidence of constitutional violation
 *   - historically_subordinated_groups: organized but excluded from the justification; the genealogy of subordination is systematically absent from the color-blind reading
 *   - constitutional_traditionalists: institutional beneficiaries of a text-anchored, historically-stable constitutional principle
 *   - supreme_court_majority: institutional agenda-setter; current enforcer of the color-blind reading through case law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.28).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Color-Blind Equal Protection Reading").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy").

domain_priors:emerges_naturally(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, 'c05b0a4c-6553-416f-a293-a448c507292a').
narrative_ontology:cs_kernel_codification('c05b0a4c-6553-416f-a293-a448c507292a', fixed_text).
narrative_ontology:cs_authority_grounding('c05b0a4c-6553-416f-a293-a448c507292a', lineage).
narrative_ontology:cs_interpretation_layer_present('c05b0a4c-6553-416f-a293-a448c507292a').
narrative_ontology:cs_reading_relation('c05b0a4c-6553-416f-a293-a448c507292a', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('c05b0a4c-6553-416f-a293-a448c507292a', equal_protection_commitment__diversity_reading, influences).
narrative_ontology:cs_axiom('c05b0a4c-6553-416f-a293-a448c507292a', foundational, racial_classification_intrinsically_invidious).
narrative_ontology:cs_axiom_status(racial_classification_intrinsically_invidious, holdable).
narrative_ontology:cs_axiom_grounding('c05b0a4c-6553-416f-a293-a448c507292a', racial_classification_intrinsically_invidious, deontological).
narrative_ontology:cs_axiom('c05b0a4c-6553-416f-a293-a448c507292a', foundational, constitution_mandates_state_color_blindness).
narrative_ontology:cs_axiom_status(constitution_mandates_state_color_blindness, holdable).
narrative_ontology:cs_axiom_grounding('c05b0a4c-6553-416f-a293-a448c507292a', constitution_mandates_state_color_blindness, conventional).
narrative_ontology:cs_reference_frame('c05b0a4c-6553-416f-a293-a448c507292a', text_based_colorblind_principle).
narrative_ontology:cs_drift_state('c05b0a4c-6553-416f-a293-a448c507292a', contemporary_identity_politics_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c05b0a4c-6553-416f-a293-a448c507292a', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, individuals_denied_race_conscious_programs).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, color_blind_constitutionalism_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, beneficiaries_of_race_conscious_programs).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, constitutional_traditionalists).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, individuals_denied_race_conscious_programs).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, beneficiaries_of_race_conscious_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applicants (typically Asian or white) denied admission to educational institutions or hiring under race-conscious affirmative action programs. Under this reading, they are the victims of unconstitutional racial classification — the state has harmed them by attending to race rather than treating all citizens as identical before law. Their exit is identity-locked: they cannot change race classification to access the program.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, individuals_denied_race_conscious_programs, payer,
    powerless, biographical, identity_locked, national).

% Universities, government agencies, and employers that operate race-conscious admissions or hiring under the remedial or diversity readings. Under the color-blind reading, they are the enforcers of an unconstitutional arrangement — they execute the racial classification the Constitution forbids. Their constraint is the obligation to administer programs whose very existence is classified as harmful under this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, implementing_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Historically underrepresented minorities admitted or hired under race-conscious programs. Under this reading, they are structured ambiguously: beneficiaries of educational/employment access, but also positioned as the evidence of the constitutional violation — their admission is framed not as justice but as the state's acknowledgment that race is a salient category. They cannot exit the racial classification that admits them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, beneficiaries_of_race_conscious_programs, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, beneficiaries_of_race_conscious_programs, payer).

% Descendants of enslaved, segregated, and systematically excluded populations. Under the color-blind reading, they are systematically absent from the justification for race-conscious programs — this reading treats the founding problem (slavery, Jim Crow, contemporary subordination) as either solved or irrelevant to equal protection's mandate. The reading forecloses the genealogy of subordination as a legitimate frame for remediation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, historically_subordinated_groups, excluded,
    organized, generational, constrained, national).

% Jurists, scholars, and political movements that vindicate the color-blind reading as the authentic meaning of equal protection and the Constitution's text. They benefit from this reading by securing a stable, text-anchored constitutional principle that requires no inquiry into history, subordination, or context — the rule is simple and derives from the Constitution's formal language.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, constitutional_traditionalists, beneficiary,
    institutional, generational, analytical, national).

% The current voting majority of the U.S. Supreme Court that has increasingly endorsed the color-blind reading and invalidated race-conscious programs. They interpret and enforce this reading through case law; their power is institutional (constitutional authority) but constrained by the competing readings held by dissenting justices and academic challenge.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Legislative body that could amend the Constitution or legislate remedies, but is formally excluded from interpreting equal protection's meaning under this reading (Marbury v. Madison doctrine). Congress holds the power to act but the Supreme Court holds interpretive authority; Congressional intent and remedial statutes are subject to judicial override.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, congress, excluded,
    institutional, generational, analytical, national).

% Electorate that selects judges and representatives who shape equal protection doctrine. They experience the reading through institutional action (admissions decisions, hiring, case outcomes) but do not directly interpret or enforce the constitutional text. Their power is remote and mediated through electoral institutions.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, voter_constituency, observer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, constitutional_traditionalists).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, race-neutral constitutional standard: the state treats all citizens identically before law regardless of race. This reading solves the coordination problem of providing a single, text-anchored interpretation of equal protection that requires no ongoing inquiry into historical fact or remedial justification.
% TRANSFER_FUNCTION: Moves educational and employment opportunity away from race-conscious remediation toward race-neutral selection. Under this reading, no material transfer occurs — rather, what is prevented is the state's allocation of a scarce resource (admission, hiring) based on race. The transfer is rhetorical/normative: away from acknowledging race in state action, toward formal color-blindness.
% ABSENT_VOICES: Descendants of historically enslaved and segregated populations are structurally absent from the equal protection discourse under this reading — the reading treats the founding problem (subordination) as either solved or irrelevant. Remedial and diversity readings would center that genealogy; the color-blind reading systematically excludes it. Congressional voices advocating statutory remediation are also absent from constitutional interpretation (per Marbury doctrine).
% DISAPPEARANCE_RATIONALE: The color-blind reading claims it rests on the Constitution's text, which would persist. If the color-blind *interpretation* disappeared (replaced by remedial or diversity readings), judicial authority would shift: race-conscious programs would become permissible, institutional practices would shift, and beneficiaries of such programs would gain access. The vanishing of the reading is not the vanishing of the Constitution but a change in how its text is read.
% FOUNDING_PROBLEM: The Constitution's equal protection clause forbids state action based on race because racial classification is inherently invidious; the state has no legitimate basis for sorting citizens by race.
% FOUNDING_PROBLEM_CORROBORATION: The color-blind reading is vindicated by constitutional textualists and originalists (e.g., Justice Harlan, Justice Thomas, contemporary originalist scholars) who cite the text itself. Remedial and diversity readings contest this founding problem, offering instead: 'the founding problem is slavery and its aftermath; equal protection must permit dismantling subordination.' No neutral external arbiter corroborates the founding problem — the dispute is fundamentally about what equal protection's text commits the nation to, which is itself the unsettled constitutional question.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_commitment__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) measures the harm to individuals excluded from race-conscious programs under this reading's logic: they suffer state action based on race, which is classified as invidious regardless of remedial intent. It is moderate-to-high because the harm is real within the reading's frame (identity-locked targets, constrained alternatives), but the reading claims the harm is not 'extraction' in the sense of benefit capture — rather, harm from constitutional violation. Suppression (0.28) is moderate because the reading must suppress competing interpretations (remedial and diversity readings) to maintain itself as the authoritative reading, yet those readings persist in academic writing, dissenting opinions, and legislative advocacy. Theater (0.15) is low because the color-blind principle is stated simply and can be applied mechanistically; yet some performative work is required to maintain the principle's innocence — the reading must treat the historical record of slavery, segregation, and ongoing subordination as irrelevant to equal protection's meaning, which requires ongoing rhetorical suppression of historical context. Accessibility collapse (0.62) reflects that once an institution adopts the color-blind reading, race-conscious remediation becomes legally unavailable — the reading's adoption does collapse alternatives for institutions bound by it. Resistance (0.71) is high because remedial and diversity readings remain live and actively contested; the color-blind reading has never achieved consensus and faces substantial academic, judicial, and constituency resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute radically different constraint types from identical structural data. From the seat of an individual denied admission based on race, the color-blind reading operates as a mountain: it is a natural, text-based constitutional limit that any just legal system must enforce. From the seat of an institutional administrator operating a race-conscious program, the same reading operates as a snare: it forecloses alternatives (remediation is illegal), requires active suppression of contrary interpretations (dissenting opinions must be overruled), and persists through institutional enforcement (courts must police compliance). From the seat of a descendant of slavery, the reading operates as a false summit: it claims naturalness (the Constitution's color-blind text) but benefits identifiable parties (constitutional traditionalists, whites and Asians advantaged by race-neutral selection) and systematically forecloses the genealogy that would justify remediation. The engine's per-seat computation will register these divergences; the narrative here documents where they arise.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality asymmetry is stark. Individuals denied race-conscious programs compute as targets (d near 1.0): they are identity-locked to racial classification and bear the material cost (denial of opportunity) under this reading. Implementing institutions compute as moderate targets (d around 0.5–0.6): they are constrained to comply with the reading but are not beneficiaries — they are instead the law's subjects, required to abandon race-conscious practices. The constitutional traditionalists and Supreme Court majority compute as beneficiaries (d near 0.0–0.2): they benefit from the reading's stability and interpretive authority without bearing material extraction costs. Historically subordinated groups compute as excluded (not seated), which means they have no d value — the reading does not structure their extraction, it structures their absence from the justification. The diversity and remedial readings would flip many of these directionalities: they would make historically subordinated groups the primary targets (for whom the reading provides no remedy) and would reframe institutional race-conscious programs as beneficent coordination rather than constitutional violation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing invidious state action based on race) is contested. The color-blind reading asserts it is still live: race-conscious state programs still sort citizens by race, and equal protection still forbids that. The remedial reading contests that the founding problem is degraded: slavery and Jim Crow are past; what now requires remedy is their ongoing subordination effects, not race-consciousness itself. The diversity reading asserts the founding problem has shifted: the problem is now educational segregation and the loss of diversity, not racial sorting per se. The color-blind reading allows no space for these contests — it claims the Constitution's text resolves the matter in favor of color-blindness, period. This is the mandatrophy structure: the reading's original justification (preventing invidious discrimination by enslaved-state authority) has been superseded by new and competing founding problems (remedying subordination, achieving diversity), but the reading's doctrine persists — not because it continues to solve the original problem, but because it has been elevated to a constitutional principle claimed as natural law. The metrics confirm this: resistance is high (0.71), indicating the reading is not voluntarily maintained; theater is present (0.15), indicating some performative work sustains it; and suppression is required (0.28), indicating competing interpretations must be actively foreclosed. A pure rope would have low suppression and low theater. This constraint is architecturally closer to a false summit: claimed as natural (mountain), but dependent on beneficiaries and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_naturality_vs_constructed_reading,
    'Does the color-blind principle emerge naturally from the Constitution''s text, or is it a reading chosen by interpreters on the basis of contemporary values?',
    'Historical analysis of the Fourteenth Amendment''s drafting, legislative record, and contemporaneous interpretation; comparison with how the same text supported different readings (Plessy''s separate-but-equal reading in 1896 also claimed to be natural reading of equal protection).',
    'If the principle emerges naturally, the constraint is a mountain and resistance/suppression metrics are measurement noise. If it is a chosen reading, the constraint is a false summit and the beneficiary set (constitutional traditionalists, those advantaged by race-neutral selection) is the true agent sustaining it; metrics confirm the false-summit diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_naturality_vs_constructed_reading, conceptual, 'Whether color-blindness is natural constitutional law or a particular reading with identifiable beneficiaries.').

omega_variable(
    race_consciousness_harm_vs_colorblind_erasure,
    'Does the harm to individuals excluded from race-conscious programs outweigh the harm from color-blind erasure of ongoing subordination?',
    'Empirical study of educational/employment outcomes under color-blind vs. race-conscious regimes; qualitative testimony from both excluded individuals and members of historically subordinated groups; counterfactual analysis of what remediation would require.',
    'If excluded-individual harm is greater, the reading is justified; if subordination-perpetuation harm is greater, the reading itself becomes extractive (its operation preserves caste structures). Classification of the constraint as snare vs. mountain depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(race_consciousness_harm_vs_colorblind_erasure, preference, 'Incommensurable harm comparison: cost to excluded vs. cost to subordinated.').

omega_variable(
    genealogy_suppression_mechanism,
    'Is the color-blind reading''s systematic exclusion of slavery/segregation history from its equal protection argument a structural property of the reading, or an evidentiary failure remediable through better legal argument?',
    'Examination of constitutional doctrine: can a color-blind interpreter acknowledge slavery''s history and still sustain the reading''s conclusion? Analysis of whether genealogy-blindness is necessary to the reading or incidental.',
    'If genealogy-suppression is necessary, the reading operates through epistemic closure — it forecloses the historical record as irrelevant to constitutional meaning — which is a form of suppression. If incidental, the metric can be lowered and the reading''s naturalness is less compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genealogy_suppression_mechanism, conceptual, 'Whether history-blindness is intrinsic to the color-blind reading or a contingent interpretive choice.').

omega_variable(
    remedial_alternative_availability,
    'If the color-blind reading were replaced by the remedial reading, would implementing institutions retain meaningful alternatives, or would they be bound to race-conscious compliance?',
    'Analysis of remedial-reading doctrine: does it mandate race-consciousness, permit it, or require evidence-based choice? Examination of post-Gratz/Grutter institutional behavior.',
    'If remedial reading binds institutions equally tightly (just in the opposite direction), the exit_options for implementing institutions remain ''constrained'' under either reading, and the real cost of the reading-switch is paid by excluded individuals. If remedial reading permits institutional discretion, it offers more exit to institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_alternative_availability, empirical, 'Whether the remedial reading constrains institutions as much as color-blind reading does, or offers more institutional discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1883, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1883, equal_protection_commitment__colorblind_reading, theater_ratio, 1883, 0.08).
narrative_ontology:measurement_basis(equa_tr_t1883, observed).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__colorblind_reading, theater_ratio, 1954, 0.11).
narrative_ontology:measurement_basis(equa_tr_t1954, observed).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.13).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement_basis(equa_tr_t2003, observed).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__colorblind_reading, theater_ratio, 2023, 0.15).
narrative_ontology:measurement_basis(equa_tr_t2023, observed).
narrative_ontology:measurement(equa_tr_t2026, equal_protection_commitment__colorblind_reading, theater_ratio, 2026, 0.15).
narrative_ontology:measurement_basis(equa_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1883, equal_protection_commitment__colorblind_reading, base_extractiveness, 1883, 0.15).
narrative_ontology:measurement_basis(equa_be_t1883, observed).
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__colorblind_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement_basis(equa_be_t1954, observed).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.41).
narrative_ontology:measurement_basis(equa_be_t2003, observed).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__colorblind_reading, base_extractiveness, 2023, 0.42).
narrative_ontology:measurement_basis(equa_be_t2023, observed).
narrative_ontology:measurement(equa_be_t2026, equal_protection_commitment__colorblind_reading, base_extractiveness, 2026, 0.42).
narrative_ontology:measurement_basis(equa_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1883, equal_protection_commitment__colorblind_reading, suppression_requirement, 1883, 0.12).
narrative_ontology:measurement_basis(equa_su_t1883, observed).
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__colorblind_reading, suppression_requirement, 1954, 0.18).
narrative_ontology:measurement_basis(equa_su_t1954, observed).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.24).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.27).
narrative_ontology:measurement_basis(equa_su_t2003, observed).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__colorblind_reading, suppression_requirement, 2023, 0.28).
narrative_ontology:measurement_basis(equa_su_t2023, observed).
narrative_ontology:measurement(equa_su_t2026, equal_protection_commitment__colorblind_reading, suppression_requirement, 2026, 0.28).
narrative_ontology:measurement_basis(equa_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__colorblind_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel is instantiated by three constraint stories, each representing a distinct reading of the Fourteenth Amendment's equal protection clause. All three readings compete for authoritative interpretation of the same constitutional text. The colorblind_reading (this file) forecloses race-consciousness; the remedial_reading forecloses color-blindness when it perpetuates subordination; the diversity_reading occupies middle ground. These are not three perspectives on one constraint; they are three structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications. The network links show structural influence: adoption of the colorblind reading constrains the remedial reading's legal availability and shapes the diversity reading's justificatory strategy. Each story carries its own cs_structure.reading_relations array documenting the relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__colorblind_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
