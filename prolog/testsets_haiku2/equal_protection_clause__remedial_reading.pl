% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection — Remedial Reading: Race-Conscious Remediation Mandate
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause interprets it to
 *   require (not merely permit) race-conscious governmental action to
 *   remediate historical group subordination. Under this reading, substantive
 *   equality — not formal identity of treatment — is the substantive
 *   guarantee. This generates a specific constraint: institutions must
 *   actively remedy effects of slavery, segregation, and ongoing
 *   discrimination through preferential policies. Historically marginalized
 *   racial minorities are the beneficiaries; individual members of
 *   non-preferred groups bear the cost. The claim is SCAFFOLD because the
 *   reading's own logic includes a sunset: once substantive equality is
 *   achieved, race-conscious remediation becomes unnecessary and should
 *   cease. The authored metrics (extractiveness 0.68, suppression 0.52)
 *   describe the constraint's actual operation during active remediation — a
 *   substantial burden placed on one group to benefit another. The
 *   theater_ratio rises over time (cognitive capture: remediation's
 *   justificatory frame hardens into institutional routine regardless of
 *   whether the founding problem remains live) and the
 *   suppression_requirement rises as legal challenges intensify. The metrics
 *   are authored independently of the claim; they describe operation, not
 *   vindication.
 *
 * KEY AGENTS:
 *   - historically_marginalized_racial_minorities — beneficiaries; group membership identity-locks the benefit
 *   - individual_members_non_preferred_groups — victims; bear admissions/hiring burden
 *   - educational_institutions — agenda-setters; design and defend remedial policies
 *   - federal_judiciary — agenda-setters; certify remediation legitimacy and manage sunset conditions
 *   - civil_rights_advocates — organized beneficiary proxies; advance the reading in courts
 *   - originalist interpreters — excluded; reject the premise of permissible racial classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.68).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.52).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection — Remedial Reading: Race-Conscious Remediation Mandate").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '85ea27d7-1d75-44ca-8909-2019c1fa32af').
narrative_ontology:cs_kernel_codification('85ea27d7-1d75-44ca-8909-2019c1fa32af', fixed_text).
narrative_ontology:cs_authority_grounding('85ea27d7-1d75-44ca-8909-2019c1fa32af', lineage).
narrative_ontology:cs_interpretation_layer_present('85ea27d7-1d75-44ca-8909-2019c1fa32af').
narrative_ontology:cs_reading_relation('85ea27d7-1d75-44ca-8909-2019c1fa32af', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('85ea27d7-1d75-44ca-8909-2019c1fa32af', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('85ea27d7-1d75-44ca-8909-2019c1fa32af', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('85ea27d7-1d75-44ca-8909-2019c1fa32af', substantive_equality_mandate, deontological).
narrative_ontology:cs_axiom('85ea27d7-1d75-44ca-8909-2019c1fa32af', foundational, historical_group_subordination_remediable).
narrative_ontology:cs_axiom_status(historical_group_subordination_remediable, holdable).
narrative_ontology:cs_axiom_grounding('85ea27d7-1d75-44ca-8909-2019c1fa32af', historical_group_subordination_remediable, empirically_contingent).
narrative_ontology:cs_reference_frame('85ea27d7-1d75-44ca-8909-2019c1fa32af', reconstruction_era_remedial_commitment).
narrative_ontology:cs_drift_state('85ea27d7-1d75-44ca-8909-2019c1fa32af', contemporary_colorblind_legal_ascendency, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('85ea27d7-1d75-44ca-8909-2019c1fa32af', '2026-06-12T14:23:15Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_racial_minorities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, descendant_beneficiary_communities).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_members_non_preferred_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, administrative_bodies).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, structural_remediation_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose members were subject to systematic subordination through slavery, Jim Crow segregation, housing discrimination, and institutional exclusion. The remedial reading locates them as the primary beneficiaries of race-conscious remediation — the policy exists to repair cumulative group disadvantage and restore substantive equality. Exit from group membership is not available; the benefit accrues to the group as a collective entity and to individual members by virtue of membership.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_marginalized_racial_minorities, beneficiary,
    organized, generational, identity_locked, national).

% Individuals from groups not designated as historically marginalized (typically white applicants, sometimes Asian American applicants in specific institutional contexts) who encounter race-conscious admissions or hiring policies. Under the remedial reading, they bear a burden — reduced admissions/hiring probability, or denial of individual-merit-based advancement — in service of group remediation. They are classified as victims of the remediation mandate because the constraint imposes costs on them individually in order to benefit another group. Their exit options are constrained: they cannot change their racial group membership or the historical fact that generated the remediation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individual_members_non_preferred_groups, payer,
    powerful, biographical, constrained, national).

% Universities, professional schools, and public employers that adopt and administer race-conscious admissions and hiring policies. Under the remedial reading, they are the operators of the remediation mechanism — they set the preferences, adjust them over time, and defend their legitimacy by reference to the historical subordination the policy addresses. They bear administrative burden and legal vulnerability, but also capture the narrative authority to define remediation targets and timelines.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Courts that interpret and enforce the Equal Protection Clause. Under the remedial reading, the judiciary authorizes and constrains the remediation mandate — they must certify that remediation is narrowly tailored, that the historical subordination is real and ongoing, and that the remedy will eventually sunset. They adjudicate disputes over whether remediation remains justified and manage the transition toward substantive equality.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, federal_judiciary, observer).

% Organizations and litigants advancing the remedial reading in courts and legislatures. They frame the constraint as the fulfillment of equal protection — the mandate that the state must actively repair the effects of its own (and society's) subordination. They do not directly collect from the remediation policy but benefit from its vindication in law and from institutional recognition of group-based historical injustice.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% State and federal legislatures that may authorize, fund, or restrict race-conscious remediation. Under the remedial reading, they set the conditions for remediation by law — defining eligible groups, duration, and scope. They face pressure from civil rights advocates and resistance from opponents of the reading; their authority is structurally constrained by the judiciary's interpretation of the Constitution.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, legislative_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Jurists and scholars advancing the colorblind reading who hold that the Fourteenth Amendment forbids racial classifications regardless of remedial intent. They are excluded from the conversation that the remedial reading conducts — not in the room when the legitimacy of group-based remediation is established. Their exclusion is structural: they reject the premise that the state may permissibly classify by race, which is the core commitment of the remedial reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, originalist_constitutional_interpreters, excluded,
    institutional, generational, trapped, national).

% Admissions offices, HR departments, and compliance officers who operationalize remedial policies. They bear the cost of designing, defending, and adjusting policies in response to litigation and political pressure. Their exit options are constrained by law: they must implement the policies their institutions adopt, and those adoptions are constrained by the remedial reading's interpretation of equal protection.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, administrative_bodies, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, historically_marginalized_racial_minorities).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the distribution of educational and employment opportunity in a way that accounts for historical group subordination — solving the problem of how a polity that was structured around racial hierarchy can transition toward genuine equality. The remedial reading frames this as a coordination problem: without coordinated remediation, historical disadvantage persists through compound disadvantage, and formal equality masks substantive inequality.
% TRANSFER_FUNCTION: Transfers opportunity (admissions slots, hiring positions, contractual preferences) from individual members of non-preferred groups to members of historically marginalized groups. The transfer is justified as repairing a prior transfer in the opposite direction — historical confiscation of opportunity from marginalized groups and reservation of it for preferred groups. Under the remedial reading, the current transfer is temporary and purpose-limited: it persists only until substantive equality is achieved.
% ABSENT_VOICES: Originalist interpreters of the Fourteenth Amendment (holding the colorblind reading) are structurally excluded — they reject the premise that race-conscious remediation is constitutionally permissible. Libertarian skeptics of remediation on grounds of individual liberty are also absent from the primary conversation. So too are prospective members of future generations: the remedial reading's assumption is that remediation will eventually be complete and unnecessary, but it does not include voices from that future state assessing whether the burden on current non-preferred individuals was correctly calibrated to the benefit to remedied groups.
% DISAPPEARANCE_RATIONALE: If the remedial reading of equal protection disappeared and were replaced by the colorblind reading (race-conscious remediation forbidden), admissions and hiring would shift to purely individualistic metrics; groups that benefited from remediation would see reduced representation in elite institutions and professions; disparities in opportunity would widen again absent other interventions; and the legal landscape of civil rights remedies would contract dramatically. The institutional architecture of remediation is built around this reading's legitimacy.
% FOUNDING_PROBLEM: The Fourteenth Amendment's Equal Protection Clause must address a polity structured by slavery and Jim Crow segregation, producing cumulative group disadvantage. Formal equality (treating individuals identically regardless of race) perpetuates substantive inequality when historical subordination created unequal starting conditions. The founding problem is: how can equal protection mean anything other than active remediation of the effects of prior subordination?
% FOUNDING_PROBLEM_CORROBORATION: Civil rights scholars, historical commissions, and institutional policymakers attest that historical subordination created persistent group disadvantage requiring active remediation. The U.S. Commission on Civil Rights, academic analyses in sociology and economics, and testimony from institutions that adopted remedial policies corroborate the founding problem. The colorblind reading contests this: it argues that the founding problem is misdiagnosed — that equal protection forbids the state from classifying by race for ANY purpose, even remediation. This contest is deep and unresolved.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the remedial reading mandates a continuous transfer of opportunity: admissions slots and hiring positions that would go to individual applicants from non-preferred groups instead go to members of remedied groups. This is not incidental cost but the policy's core function. Suppression (0.52) is moderate rather than extreme because the constraint operates through law and institutional policy, not through external coercion — opposition is expressed through legal challenge and political speech, not silenced. However, suppression has a threshold: colorblind-reading advocates cannot operate within institutions committed to the remedial reading; they are excluded from the conversation that defines the remediation mandate. Theater_ratio rises from 0.12 to 0.28 because, over time, the remediation policy develops institutional routines, compliance bureaucracies, and public justificatory narratives that persist even as institutional commitment to the founding problem may wane. At t=60 (end of interval, post-sunset), extractiveness and theater_ratio drop as we project forward to a scenario where substantive equality has been achieved and remediation is no longer mandated. Accessibility_collapse (0.71) reflects that once the remedial reading is institutionalized, alternatives (colorblind approach, diversity-only approach) become organizationally difficult to access for actors within the remediation framework; resistance (0.74) reflects strong ongoing opposition from colorblind and libertarian critics.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary seats (historically marginalized groups, civil rights advocates) and the agenda-setter judicial seat should compute the constraint as foundational — remediation as the substance of equal protection, not a deviation from it. Victim seats (individual members of non-preferred groups) should compute it as extractive, because the constraint imposes identifiable costs on individuals to serve group remediation. The judicial agenda-setter occupies a third position: it must authorize remediation while managing the sunset — from the judiciary's seat, the constraint is legitimate only insofar as it is temporary and condition-specific. These are structural asymmetries, not differences of opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary direction (d near 0.0): historically marginalized groups collect opportunity transfers and identity-locked membership makes exit impossible. They accrue direct benefit from remediation. Civil rights advocates are organized and mobile (d around 0.25) — they benefit from the reading's vindication but have more exit options. Victim direction (d near 1.0): individual members of non-preferred groups pay through reduced admissions/hiring probability and identity-bound group membership (non-preferred group membership is identity-locked in the reverse sense: they cannot claim preferred-group identity). They bear specific, individualized cost. Agenda-setter direction (d around 0.5–0.6): institutions operate under legal constraint; they are neither pure beneficiaries (they face ongoing litigation, political pressure, and compliance burden) nor pure targets (they also benefit from the remediation framework's legitimacy and from federal/state funding tied to remediation adoption). The judiciary's d is ambiguous (analytical, around 0.5) — they are neither collecting from the remediation nor bearing its cost; they are the institution that certifies its legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents false mandatrophy by being explicitly framed as temporary (scaffold with sunset clause). The remedial reading's internal logic includes its own termination condition: once substantive equality is achieved, the mandate to remediate ends. This avoids the mandatrophy trap where a remedy persists after its founding problem is resolved. However, there is an omega-level question here: when is substantive equality 'achieved'? If institutional remediators cannot agree on the sunset condition, the mandate can persist long after its justification dissolves. The theater_ratio measurement series (rising from 0.12 to 0.28) models this risk: routine, justification-by-rote, and institutional inertia may sustain the remediation policy past the point where the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_equality_definition,
    'What counts as ''substantive equality'' — the sunset condition for the remediation mandate? Is it parity in representation, parity in outcomes, parity in opportunity access, or something else?',
    'Judicial standard-setting or legislative definition. Courts must clarify what metric determines when remediation is complete (e.g., undergraduate representation at 12% for a racial group that is 12% of the national population).',
    'A narrow definition of substantive equality makes the sunset clause meaningful and the scaffold temporary. A vague or unobtainable definition turns the mandate into a de facto permanent extraction mechanism. If substantive equality is defined as outcome parity rather than opportunity parity, remediation may never be complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substantive_equality_definition, conceptual, 'The definition of the terminus condition determines whether the scaffold is genuinely temporary or disguised permanent extraction.').

omega_variable(
    knowledge_of_founding_injustice,
    'How long does the remedial obligation persist if the founding injustice (slavery, Jim Crow, housing discrimination) becomes temporally remote? Does the remedial mandate require ongoing knowledge and acknowledgment of historical subordination, or does it persist structurally regardless of whether current actors remember the history?',
    'Empirical: survey generational knowledge of historical subordination and institutional investment in maintaining historical literacy. Correlate with institutional commitment to remediation. Conceptual: debate whether remediation is about correcting a known historical wrong (time-bound) or about structural inequality that becomes self-sustaining (potentially time-unbounded).',
    'If remedial obligation requires active knowledge of the founding injustice, the reading is vulnerable to mandatrophy as historical memory fades and younger cohorts lack direct experience of segregation. If it is purely structural, the mandate persists regardless of historical awareness but risks becoming a pure-extraction mechanism detached from its justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_of_founding_injustice, empirical, 'Whether the mandate''s persistence depends on continuous institutional memory of historical subordination.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the remedial reading logically FORECLOSE the colorblind reading, or do they coexist as different coherent interpretations of the same text? Can a single constitutional framework hold both readings, or does adopting one require rejecting the other at the foundational level?',
    'Jurisprudential analysis: Examine whether the colorblind and remedial readings rest on contradictory foundational premises (colorblind''s assertion that race-consciousness is always illegitimate vs. remedial''s assertion that race-consciousness is sometimes mandated). If the premises are contradictory, they foreclose each other. If each premise is coherent on its own interpretation of the text, they coexist.',
    'If the readings foreclose each other, one must eventually prevail in law; a constitutional framework cannot hold both permanently. If they coexist, they will remain in competition across different institutional seats and judicial factions indefinitely. This affects whether the remedial reading is a stable long-term feature of equal protection law or a temporary ascendant position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the remedial and colorblind readings are logically incompatible or merely disagreed-upon alternatives.').

omega_variable(
    suppression_of_colorblind_advocacy,
    'Is the measured suppression (0.52) structural (colorblind advocates face institutional barriers due to the remedial reading''s dominance) or internalized (they have adopted the remedial frame''s premises and no longer articulate the colorblind position with full force)?',
    'Institutional audit: compare colorblind advocacy capacity and presence in institutions with active remedial policies vs. institutions without them. Post-policy trajectory: if colorblind advocacy resurges when remedial policy ends, suppression was structural. If it remains muted, suppression was partly internalized.',
    'If suppression is structural, it may be a temporary byproduct of remedial policy dominance and will ease if the reading''s institutional position weakens. If internalized, the constraint may carry forward attitudinal effects even after formal policy ends. Higher internalized suppression suggests deeper identity fusion with the remedial frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_colorblind_advocacy, empirical, 'Whether suppression of colorblind critique is structural or internalized within remediation-committed institutions.').

omega_variable(
    kernel_authentication,
    'This reading invokes the Fourteenth Amendment''s Equal Protection Clause as its kernel. Is the remedial reading a coherent interpretation of that specific text, or is it a reading that reads INTO the text commitments that must be supplied from contemporary political theory and social justice movements?',
    'Textual and originalist scholarship: examine the original public meaning of the Equal Protection Clause and whether remedial race-consciousness was intended or contemplated. Compare historical practice in Reconstruction and subsequent eras. Modern purposivist scholarship: assess whether the remedial reading follows from the Clause''s purpose (preventing subordination) even if not its original text.',
    'If the remedial reading is an authentic interpretation of the text, it has strong constitutional grounding. If it requires reading-in contemporary commitments, it may be vulnerable to constitutional challenge via text-and-originalism arguments. This affects the reading''s long-term stability and legitimacy across different interpretive communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_authentication, conceptual, 'Whether the remedial reading is textually grounded in the Equal Protection Clause or relies on reading-in contemporary values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(equa_tr_t0, projected).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__remedial_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__remedial_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_clause__remedial_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_clause__remedial_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t60, equal_protection_clause__remedial_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement_basis(equa_tr_t60, projected).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(equa_be_t0, projected).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__remedial_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__remedial_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_clause__remedial_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_clause__remedial_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t60, equal_protection_clause__remedial_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(equa_be_t60, projected).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(equa_su_t0, projected).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__remedial_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__remedial_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_clause__remedial_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_clause__remedial_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t60, equal_protection_clause__remedial_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(equa_su_t60, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The remedial reading is one of three structurally distinct interpretations of the Equal Protection Clause kernel. All three readings address the same constitutional text but instantiate different constraints with different ε values, beneficiary/victim structures, and classifications. The remedial reading treats race-conscious remediation as mandated (not optional or forbidden); it locates historically marginalized groups as beneficiaries and individual members of non-preferred groups as victims; it carries high extractiveness (0.68) because the mandate is strong and the transfer is specific. The colorblind reading forbids all racial classification (low ε, no extraction from preferred groups). The diversity reading permits racial consideration when it serves educational interests (moderate ε, distributed benefits). These are NOT the same constraint viewed from different angles — they have fundamentally different ε values and would classify differently at each seat. Network link captures the structural influence: the remedial reading's dominance constrains the colorblind reading's institutional voice and creates pressure on the diversity reading's scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
