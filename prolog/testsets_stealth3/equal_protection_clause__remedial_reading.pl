% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Remedial Reading — Mandated Race-Conscious Remediation of Historical Group Subordination
 *   domain: constitutional law/political philosophy/education policy
 *
 * SUMMARY:
 *   The remedial reading of the Fourteenth Amendment holds that equal
 *   protection REQUIRES race-conscious correction of historical group
 *   subordination until substantive equality is reached. Instantiated, it is
 *   the constitutional warrant beneath school-desegregation decrees,
 *   affirmative admissions, minority-contracting set-asides, and preferential
 *   employment measures. Beneficiaries are historically subordinated racial
 *   minorities as groups; victims are individual members of non-preferred
 *   groups who bear classification costs at concrete decision points. Per the
 *   epsilon-referent rule, extractiveness is authored for the STANDING
 *   arrangement under contest — the operating race-conscious remediation
 *   regime — assessed by this reading's own lights: the reading openly
 *   acknowledges that real, concentrated, biography-scale costs fall on
 *   dispreferred individuals and judges them justified by corrective debt;
 *   the metric records the magnitude of that imposition, not a verdict on its
 *   justice. This file is ONE reading of the equal_protection_clause kernel;
 *   the colorblind and diversity readings are separate constraints (separate
 *   files) linked through network.affects_constraints. Claim and metrics are
 *   independent: tangled_rope is claimed from structure (genuine coordination
 *   function + asymmetric extraction through the same preference machinery +
 *   indispensable active enforcement), while the metric values record
 *   descriptive operation — including an enforcement regime now in visible
 *   decay.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_minorities: primary beneficiary (organized/constrained) — receives allocated opportunities through race-conscious mechanisms; cannot exit without forfeiting the channel
 *   - members_of_non_preferred_racial_groups: primary target (moderate/constrained) — bears denied opportunities as classified individuals; no personal opt-out
 *   - federal_judiciary: agenda setter (institutional/constrained) — sustains, narrows, or repudiates the reading through interpretation; cannot exit the interpretive role
 *   - remedial_program_administrators: administering beneficiary (institutional/identity_locked) — runs the apparatus; careers and professional identity fused to its continuation
 *   - colorblind_coalition: excluded challenger (powerful/mobile) — holds the rival reading; operates through litigation and initiative; currently ascendant
 *   - constitutional_political_theorists: analytical observer (analytical/analytical) — audits the reading's coherence and outcomes from outside enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.68).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.4).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Remedial Reading — Mandated Race-Conscious Remediation of Historical Group Subordination").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional law/political philosophy/education policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '42ffa4f6-fd61-4830-bcaa-98a428677b0b').
narrative_ontology:cs_kernel_codification('42ffa4f6-fd61-4830-bcaa-98a428677b0b', fixed_text).
narrative_ontology:cs_authority_grounding('42ffa4f6-fd61-4830-bcaa-98a428677b0b', lineage).
narrative_ontology:cs_interpretation_layer_present('42ffa4f6-fd61-4830-bcaa-98a428677b0b').
narrative_ontology:cs_reading_relation('42ffa4f6-fd61-4830-bcaa-98a428677b0b', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('42ffa4f6-fd61-4830-bcaa-98a428677b0b', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('42ffa4f6-fd61-4830-bcaa-98a428677b0b', foundational, historical_subordination_creates_present_justice_claims).
narrative_ontology:cs_axiom_status(historical_subordination_creates_present_justice_claims, holdable).
narrative_ontology:cs_axiom_grounding('42ffa4f6-fd61-4830-bcaa-98a428677b0b', historical_subordination_creates_present_justice_claims, deontological).
narrative_ontology:cs_axiom('42ffa4f6-fd61-4830-bcaa-98a428677b0b', foundational, formal_neutrality_perpetuates_subordination).
narrative_ontology:cs_axiom_status(formal_neutrality_perpetuates_subordination, holdable).
narrative_ontology:cs_axiom_grounding('42ffa4f6-fd61-4830-bcaa-98a428677b0b', formal_neutrality_perpetuates_subordination, empirically_contingent).
narrative_ontology:cs_reference_frame('42ffa4f6-fd61-4830-bcaa-98a428677b0b', reconstruction_antisubordination_promise).
narrative_ontology:cs_drift_state('42ffa4f6-fd61-4830-bcaa-98a428677b0b', contemporary_post_repudiation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('42ffa4f6-fd61-4830-bcaa-98a428677b0b', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, members_of_non_preferred_racial_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, remedial_program_administrators).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members receive allocated opportunities — admissions seats, public contracts, legislative districts, employment advancement — through mechanisms that weigh group history in their favor. Advocacy organizations litigate and lobby to keep the reading dominant. An individual member cannot opt out of the classification without forfeiting the channel it opens; the community's claim on the arrangement is collective and spans generations, so exit would mean abandoning a protected route to accumulated correction.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities, beneficiary,
    organized, generational, constrained, national).

% Individual applicants, contractors, and employees are evaluated partly by race and lose concrete opportunities at decision points — a denied seat, a lost contract award, a passed-over promotion. The justification references group history they did not enact, and the cost lands on them as individuals with no personal opt-out. Moving jurisdictions rarely escapes the classification, since the reading travels with federal funding and accreditation; their recourse is litigation or political mobilization, both slow relative to a single application cycle.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, members_of_non_preferred_racial_groups, payer,
    moderate, biographical, constrained, national).

% Interprets the Fourteenth Amendment and decides which reading of equal protection governs. For decades it sustained and administered the remedial reading through school-desegregation orders and deference to congressional remediation; later majorities narrowed it (requiring identified discrimination and strict temporal limits) and the current Court has effectively repudiated race-conscious admissions. The judiciary cannot exit the interpretive role — its only alternatives are among readings — so its composition shifts, not any choice to leave, determine the constraint's fate.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Admissions deans, contracting officers, and equity-office staff design and run the preference apparatus: setting aside slots, scoring applications, filing compliance reports, defending programs in court. Budgets, headcount, and professional standing are constituted by the mandate's continuation; administering it is not a task they perform but the role they occupy. Dismantling the apparatus would mean dismantling their own positions, and the surrounding professional culture treats skepticism about the mandate as heresy.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, remedial_program_administrators, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, remedial_program_administrators, beneficiary).

% Jurists, litigants, and state electorates committed to the rival reading that equal protection forbids all governmental racial classifications. Inside this story's frame they are the dissenting voice outside the remedial consensus — but they are hardly silenced: they operate through litigation, ballot initiatives, and now hold a Supreme Court majority. Their exclusion from the remedial frame is increasingly nominal as their reading wins; they are included here because the remedial arrangement's persistence has always depended on keeping their objection out of operative constitutional doctrine.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_coalition, excluded,
    powerful, generational, mobile, national).

% Scholars of constitutional law and political philosophy who analyze the reading's coherence, genealogy, and consequences from outside enforcement — tracing it to Reconstruction purposes, comparing it with rival readings, and auditing whether outcomes match the anti-subordination premise. They collect no rents from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, constitutional_political_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the society-wide response to entrenched racial hierarchy into structured, lawful remediation: it solves the collective-action problem that neither markets, nor voluntary charity, nor scattered private action can reverse cumulative group disadvantage, and gives institutions a shared constitutional standard for allocating corrective opportunity.
% TRANSFER_FUNCTION: Moves opportunities and resources — admissions seats, contract awards, promotions, representation — from individual members of non-preferred racial groups (with diffuse public costs for program administration) toward members of historically subordinated groups, as payment of a corrective debt the reading locates in state-enforced caste.
% ABSENT_VOICES: Individual non-preferred-group members who bear the classification's costs are present only statistically — no seat speaks for the particular applicant denied, whose loss is aggregated away into 'burdens justified by greater goods.' Colorblind-reading advocates are heard in wider politics but excluded from this reading's internal consensus. Descendants on both sides of the ledger — of the harmed and of the benefited — who will inherit the arrangement's end-state have no seat at all.
% DISAPPEARANCE_RATIONALE: If the remedial mandate vanished overnight, admissions pools, contracting awards, districting plans, and promotion ladders would re-sort immediately around colorblind or diversity criteria; beneficiary communities would lose a constitutionally protected channel they currently hold; the compliance and advocacy apparatus built on the mandate would dissolve or migrate; and the constitutional settlement of the civil-rights era would reopen, since the rival readings dispute precisely this allocation.
% FOUNDING_PROBLEM: State-enforced caste — slavery, Black Codes, Jim Crow, exclusionary property and immigration regimes — produced durable group hierarchies that formally neutral law would freeze in place; the arrangement was built to make the Fourteenth Amendment's guarantee meaningful for the subordinated rather than a shield for inherited disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction historiography, census and educational-attainment disparity series, and civil-rights commission findings — all external to the beneficiary set — attest that the founding problem's underlying reality (persistent group disparity with roots in state-enforced caste) is real. Against the apparatus's current warrant, the Croson plurality's demand for identified discrimination, statewide electorates abolishing preferences by initiative, and the current Court's repudiation of race-conscious admissions — also external to the beneficiary set — attest that the specific remedial arrangement has outrun discrete, identified wrongs. Corroboration exists on both sides, from outside the benefiting parties; hence contested rather than dead or live.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.68: costs are concentrated and identity-specific — a denied seat is a lifetime event for the individual who would have held it — but bounded (marginal reallocation at competitive margins, not dispossession) and reciprocally channeled to the remediation target rather than to a capturing intermediary, which caps epsilon below snare levels. Suppression 0.40 (current): the constraint's coercive force has decayed sharply — noncompliance-era liability exposure and the absence of individual opt-out remain, but the reading no longer commands the enforcement organ. Theater 0.38: the core allocation function is real, but a growing share of activity is compliance symbolism, justification migration (remediation relabeled as outreach or holistic review), and consultant apparatus. Accessibility_collapse 0.50: the colorblind alternative does NOT collapse — it is constitutionally available and now federally mandated — so understanding the constraint does not close exits. Resistance 0.75: five decades of litigation, ballot initiatives, and political backlash. The measurement series run on ONE shared grid (t=0..70, step 10) with every tracked metric authored at every point. suppression_requirement is authored deliberately: enforcement capacity is the traced dynamic — build-up through the desegregation and set-aside eras (peak ~t=30), attrition after Croson/Adarand narrowed remedial justifications, and collapse after the current Court's repudiation of race-conscious admissions (~t=70). The falling trajectory models enforcement decay, not stable suppression; the scalar base_properties.suppression matches the interval endpoint.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seat compute opposite types from identical institutional facts: from inside the preferred group the arrangement is overdue correction flowing through the only mechanism that reaches accumulated disadvantage; from inside the dispreferred applicant the same mechanism is denial of individual consideration on account of race. The administrator seat straddles: it collects careers and budgets from the mandate while supplying the enforcement labor that sustains it. The judiciary sits near-symmetric stewardship — it neither collects the transfer nor bears it, but its composition decides which reading governs. Same-nominal-level divergence is sharpest here: two applicant populations face the same committee with opposite directionalities, differentiated not by global power but by the constraint's assignment of group membership.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (historically_subordinated_racial_minorities) derive low directionality — the constraint subsidizes them, damping or inverting effective extraction. Declared victims (members_of_non_preferred_racial_groups) with constrained exit derive high directionality — trapped-at-the-decision-point targets sit near the full-target end, so effective extraction is amplified for them beyond the base rate. Remedial_program_administrators are dual-positioned (agenda_setter collecting careers, secondary beneficiary) and land mid-low. Federal_judiciary appears in neither array and derives near-symmetric stewardship. Scope is national: verification of 'remediation progress' is genuinely hard at that scale, which the engine's scope modifier registers on the extractive side. Suppression is authored as a raw structural property (legal exposure, no opt-out) and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — caste-produced hierarchy frozen behind formal neutrality — is partly live and partly answered, and the arrangement's justification has visibly migrated: from correcting identified discrimination toward a generalized equity apparatus with self-perpetuating administration. That migration is the classic mandatrophy signature. The tangled_rope classification prevents mislabeling in both directions: calling this a pure snare erases the genuine corrective channel the reading provides to a subordinated class; calling it a rope or scaffold erases the asymmetric individual costs and the fact that the sunset is rhetorical rather than operative (no institution holds authority to certify 'remediation complete' — see omega remediation_endgame_certifiability). If the founding problem resolves and the structure persists as compliance ritual with flat extractiveness and rising theater, the terminal attractor is piton; if disparity data instead show the mandate's empirical premise failing, the scaffold-shaped retirement path opens. Both trajectories are carried as omegas rather than resolved by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading (remedial_reading) of the equal_protection_clause kernel; how would the sibling readings restructure the beneficiary and victim sets, and therefore the classification?',
    'Author and compile the sibling stories (equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading) and compare per-seat classifications across the family. The disagreement is located in whether the Fourteenth Amendment''s guarantee runs to groups or to individuals, and whether the Amendment embodies a corrective or a protective-neutral purpose.',
    'Under the colorblind instantiation the very same race-conscious arrangements flip from mandated remedy to forbidden extraction — beneficiary and victim sets invert and epsilon''s sign meaning reverses. Under the diversity instantiation the victim set dissolves into diffuse student-body benefits. The classification of THIS arrangement is unstable across readings by construction; cross-family comparison, not refinement of this file, is the resolution path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of a contested constitutional kernel; sibling readings restructure the party sets entirely.').

omega_variable(
    remediation_endgame_certifiability,
    'Does the remedial mandate contain a certifiable end-state — measurable criteria plus an institution empowered to declare remediation complete — or is the sunset indefinitely deferrable?',
    'Search doctrine and statute for remediation benchmarks and a designated certifying authority; observe whether any jurisdiction has ever triggered a completion declaration in seventy years of operation.',
    'If no certifiable sunset exists, the reading''s transitional self-understanding is aspirational rather than structural: scaffold certification is blocked, the tangled_rope classification holds, and drift risk toward snare rises as justifications migrate. If a credible endgame exists, the constraint is better read as scaffold-with-unmet-conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_endgame_certifiability, conceptual, 'Whether the reading''s built-in sunset (''when remediation is complete'') is operative or rhetorical.').

omega_variable(
    neutrality_disparity_empirics,
    'Does formal (colorblind) neutrality in fact perpetuate group subordination, as the reading''s load-bearing empirical premise asserts?',
    'Longitudinal disparity analysis under natural experiments: post-Proposition-209 California systems, post-repudiation admissions cycles, difference-in-differences on representation and outcome gaps before and after colorblind mandates take effect.',
    'If gaps persist or widen under neutrality, the empirical axiom (formal_neutrality_perpetuates_subordination) strengthens and the mandate''s warrant holds. If gaps close, the warrant decays and the constraint drifts toward scaffold-completion or piton. Because this axiom is grounded empirically_contingent, sustained contrary evidence routes toward engine-computed foreclosure of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_disparity_empirics, empirical, 'Empirical foundation of the anti-subordination premise; the reading stands or falls with it.').

omega_variable(
    enforcement_decay_terminal_state,
    'Is the measured decay in suppression_requirement decay-to-obsolescence (the sunset finally arriving) or decay-to-piton (enforcement collapses while program shells persist theatrically)?',
    'Track whether remedial programs formally sunset versus relabel (outreach, holistic review), migrate to private institutions beyond constitutional reach, or persist as compliance ritual with no allocation function.',
    'The obsolescence path supports orderly retirement of the constraint; the piton path predicts rising theater_ratio with flat extractiveness and warrants piton reclassification at interval end. The two paths diverge on whether the beneficiary seat retains anything worth defending.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_terminal_state, empirical, 'Terminal trajectory of the decaying enforcement regime documented in the suppression_requirement series.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_remedial_tr_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ep_remedial_tr_t10, equal_protection_clause__remedial_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ep_remedial_tr_t20, equal_protection_clause__remedial_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(ep_remedial_tr_t30, equal_protection_clause__remedial_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(ep_remedial_tr_t40, equal_protection_clause__remedial_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(ep_remedial_tr_t50, equal_protection_clause__remedial_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(ep_remedial_tr_t60, equal_protection_clause__remedial_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(ep_remedial_tr_t70, equal_protection_clause__remedial_reading, theater_ratio, 70, 0.38).

% Extraction over time
narrative_ontology:measurement(ep_remedial_be_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ep_remedial_be_t10, equal_protection_clause__remedial_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ep_remedial_be_t20, equal_protection_clause__remedial_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(ep_remedial_be_t30, equal_protection_clause__remedial_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(ep_remedial_be_t40, equal_protection_clause__remedial_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(ep_remedial_be_t50, equal_protection_clause__remedial_reading, base_extractiveness, 50, 0.71).
narrative_ontology:measurement(ep_remedial_be_t60, equal_protection_clause__remedial_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(ep_remedial_be_t70, equal_protection_clause__remedial_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ep_remedial_su_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ep_remedial_su_t10, equal_protection_clause__remedial_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(ep_remedial_su_t20, equal_protection_clause__remedial_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(ep_remedial_su_t30, equal_protection_clause__remedial_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(ep_remedial_su_t40, equal_protection_clause__remedial_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(ep_remedial_su_t50, equal_protection_clause__remedial_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement(ep_remedial_su_t60, equal_protection_clause__remedial_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(ep_remedial_su_t70, equal_protection_clause__remedial_reading, suppression_requirement, 70, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'equal protection' covers three structurally distinct claims that share one kernel (the Fourteenth Amendment text) but instantiate different constraints with different epsilon, different beneficiary/victim sets, and different failure modes. This file is the remedial_reading (groups hold justice claims; race-conscious correction mandated; individuals of non-preferred groups pay). The colorblind_reading (guarantee runs to individuals; all racial classifications forbidden) and the diversity_reading (classifications permitted for compelling educational diversity benefiting all students) are separate stories. The upstream/downstream structure is historical: remedial-era precedent and administrative machinery enabled diversity-framed programs, and remedial retrenchment drove institutional migration to the diversity rationale — so this reading structurally influences its diversity sibling while logically foreclosing the colorblind sibling. Linkage via affects_constraints enables contamination propagation: erosion of the remedial reading's enforcement (documented in the suppression_requirement series) propagates to both siblings' operating environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
