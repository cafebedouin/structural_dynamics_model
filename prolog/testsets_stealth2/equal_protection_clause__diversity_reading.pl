% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__diversity_reading, []).

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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Diversity Reading — Compelling Educational Diversity Permission
 *   domain: constitutional law/political philosophy/education policy
 *
 * SUMMARY:
 *   This story instantiates the DIVERSITY reading of the
 *   equal_protection_clause kernel: the claim that equal protection permits
 *   race-conscious admissions policies when they are narrowly tailored to
 *   serve compelling educational diversity interests whose benefits flow to
 *   all students — with minority students' presence serving the learning
 *   environment rather than remediating group subordination, and with no
 *   remedial sunset (the justification is ongoing educational value). The
 *   standing arrangement under contest, and the sole referent of epsilon, is
 *   the regime that operated from Powell's Bakke opinion (1978) through
 *   Students for Fair Admissions v. Harvard (2023): holistic review with race
 *   as one factor, defended case by case under strict scrutiny. Per the
 *   kernel-reading rules this file is authored epsilon-invariant: the
 *   colorblind and remedial readings are separate constraint stories with
 *   their own epsilon values, beneficiary/victim structures, and
 *   classifications, linked through network.affects_constraints and
 *   documented in the omega variables. The claimed type (tangled_rope) and
 *   the metrics are authored independently: the arrangement carries a genuine
 *   coordination function (a shared, litigable standard that made
 *   diversity-seeking lawful) AND asymmetric, actively enforced extraction
 *   (concentrated admission-denial costs on over-represented-group
 *   applicants, diffuse benefits across the class, capture of the composed
 *   class by the institutions that administer the process). Where the
 *   engine's computed per-seat classifications diverge from this claim, that
 *   divergence is the datum the corpus exists to collect. KEY AGENTS (by
 *   structural relationship): - selective_universities: agenda-setter and
 *   primary beneficiary (institutional/constrained) — administers the
 *   arrangement, captures the composed class, voluntarily bears its
 *   litigation costs - supreme_court: agenda-setter and analytical seat
 *   (institutional/analytical) — administers the strict-scrutiny boundary;
 *   its composition is the principal drift driver -
 *   underrepresented_minority_applicants: beneficiary with payer secondary
 *   (moderate/constrained) — access gained through a framing that makes their
 *   presence a means to the class's education - asian_american_applicants:
 *   primary payer (moderate/constrained) — admission odds reduced by
 *   race-conscious balancing; entered the structure only as litigants -
 *   white_applicants: payer (moderate/constrained) — the Bakke-era paradigm
 *   payers - admitted_student_body: beneficiary (moderate/mobile) — collects
 *   the diverse learning environment - race_neutral_alternative_advocates:
 *   excluded (organized/mobile) — alternatives adjudicated under deferential
 *   review controlled by the benefiting institutions
 *
 * KEY AGENTS:
 *   - selective_universities: agenda-setter and primary beneficiary (institutional/constrained) — designs and administers the arrangement, captures the composed diverse class as an institutional asset, voluntarily bears its litigation costs
 *   - supreme_court: agenda-setter and analytical seat (institutional/analytical) — administers the strict-scrutiny boundary; composition changes drive the arrangement's drift
 *   - underrepresented_minority_applicants: beneficiary with payer secondary (moderate/constrained) — gain access while serving, on this reading's own framing, as means to the class's education
 *   - asian_american_applicants: primary payer (moderate/constrained) — admission odds reduced by race-conscious balancing; recourse only through class litigation
 *   - white_applicants: payer (moderate/constrained) — the Bakke-era paradigm payers; odds reduced relative to race-neutral counterfactuals
 *   - admitted_student_body: beneficiary (moderate/mobile) — collects the diverse learning environment the arrangement exists to produce
 *   - race_neutral_alternative_advocates: excluded (organized/mobile) — percentage-plan and SES-preference proposals adjudicated under deferential review controlled by the benefiting institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.52).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.55).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Diversity Reading — Compelling Educational Diversity Permission").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional law/political philosophy/education policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '0cc1c675-e0d8-4348-8506-e1f6a9de808b').
narrative_ontology:cs_kernel_codification('0cc1c675-e0d8-4348-8506-e1f6a9de808b', fixed_text).
narrative_ontology:cs_authority_grounding('0cc1c675-e0d8-4348-8506-e1f6a9de808b', lineage).
narrative_ontology:cs_interpretation_layer_present('0cc1c675-e0d8-4348-8506-e1f6a9de808b').
narrative_ontology:cs_reading_relation('0cc1c675-e0d8-4348-8506-e1f6a9de808b', equal_protection_clause__remedial_reading, influences).
narrative_ontology:cs_reading_relation('0cc1c675-e0d8-4348-8506-e1f6a9de808b', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_axiom('0cc1c675-e0d8-4348-8506-e1f6a9de808b', foundational, educational_diversity_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('0cc1c675-e0d8-4348-8506-e1f6a9de808b', educational_diversity_compelling_interest, instrumental).
narrative_ontology:cs_axiom('0cc1c675-e0d8-4348-8506-e1f6a9de808b', secondary, all_students_primary_beneficiaries).
narrative_ontology:cs_axiom_status(all_students_primary_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('0cc1c675-e0d8-4348-8506-e1f6a9de808b', all_students_primary_beneficiaries, instrumental).
narrative_ontology:cs_reference_frame('0cc1c675-e0d8-4348-8506-e1f6a9de808b', strict_scrutiny_diversity_permission).
narrative_ontology:cs_drift_state('0cc1c675-e0d8-4348-8506-e1f6a9de808b', post_sffa_v_harvard_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0cc1c675-e0d8-4348-8506-e1f6a9de808b', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, admitted_student_body).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, asian_american_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, white_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, grutter_compelling_diversity_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__diversity_reading, holistic_individualized_review_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer holistic admissions at selective institutions, weighing race as one factor among many under the diversity rationale. They assert that a racially diverse class is pedagogically essential, litigated for decades to preserve the permission (the Grutter amicus coalition, the SFFA defense), and collect the composed class as an institutional asset. They control the process, the periodic reviews that reaffirm its necessity, and the internal data on how race actually operates in file reading. Going race-neutral is available — nine states operate under bans — but collides with mission statements, rankings, peer behavior, and alumni and faculty expectations.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, selective_universities, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicates whether a given race-conscious program is narrowly tailored to a compelling educational interest. Its composition sets the doctrine's boundaries — Bakke's median holding in 1978, Grutter's five-to-four apex in 2003, SFFA's repudiation in 2023. It administers the arrangement's boundaries rather than collecting from it, and its internal disagreement is the arrangement's principal drift mechanism.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, supreme_court, observer).

% Students at selective institutions who receive the learning environment the arrangement exists to produce: cross-racial exposure, classroom heterogeneity, preparation for leadership in a diverse society. They chose to attend and can transfer; their stake is diffuse across the class and biographical in duration.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, admitted_student_body, beneficiary,
    moderate, biographical, mobile, national).

% Applicants whose odds of admission to selective institutions rise when race is weighed. They gain access they would often not otherwise have. Under the diversity reading's own framing their presence serves the education of the whole class, which places them in a dual position: they receive access while also being the means through which the class's benefit is produced, with the representation burdens and heightened scrutiny that follow. Forgoing selective admission is the only individual exit.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, underrepresented_minority_applicants, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, underrepresented_minority_applicants, payer).

% Applicants to selective institutions whose admit rates fall relative to a race-neutral counterfactual when race is weighed — the group at the center of SFFA v. Harvard, where district-court findings recorded a statistical penalty (contested on appeal). They bear concentrated, identity-relevant denials they did not individually consent to and cannot individually escape, because elite admissions is a national market of similar practices. Their recourse was litigation as class representatives.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% Applicants whose admission odds likewise fall relative to race-neutral counterfactuals — the Bakke-era paradigm. The cost is concentrated and identity-relevant: denial of a scarce, life-shaping good through a classification whose rationale they may not share, with no individual exit from the practice.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, white_applicants, payer,
    moderate, biographical, constrained, national).

% Scholars, policymakers, and reformers proposing percentage plans, socioeconomic preferences, legacy elimination, or class-based admissions as substitutes. Courts adjudicated their proposals under deferential review controlled by the institutions whose practice they would replace, repeatedly accepting institutional findings that the alternatives were unworkable. They would restructure admissions entirely but had no seat in the periodic reviews that reaffirmed the arrangement.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, race_neutral_alternative_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:fixing_cost_class(equal_protection_clause__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the constitutional coordination problem of how selective universities can weigh race in assembling classes without violating equal protection: it conditions permissibility on individualized holistic review, absence of quotas, and a compelling educational-diversity rationale, giving institutions a shared, litigable standard.
% TRANSFER_FUNCTION: Moves admission probability at selective institutions from applicants of over-represented groups toward under-represented minority applicants; moves authority over racial composition to the institutions; and moves the burden of justification onto challengers, who must disprove narrow tailoring under deferential review.
% ABSENT_VOICES: Rejected applicants — principally Asian American and white applicants denied admission under race-conscious balancing — had no seat in the doctrinal conversation for most of the interval; they entered only as individual litigants (Bakke, Fisher, the SFFA plaintiffs). Also absent: advocates of race-neutral alternatives, whose proposals were adjudicated under deferential review controlled by the benefiting institutions, and minority students speaking to the costs of instrumentalization rather than access.
% DISAPPEARANCE_RATIONALE: If the diversity permission vanished overnight, selective admissions would reorganize around race-neutral criteria (as SFFA in fact forced): institutional diversity machinery would dismantle, enrollment compositions at selective schools would shift, state-law regimes would converge with federal doctrine, and the decades-long litigation economy built around strict scrutiny of admissions would dissolve.
% FOUNDING_PROBLEM: Powell's 1978 Bakke opinion was built to resolve a four-to-four constitutional deadlock: four justices would have permitted race-conscious admissions to remedy discrimination, four would have forbidden all racial classifications. The diversity rationale was the median holding — narrow enough to hold five votes, permissive enough to preserve institutional flexibility.
% FOUNDING_PROBLEM_CORROBORATION: The SFFA majority (2023) attests from outside the beneficiary set that the accommodation problem was real — it resolved the same problem the opposite way — and the Grutter dissenters attested it from the colorblind seat; admissions scholars across the normative spectrum, including race-neutral-alternative advocates, corroborate that the problem existed. No source outside the beneficiary set attests the problem is solved: the contest over how equal protection treats diversity-seeking continues in state policy, institutional practice, and pending litigation.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) and drifted slowly upward: costs are concentrated and identity-relevant (admission denied through a racial classification) while benefits are diffuse across the class and captured administratively by the institutions, but narrow-tailoring doctrine bounded the practice and the coordination function is genuine — hence a moderate, not high, base rate. Suppression (0.55) is enforcement machinery rather than coercion of applicants: the arrangement required a standing litigation-defense apparatus, deferential review that kept race-neutral alternatives legally 'unworkable' as found by the benefiting institutions themselves, and a transparency cost — institutions concealed race's actual operation in file reading (the Harvard personal-rating episode), a suppression of honest process that grew with scrutiny. Theater (0.36, rising) tracks the widening gap between individualized-review rhetoric and outcome-steering practice. Accessibility_collapse is low-moderate (0.35): race-neutral alternatives demonstrably function — nine states operate under bans — they are merely disfavored where the doctrine governs, so alternatives persist rather than collapse. Resistance is high (0.75): a half-century of litigation (Bakke, Hopwood, Grutter/Gratz, Fisher I and II, SFFA), state referenda in nine states, and sustained scholarly opposition. The three metric series share one time grid (eight points, 1978-2023). Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: the defense apparatus hardened from Bakke-era sub rosa operation through the Grutter apex to the SFFA terminal mobilization — a rising trajectory, not a static enforcement picture. The trajectories are drift with litigation-event step changes, not cycles; no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structural data. From the universities' seat the arrangement is a hard-won legal accommodation they litigated to keep — the costs are the price of a compelling educational good they consume. From the rejected applicants' seat the same structure is a racial gate on life-chances administered by parties who capture its value and control the deferential review that judges it. The minority-applicant seat computes a third way: access gained through a framing that makes their presence a means to others' education — benefit and instrumentalization in the same seat. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are declared beneficiaries and agenda-setters: directionality sits near the beneficiary end — they collect the composed class, set the process, and chose the litigation costs; constrained exit (mission, rankings, peer behavior) keeps them from the arbitrage end but they remain net gainers. Admitted students benefit (low directionality) diffusely and without administering. Under-represented minority applicants derive low directionality from the beneficiary declaration with the payer secondary pulling it upward — the instrumentalization cost is real but secondary to access. Asian American and white applicants are declared victims: directionality near the full-target end — concentrated, non-consensual, identity-relevant costs with constrained exit (elite admissions is a national oligopoly of similar practices). The Court is an analytical seat: it administers the boundary rather than collecting from it. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabels. Reading the arrangement as pure coordination would erase the concentrated costs: real applicants were denied admission through a racial classification for a benefit diffused across others, and the seat that captured the value also controlled the deferential review that kept alternatives 'unworkable.' Reading it as pure extraction would erase the genuine coordination function: the arrangement solved a real constitutional coordination problem (how diversity-seeking can be lawful), operated under binding tailoring constraints, and produced educational conditions its participants demonstrably valued. The mandatrophy question is sharpened by the sunset ambiguity (omega sunset_expectation_status): the arrangement carried sunset rhetoric (Grutter's 25-year expectation) without a sunset clause, and its founding problem is contested rather than dead — SFFA resolved the contest the other way rather than the problem evaporating. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) records a live contest, not a zombie mandate; the mandate did not outlive its function so much as lose a judicial majority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the diversity reading of the equal_protection_clause kernel; the colorblind and remedial readings are separate constraints with different victim sets, different epsilon, and different types — which reading governs is the primary contest and is not resolvable inside this story.',
    'Political and doctrinal evolution: which reading captures judicial majorities and institutional practice (as SFFA moved the Court toward the colorblind reading for admissions).',
    'If the colorblind reading governs, the operative arrangement becomes the prohibition itself (a different story file: applicants as rights-bearers, the classification regime as target). If the remedial reading governs, the victim set shifts to historically subordinated groups denied remediation and epsilon is assessed on the subordination arrangement. This story''s classification is valid only within the diversity reading''s framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the equal protection kernel governs is the primary contest; this story''s classification holds only within the diversity reading.').

omega_variable(
    reading_disagreement_location,
    'Where do the three readings structurally diverge — on the identity of the beneficiary class (all students vs. historically subordinated groups vs. individual rights-bearers), on whether equal protection permits or forbids racial classifications, or on temporal scope (permanent educational value vs. remedial completion)?',
    'Conceptual: map each sibling''s foundational axiom and identify which structural element (beneficiary identity, permissive/prohibitory direction, sunset) each axiom actually fixes.',
    'Different disagreement locations imply different foreclosure structures: divergence on permissibility direction is near-foreclosing within one framework; divergence on beneficiary identity is coexistence-compatible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_location, conceptual, 'Locating the structural element on which the three readings actually diverge.').

omega_variable(
    diversity_benefit_magnitude,
    'Are the educational benefits of classroom diversity — the empirical foundation of the compelling-interest axiom — as large as the arrangement''s defenders claim?',
    'Longitudinal outcome studies of students educated under race-conscious versus race-neutral regimes; the SFFA litigation record; post-SFFA institutional outcome data.',
    'If benefits are small, the coordination function is overstated, epsilon rises, and the arrangement drifts snare-ward (costs without commensurate coordination value); if large, the coordination component is genuine and the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_benefit_magnitude, empirical, 'Whether classroom diversity produces the educational benefits the compelling-interest axiom requires.').

omega_variable(
    minority_instrumentalization_cost,
    'Do under-represented minority students bear net costs from admission as means to others'' education (stereotype threat, representation burden, academic mismatch) or net benefits from the access itself?',
    'Outcome comparisons for minority students admitted under race-conscious versus race-neutral regimes (mismatch literature versus access literature); post-SFFA enrollment and completion data.',
    'If net costs, the minority-applicant seat flips from beneficiary to payer, epsilon rises substantially, and the all-students-benefit axiom loses its empirical footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_instrumentalization_cost, empirical, 'Whether minority students bear net costs from serving as means to the class''s education.').

omega_variable(
    tailoring_genuineness,
    'Do the narrow-tailoring requirements (individualized review, no quotas, no mechanical weighting) genuinely bound outcomes, or is the holistic process a theater that steers toward target compositions while denying it?',
    'Discovery into admissions internals (as in SFFA v. Harvard), internal institutional research, and natural experiments from the nine state bans.',
    'If tailoring is theater, the authored theater_ratio is understated and the arrangement drifts piton/snare-ward; if genuine, the moderate-epsilon assessment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tailoring_genuineness, empirical, 'Whether narrow-tailoring requirements genuinely bound outcomes or are theater.').

omega_variable(
    sunset_expectation_status,
    'Does Grutter''s 25-year expectation constitute a functional sunset clause (making the arrangement scaffold-like) or non-binding dicta leaving the constraint permanent?',
    'Doctrinal analysis of the remark''s operative force plus institutional behavior: whether programs ever adopted termination criteria (they did not — periodic reviews reaffirmed necessity).',
    'A functional sunset would make scaffold available and re-date the arrangement''s legitimacy; permanence supports the tangled_rope classification and the permanent-constraint structure of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_expectation_status, conceptual, 'Whether Grutter''s 25-year remark is a functional sunset clause or dicta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__diversity_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t1985, equal_protection_clause__diversity_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(equa_tr_t1985, observed).
narrative_ontology:measurement(equa_tr_t1992, equal_protection_clause__diversity_reading, theater_ratio, 1992, 0.25).
narrative_ontology:measurement_basis(equa_tr_t1992, observed).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_clause__diversity_reading, theater_ratio, 1996, 0.28).
narrative_ontology:measurement_basis(equa_tr_t1996, observed).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__diversity_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement_basis(equa_tr_t2003, observed).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_clause__diversity_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement_basis(equa_tr_t2010, observed).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_clause__diversity_reading, theater_ratio, 2016, 0.35).
narrative_ontology:measurement_basis(equa_tr_t2016, observed).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__diversity_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement_basis(equa_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__diversity_reading, base_extractiveness, 1978, 0.44).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t1985, equal_protection_clause__diversity_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement_basis(equa_be_t1985, observed).
narrative_ontology:measurement(equa_be_t1992, equal_protection_clause__diversity_reading, base_extractiveness, 1992, 0.47).
narrative_ontology:measurement_basis(equa_be_t1992, observed).
narrative_ontology:measurement(equa_be_t1996, equal_protection_clause__diversity_reading, base_extractiveness, 1996, 0.48).
narrative_ontology:measurement_basis(equa_be_t1996, observed).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__diversity_reading, base_extractiveness, 2003, 0.5).
narrative_ontology:measurement_basis(equa_be_t2003, observed).
narrative_ontology:measurement(equa_be_t2010, equal_protection_clause__diversity_reading, base_extractiveness, 2010, 0.51).
narrative_ontology:measurement_basis(equa_be_t2010, observed).
narrative_ontology:measurement(equa_be_t2016, equal_protection_clause__diversity_reading, base_extractiveness, 2016, 0.52).
narrative_ontology:measurement_basis(equa_be_t2016, observed).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__diversity_reading, base_extractiveness, 2023, 0.52).
narrative_ontology:measurement_basis(equa_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__diversity_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t1985, equal_protection_clause__diversity_reading, suppression_requirement, 1985, 0.32).
narrative_ontology:measurement_basis(equa_su_t1985, observed).
narrative_ontology:measurement(equa_su_t1992, equal_protection_clause__diversity_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement_basis(equa_su_t1992, observed).
narrative_ontology:measurement(equa_su_t1996, equal_protection_clause__diversity_reading, suppression_requirement, 1996, 0.4).
narrative_ontology:measurement_basis(equa_su_t1996, observed).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__diversity_reading, suppression_requirement, 2003, 0.5).
narrative_ontology:measurement_basis(equa_su_t2003, observed).
narrative_ontology:measurement(equa_su_t2010, equal_protection_clause__diversity_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement_basis(equa_su_t2010, observed).
narrative_ontology:measurement(equa_su_t2016, equal_protection_clause__diversity_reading, suppression_requirement, 2016, 0.54).
narrative_ontology:measurement_basis(equa_su_t2016, observed).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__diversity_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement_basis(equa_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'equal protection and affirmative action' covers three structurally distinct constraints (the epsilon-invariance decomposition): the diversity reading (this story — permissive, all-students beneficiaries, moderate epsilon, permanent), the remedial reading (remediation of historical subordination, subordinated-group beneficiaries, groups denied remediation as the cost-bearers, remedial sunset), and the colorblind reading (prohibitory, individual rights-bearers, the classification regime itself as target). One text, three constraints, three files; each links the others via network.affects_constraints. The diversity reading upstream-influenced the remedial reading: once Powell made diversity the sole surviving rationale, remedial justifications were channeled out of admissions doctrine without being logically eliminated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
