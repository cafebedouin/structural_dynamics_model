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
 *   human_readable: Race-Conscious Remediation Mandate for Substantive Equality
 *   domain: constitutional/educational
 *
 * SUMMARY:
 *   The remedial reading of equal protection interprets the Fourteenth
 *   Amendment as requiring race-conscious remediation of documented
 *   historical group subordination to achieve substantive equality. The
 *   reading claims that formal color-blindness perpetuates de facto hierarchy
 *   and that affirmative institutional measures targeting historically
 *   excluded groups are constitutionally mandated, not merely permitted. This
 *   reading directly contradicts the colorblind reading (equal protection
 *   forbids racial classification) and coexists with the diversity reading
 *   (race-consciousness is permissible for diversity benefits, distinct from
 *   remediation justification). The remedial reading stakes are structural:
 *   who bears the individual cost of group remediation, whether that cost is
 *   a necessary price of correcting subordination, and when remediation can
 *   credibly be declared complete and the constraint retired.
 *
 * KEY AGENTS:
 *   - Historically marginalized racial groups (beneficiaries, structurally trapped in group identity, generational time horizon)
 *   - Non-preferred group individual applicants (payers, bearing biographical opportunity costs, constrained exit)
 *   - Remediation-administering institutions (agenda-setters, enforcing the mandate, facing litigation pressure)
 *   - Constitutional interpreters (agenda-setters, defining scope and permissibility via doctrine)
 *   - Colorblind reading advocates (excluded, institutionally powerful, contesting the reading's legitimacy)
 *   - Empirical policy evaluators (observers, tracking remediation outcomes and completion credibility)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.68).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.41).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Race-Conscious Remediation Mandate for Substantive Equality").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional/educational").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '5cb6098c-81cc-4611-a767-e357a87c24c3').
narrative_ontology:cs_kernel_codification('5cb6098c-81cc-4611-a767-e357a87c24c3', fixed_text).
narrative_ontology:cs_authority_grounding('5cb6098c-81cc-4611-a767-e357a87c24c3', lineage).
narrative_ontology:cs_interpretation_layer_present('5cb6098c-81cc-4611-a767-e357a87c24c3').
narrative_ontology:cs_reading_relation('5cb6098c-81cc-4611-a767-e357a87c24c3', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('5cb6098c-81cc-4611-a767-e357a87c24c3', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('5cb6098c-81cc-4611-a767-e357a87c24c3', foundational, race_consciousness_remediation_mandate).
narrative_ontology:cs_axiom_status(race_consciousness_remediation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('5cb6098c-81cc-4611-a767-e357a87c24c3', race_consciousness_remediation_mandate, deontological).
narrative_ontology:cs_axiom('5cb6098c-81cc-4611-a767-e357a87c24c3', foundational, substantive_equality_requires_group_correction).
narrative_ontology:cs_axiom_status(substantive_equality_requires_group_correction, holdable).
narrative_ontology:cs_axiom_grounding('5cb6098c-81cc-4611-a767-e357a87c24c3', substantive_equality_requires_group_correction, empirically_contingent).
narrative_ontology:cs_reference_frame('5cb6098c-81cc-4611-a767-e357a87c24c3', post_brown_remediation_framework).
narrative_ontology:cs_drift_state('5cb6098c-81cc-4611-a767-e357a87c24c3', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5cb6098c-81cc-4611-a767-e357a87c24c3', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_racial_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_group_individual_applicants).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, structural_racism_perpetuation_thesis).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, group_remediation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collective benefit from remedial race-conscious admissions, hiring, and contracting policies designed to overcome documented patterns of exclusion and subordination. Individual members may not directly control their group's remedial status but structurally benefit from systemic correction of historical barriers. Exit from beneficiary status would require denying the group's historical disadvantage.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_marginalized_racial_groups, beneficiary,
    powerless, generational, trapped, national).

% Bear individual opportunity costs when race-conscious remedial preferences are applied in admissions and hiring contexts: a qualified individual may be rejected or lower-ranked due to racial classification designed to serve group remediation. Their options are absorbing the cost, seeking opportunities in non-remediated settings, or challenging the remedial policy via litigation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_group_individual_applicants, payer,
    moderate, biographical, constrained, national).

% Universities, employers, and government agencies tasked with implementing race-conscious remedial policies. They design admissions matrices, hiring preferences, and contracting set-asides; they face litigation, administrative review, and political pressure from opponents of race-consciousness while being mandated to achieve substantive equity outcomes. Enforcement machinery requires them to document historical subordination and calibrate remedial intensity.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, remediation_administering_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Courts and constitutional scholars who adjudicate the remedial reading's scope, duration, and permissibility. They define what counts as historical subordination warranting remediation, what remedial measures satisfy strict scrutiny, and when remediation becomes complete. Their interpretive choices directly enable or constrain the constraint's operation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, constitutional_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Parties (litigators, legislators, voters, judges) who advocate the colorblind reading of equal protection and argue that race-consciousness violates equal protection itself. They are excluded from the remedial framing's decision-making structure by definition; their challenge is to overturn or narrow the remedial mandate via constitutional amendment or judicial reinterpretation. They hold significant institutional power but operate outside the remedial framework's legitimacy boundaries.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_reading_advocates, excluded,
    powerful, generational, mobile, national).

% Empirical researchers and independent analysts who assess whether race-conscious remediation achieves stated substantive equality goals, whether remedial intensity tracks historical subordination magnitude, and when remediation can credibly be declared complete. They take data from all other seats but serve no institutional beneficiary.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, policy_evaluators, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, remediation_administering_institutions).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Corrects for the ongoing structural effects of historical racial subordination by using racial classification to redirect institutional resources (admissions slots, employment positions, contracts) toward groups systematically excluded from prior distribution. Solves the remediation problem: how institutions can move from de facto racial hierarchy (perpetuated by race-neutral rules applied to unequal starting conditions) to substantive equality of access and opportunity.
% TRANSFER_FUNCTION: Transfers opportunity (admissions slots, job placements, contract awards) from non-preferred group individual applicants to members of historically marginalized groups, conditional on historical group subordination being documented and remediation being calibrated to remedy that specific subordination.
% ABSENT_VOICES: Individual members of non-preferred groups who would challenge the constraint as imposing unchosen costs for historical wrongs they did not commit; colorblind reading advocates who argue race-consciousness itself violates equal protection; future generations' interest in whether permanent group remediation becomes entrenched. Excluded from the remedial framing by constitutional interpretation but structurally affected.
% DISAPPEARANCE_RATIONALE: If the remedial reading and its enforcement disappeared, institutions would revert to formally race-neutral admissions and hiring, which would reproduce pre-remediation racial hierarchies absent alternative corrective mechanisms. The distribution of opportunity would reorganize around non-racial proxies (socioeconomic status, geography) that correlate imperfectly with historical group subordination; historically marginalized groups' representation in higher education, professional employment, and government contracting would decline measurably within 5–10 years.
% FOUNDING_PROBLEM: Formal abolition of explicit racial discrimination (Jim Crow repeal, Civil Rights Act 1964) did not dismantle the institutional effects of prior subordination: segregated housing, inherited wealth gaps, educational underinvestment, and employer hiring networks continued to exclude historically marginalized groups even after legal discrimination ended. Achieving substantive equality required affirmative measures addressing documented group subordination, not merely forbidding explicit racism.
% FOUNDING_PROBLEM_CORROBORATION: Documented by the Warren Court and remedial civil rights scholarship (Brown remediation framework, Green v. County School Board institutional integration mandate). Attested independently by empirical research on intergenerational wealth gaps, segregation persistence, and pre-remediation admissions/hiring disparities. Contested by colorblind reading advocates who argue the founding problem conflates unlawful discrimination (which ended) with unequal outcomes (which may have multiple causes). Independent verification exists from outside the remediation-supporting coalition: labor economists, sociologists, and historians document persistent group-level exclusion despite formal legal equality.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at current point) because the remedial mandate consciously imposes individual costs (rejected admissions, lower-ranked hiring) on non-preferred applicants to serve group remediation goals, and this transfer is sustained by constitutional interpretation that prioritizes group subordination correction over individual color-blindness. Suppression is moderate (0.41) because resistance is substantial and well-organized (colorblind advocates control significant litigation and legislative capacity), and the remedial reading must actively defend itself against legal challenges; suppression is NOT high because the constraint's operation is transparent (race-conscious policies are disclosed, not hidden) and individual payers have exit options (litigation, seeking non-remediated institutions). Theater is low-moderate (0.22) because remedial policy administration involves real institutional work (historical subordination documentation, individualized assessment within group categories) even though some enforcement activity defends the reading's legitimacy against constitutional challenge rather than directly managing opportunity allocation. The measurement series shows sharp rise (1954–1995) as the Bakke framework emerged and remedial policies institutionalized, then plateau (1995–2026) as litigation stabilized the constraint's boundaries without expanding scope — a signature of a scaffold with defined but contested endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat and the payer seat compute radically different types from identical constraint structure: from the historically marginalized group's view, the constraint solves a group remediation problem and operates as necessary correction (coordinated remediation, low extraction, temporary); from the non-preferred individual applicant's view, the constraint imposes an unchosen biographical cost to serve a group goal they did not create and may not endorse (extraction, identity-locked suppression, mandated participation). The agenda-setter (constitutional interpreters) sees the constraint as legitimate constitutional mandates; the excluded seat (colorblind advocates) sees it as a constitutional violation. These are not different measurements of the same type — they are genuinely different perceived constraint types emerging from structural position and temporal horizon. The engine computes this divergence from power, exit, and beneficiary/victim declarations; the authored claim (scaffold) reflects the remedial reading's own framing (temporary, justified by founding problem, carrying a sunset), not the payer seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups: d approaches 0.0 (full beneficiary). They are structurally trapped in group identity (exit_options: trapped — one cannot exit being a member of a group with documented subordination history; the only exit is individual denial of group membership, which the remedial reading treats as rejecting the correction offered). They carry zero biographical cost within this constraint (the constraint transfers TO them, not FROM them). Non-preferred individual applicants: d approaches 1.0 (full target). They are identity-locked into individual-level applicant status; they cannot exit by changing how they are categorized (their race is fixed for purposes of remedial policy). They bear the direct biographical cost of remedial priority. Remediation-administering institutions: d near 0.5–0.6 (mixed). They benefit from the mandate's legitimacy and institutional clarity but bear enforcement costs and litigation exposure. Constitutional interpreters: d near 0.5 (symmetric). They set the rules and thus have agenda power, but they are also constrained by constitutional text and prior doctrine, and they face contestation from the colorblind reading advocates with equal institutional standing. No directionality overrides needed — the derivation chain produces accurate d from the declared beneficiary/victim + exit structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading carries inherent mandatrophy risk: the founding problem (persistent group subordination effects despite formal legal equality) was live in 1964–1995 and remains contested in 2026. Empirical indicators (wealth gaps, segregation indices, hiring disparities) show that subordination perpetuation has NOT fully resolved, suggesting the founding problem persists. However, colorblind reading advocates argue that continued unequal outcomes are NOT evidence of ongoing legally cognizable subordination — they may reflect cultural factors, geographic distribution, or educational pipeline effects unrelated to constitutional injury. The measurement plateau (extraction and suppression stabilized since ~2009) suggests the constraint has reached a steady state rather than progressing toward completion and sunset. The scaffold classification requires that the founding problem remain live enough to justify the sunset expectation; a plateau that extends indefinitely would signal mandatrophy (a temporary measure that became permanent extraction). The omega variable 'remediation_completion_criteria' documents this uncertainty: when, by what evidence, and according to which reading will remediation be declared complete?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_criteria,
    'What evidence and standards determine when historical group subordination has been sufficiently remedied that the remedial mandate''s sunset can credibly be triggered?',
    'Court-approved metrics: parity in wealth accumulation, occupational distribution, educational attainment, hiring rates, and contracting awards between historically marginalized and non-marginalized groups, sustained over a specified interval (20+ years). Independent evaluation by policy researchers outside both remedial-reading and colorblind-reading constituencies.',
    'If completion criteria are unmet or unmeasurable, the constraint risks permanent mandatrophy — a temporary remedy that becomes entrenched extraction. If criteria are defined but unmet, the scaffold classification holds and remediation continues justified. If criteria are met and acknowledged by independent evaluators, sunset is constitutionally and politically feasible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remediation_completion_criteria, empirical, 'Whether historical group subordination effects can be measured to determine remediation completion').

omega_variable(
    individual_vs_group_remediation_boundary,
    'Is the remedial reading''s mandate to remedy GROUP subordination properly applied to individual-level admissions and hiring decisions, or should remediation operate at the institutional/sectoral level only?',
    'Comparative analysis of remedial regimes: group-level remediation (hiring goals, contracting set-asides for sectors/regions) vs. individual-level affirmative action (admissions preferences, individual employment decisions). Measure whether both approaches reduce group-level subordination measures and whether individual-level approaches impose disproportionate costs on specific non-preferred individuals.',
    'If group-level remediation can achieve subordination reduction without individual-level preferences, individual extractiveness declines and the constraint becomes more clearly coordinative. If individual-level application is necessary to reach subordinated groups, the constraint''s extractiveness remains justified by remedial necessity. The boundary affects d for non-preferred individuals (down if group-level suffices, steady if individual-level is mandated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_group_remediation_boundary, empirical, 'Whether remedial mandate requires individual-level race-consciousness or can operate via group/sectoral mechanisms').

omega_variable(
    colorblind_foreclosure_contingency,
    'Can the remedial reading''s core premise (race-consciousness required for remediation) coexist with the colorblind reading''s core premise (race-consciousness violates equal protection) within a single constitutional framework, or do they logically foreclose one another?',
    'Constitutional jurisprudence analysis: test whether a court could endorse both that race-consciousness is mandated for remediation AND that all racial classification violates equal protection. If the premises are logically incompatible, one reading forecloses the other; if frameworks can distinguish remedial from non-remedial contexts, coexistence is possible.',
    'If foreclosure is confirmed, the remedial and colorblind readings cannot both be ''live'' in the same institutional decision-maker''s doctrine — one must override the other, and the winner-take-all structure determines constitutional meaning for decades. If coexistence is possible (via narrow-tailoring doctrine that permits remedial but forbids non-remedial race-consciousness), both readings remain institutionally live, courts can split, and remand/relitigation cycles propagate. Foundational axiom status depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_foreclosure_contingency, conceptual, 'Logical foreclosure relationship between remedial and colorblind interpretations of equal protection').

omega_variable(
    intergenerational_remediation_scope,
    'Does the remedial mandate apply only to the generation that directly experienced historical subordination, or does it extend to descendants across multiple generations until subordination effects are fully remedied?',
    'Genealogical and empirical analysis: track whether group subordination benefits and harms are inherited (wealth transmission, educational access, social capital) and whether remedial policies calibrate to generational proximity or to total group effect. Compare policy design in remedial regimes that limit remediation by generation vs. those that apply open-endedly until outcomes parity is achieved.',
    'Narrow scope (one generation removed from direct subordination) shortens the remedial mandate and enables faster sunset; broad scope (intergenerational until parity) extends the mandate indefinitely if subordination effects compound across generations. This directly affects mandatrophy risk: open-ended intergenerational remediation can become permanent extraction if group subordination effects never fully dissipate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_remediation_scope, conceptual, 'Temporal scope of remedial mandate across generational lines').

omega_variable(
    remedial_reading_kernel_contest,
    'Can the remedial and colorblind readings both claim legitimate grounding in the same constitutional text (the Fourteenth Amendment), or does one reading''s textual authority preclude the other?',
    'Constitutional hermeneutics: analyze whether the Amendment''s text (''No State shall deny to any person the equal protection of the laws'') mandates, permits, forbids, or remains silent on race-consciousness. Test whether textual authority alone can adjudicate between readings or whether all readings must appeal to purposes, history, and institutional consequences beyond the text.',
    'If textual authority favors one reading, that reading gains structural legitimacy and the other becomes a minority position. If the text is genuinely underdetermined, all readings remain equally textually grounded and institutional power determines which interpretation prevails. This affects the axiom_overriding pathway: an axiom grounded in a textual claim can be foreclosed if the text''s meaning is reinterpreted; an axiom grounded in extratextual purposes cannot be foreclosed by text alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_reading_kernel_contest, conceptual, 'Textual grounding and hermeneutic authority of remedial vs. colorblind readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1954, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_clause__remedial_reading, theater_ratio, 1954, 0.0).
narrative_ontology:measurement_basis(equa_tr_t1954, observed).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__remedial_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_clause__remedial_reading, theater_ratio, 1995, 0.16).
narrative_ontology:measurement_basis(equa_tr_t1995, observed).
narrative_ontology:measurement(equa_tr_t2009, equal_protection_clause__remedial_reading, theater_ratio, 2009, 0.2).
narrative_ontology:measurement_basis(equa_tr_t2009, observed).
narrative_ontology:measurement(equa_tr_t2020, equal_protection_clause__remedial_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement_basis(equa_tr_t2020, observed).
narrative_ontology:measurement(equa_tr_t2026, equal_protection_clause__remedial_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(equa_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_clause__remedial_reading, base_extractiveness, 1954, 0.0).
narrative_ontology:measurement_basis(equa_be_t1954, observed).
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__remedial_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t1995, equal_protection_clause__remedial_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement_basis(equa_be_t1995, observed).
narrative_ontology:measurement(equa_be_t2009, equal_protection_clause__remedial_reading, base_extractiveness, 2009, 0.65).
narrative_ontology:measurement_basis(equa_be_t2009, observed).
narrative_ontology:measurement(equa_be_t2020, equal_protection_clause__remedial_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(equa_be_t2020, observed).
narrative_ontology:measurement(equa_be_t2026, equal_protection_clause__remedial_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(equa_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_clause__remedial_reading, suppression_requirement, 1954, 0.0).
narrative_ontology:measurement_basis(equa_su_t1954, observed).
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__remedial_reading, suppression_requirement, 1978, 0.28).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t1995, equal_protection_clause__remedial_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement_basis(equa_su_t1995, observed).
narrative_ontology:measurement(equa_su_t2009, equal_protection_clause__remedial_reading, suppression_requirement, 2009, 0.39).
narrative_ontology:measurement_basis(equa_su_t2009, observed).
narrative_ontology:measurement(equa_su_t2020, equal_protection_clause__remedial_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement_basis(equa_su_t2020, observed).
narrative_ontology:measurement(equa_su_t2026, equal_protection_clause__remedial_reading, suppression_requirement, 2026, 0.41).
narrative_ontology:measurement_basis(equa_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_clause kernel decomposes into three structurally distinct constraint stories: (1) remedial_reading (this file) — race-consciousness mandated for group remediation, high ε, temporary, scaffold type; (2) colorblind_reading — all racial classification forbidden, near-zero ε, permanent, mountain type; (3) diversity_reading — race-consciousness permitted for educational diversity, moderate ε, conditional, tangled_rope type. Each reading instantiates a different constraint because their ε values, beneficiary/victim structures, and temporal profiles differ radically. The remedial reading's epsilon (0.68) measures extraction imposed ON non-preferred individuals TO remediate group subordination. The colorblind reading's epsilon measures the constraint imposed ON remedial policies themselves (foreclosure of race-consciousness, high suppression of remedial administration). The diversity reading's epsilon measures the coordination overhead of maintaining diversity-justifying institutional mechanisms. They share the kernel (the Fourteenth Amendment) but emit entirely different constraint structures because they read the Amendment's mandate differently. All three are live positions in contemporary constitutional discourse, held by different institutional coalitions (courts, legislatures, scholar networks). The remedial reading influences both siblings: it constrains the colorblind reading's institutional expansion (remedial mandates must be satisfied before colorblindness can be enforced) and it competes with the diversity reading for race-consciousness's legitimacy (if remediation fully succeeds, diversity justification becomes optional rather than mandated). Constraint family structure: remedial_reading -> {colorblind_reading, diversity_reading}.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
