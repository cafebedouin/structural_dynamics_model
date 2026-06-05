% ============================================================================
% CONSTRAINT STORY: dignitary_harm_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignitary_harm_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dignitary_harm_reading
 *   human_readable: Dignitary Harm Reading of Speech Protection Boundary
 *   domain: constitutional_law/free_speech
 *
 * SUMMARY:
 *   The dignitary harm reading asserts that speech causing demonstrable,
 *   non-speculative harm to vulnerable minorities and constructing systemic
 *   oppression may be constitutionally restricted. This reading instantiates
 *   one coherent interpretation of the speech protection boundary in
 *   constitutional law, coexisting with the near-absolutist reading
 *   (expression can almost never be suppressed, even if harmful) and the
 *   balancing reading (protection depends on context and competing
 *   interests). The dignitary harm reading is characterized by: (1) an
 *   expanded victim set (targets of bigoted speech, vulnerable minorities
 *   experiencing cumulative psychological and epistemic harm), (2) a
 *   requirement that harm be demonstrable rather than speculative (moving the
 *   boundary from intent to effects), and (3) recognition that speech
 *   functions as a mechanism of systemic oppression rather than merely
 *   expressing pre-existing hierarchies. The constraint is a tangled rope
 *   because the restriction mechanism contains both genuine coordination
 *   (enabling epistemic participation and social access for historically
 *   silenced groups) and extractive overhead (institutional authority to
 *   police speech boundaries, chilling effects on marginal political
 *   expression, enforcement asymmetries, potential weaponization). The
 *   theater ratio is low (0.38) because the restriction mechanism is
 *   functionally tied to its stated purpose (preventing demonstrable
 *   dignitary harm), unlike piton constraints where performance has decoupled
 *   from function. Base extractiveness has increased from 0.35 to 0.52 over
 *   the interval, reflecting both expanded enforcement (more cases
 *   prosecuted) and boundary creep (definitions of covered speech expanding
 *   beyond original intent).
 *
 * KEY AGENTS:
 *   - Vulnerable minorities and targets of systemic harassment: Primary beneficiaries (institutional/arbitrage) — experience the restriction as enabling epistemic access and protecting from cumulative psychological harm
 *   - Marginally political speakers and activist communities: Primary victims (moderate/constrained) — face chilling effects and self-censorship costs; borderline cases bear high enforcement risk
 *   - Judiciary and civil rights enforcement agencies: Institutional enforcer (institutional/constrained) — experience both coordination function (protecting access) and extraction benefit (expanded authority); constrained because bound by constitutional interpretation
 *   - Civil rights advocacy coalition: Organized beneficiary (institutional/arbitrage) — frames the restriction as pure coordination; has strategic agency in enforcement intensity and test case selection
 *   - Analytical observer at civilizational scope: Sees both genuine coordination and extractive overhead without reducing to either
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignitary_harm_reading, 0.52).
domain_priors:suppression_score(dignitary_harm_reading, 0.58).
domain_priors:theater_ratio(dignitary_harm_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignitary_harm_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignitary_harm_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dignitary_harm_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignitary_harm_reading, tangled_rope).
narrative_ontology:human_readable(dignitary_harm_reading, "Dignitary Harm Reading of Speech Protection Boundary").
narrative_ontology:topic_domain(dignitary_harm_reading, "constitutional_law/free_speech").

domain_priors:requires_active_enforcement(dignitary_harm_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(dignitary_harm_reading, fixed_text).
narrative_ontology:cs_authority_grounding(dignitary_harm_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(dignitary_harm_reading).
narrative_ontology:cs_kernel_id(dignitary_harm_reading, speech_protection_boundary).
narrative_ontology:cs_reading_relation(dignitary_harm_reading, near_absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation(dignitary_harm_reading, balancing_reading, coexists_with).
narrative_ontology:cs_axiom(dignitary_harm_reading, foundational, dignitary_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(dignitary_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom(dignitary_harm_reading, foundational, systemic_oppression_constructed_by_speech).
narrative_ontology:cs_axiom_status(systemic_oppression_constructed_by_speech, holdable).
narrative_ontology:cs_reference_frame(dignitary_harm_reading, equal_epistemic_access_framework).
narrative_ontology:cs_drift_state(dignitary_harm_reading, contemporary_polarization_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignitary_harm_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(dignitary_harm_reading, targets_of_systemic_harassment).
narrative_ontology:constraint_beneficiary(dignitary_harm_reading, epistemic_commons).
narrative_ontology:constraint_victim(dignitary_harm_reading, speakers_subject_to_restriction).
narrative_ontology:constraint_victim(dignitary_harm_reading, marginally_political_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED MINORITY / DIGNITARY HARM VICTIM (SNARE) — Cannot exit the speech environment; bears cumulative harm from repeated bigoted speech. The constraint itself (restriction on hate speech) is experienced as minimal — the true extractive constraint is the prior speech ecosystem that constructs systemic oppression. This perspective experiences the dignitary harm reading as a necessary correction, not as suppression. However, the reading's classification logic places this agent in a powerless/trapped position relative to the speech restriction mechanism itself, creating a structural paradox resolved by omega variables.
constraint_indexing:constraint_classification(dignitary_harm_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALLY POLITICAL SPEAKER / BORDERLINE CASE (SNARE) — Moderate power agents (local activists, community organizers, nonprofit staff, academics) face high suppression from the restriction because they operate near the boundary of protected speech. The restriction creates chilling effects: speakers self-censor to avoid misclassification. Exit is constrained but possible (relocate, change organizations, moderate rhetoric). The effective extraction is the self-censorship cost plus enforcement risk. This perspective sees pure extraction: minimal coordination benefit, maximum perceived suppression.
constraint_indexing:constraint_classification(dignitary_harm_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENFORCEMENT INSTITUTION / JUDICIARY (TANGLED ROPE) — Courts and civil rights agencies experience the dignitary harm reading as a genuine coordination mechanism (protecting access to public discourse for vulnerable minorities) layered with enforcement extraction (expanded institutional authority to adjudicate speech boundaries). The institution both benefits (expanded mandate, legitimacy from protecting vulnerable groups) and bears costs (difficult factual determinations, litigation risk, boundary-policing overhead). Constrained exit because the institution is obligated to enforce constitutional interpretation, but also has some agency in how aggressively to prosecute borderline cases.
constraint_indexing:constraint_classification(dignitary_harm_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS ADVOCACY COALITION (ROPE) — Organized advocates (civil rights organizations, anti-hate-speech coalitions, vulnerable minority coalitions) experience the dignitary harm reading as pure coordination: protecting epistemic access and social participation for members. The restriction mechanism is the beneficiary's own framing of what justice requires. High arbitrage (can shift strategies, scale enforcement pressure, litigate test cases). Minimal experienced extraction because the coordination function aligns with the beneficiary's core mission.
constraint_indexing:constraint_classification(dignitary_harm_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / DIGNITARY HARM FRAME (TANGLED ROPE) — At civilizational scope, the analytical observer sees both genuine coordination (protection of vulnerable groups from epistemic injustice and systemic harassment) and extractive overhead (institutional authority to police speech boundaries, chilling effects on marginal political expression, potential weaponization of hate-speech law against minority speech). The reading cannot be reduced to pure coordination because enforcement creates asymmetric power concentration. Cannot be reduced to pure extraction because the coordination function (enabling epistemic participation by historically silenced groups) is structurally real.
constraint_indexing:constraint_classification(dignitary_harm_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignitary_harm_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dignitary_harm_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dignitary_harm_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignitary_harm_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignitary_harm_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The restriction mechanism creates genuine coordination benefits (protecting epistemic access and reducing systemic exclusion) but also imposes significant overhead. Speakers face chilling effects, borderline cases face prosecution risk, and institutional authority concentrates in enforcement agencies. The extractiveness reflects both the speaker's experienced cost and the institutional beneficiary's authority gain. Rising from 0.35 to 0.52 indicates increasing enforcement intensity and boundary creep. Suppression (0.58): Moderate-high. Significant barriers to expressing politically marginal or community-specific discourse exist, creating self-censorship. However, suppression is not total — many political speech categories remain protected. The 0.58 value reflects that the restriction does suppress expression categories that would be protected under the near-absolutist reading, but less comprehensively than historical censorship regimes. Theater ratio (0.38): Low. The restriction mechanism has a clear functional purpose (preventing demonstrable dignitary harm from bigoted speech) and the institutional machinery is tied to that purpose rather than to performative ritual. This distinguishes the dignitary harm reading from the balancing reading, which may produce higher theater (courts performing 'balancing' without clear criteria).
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the beneficiary (civil rights coalition, vulnerable minorities) and the marginal speaker. The beneficiary sees the restriction as pure coordination — enabling participation in a previously exclusionary discourse. The marginal speaker sees snare or extraction — facing chilling effects and self-censorship costs without clear path to exit. The enforcement institution occupies the middle ground (tangled rope) — experiencing both coordination function (protecting access) and extraction benefit (expanded authority). The analytical observer cannot reduce this to a single type because the perspectives are not just disagreements about the same constraint but reflect genuinely different structural positions: beneficiaries have high arbitrage (can shift enforcement strategies, scale pressure), speakers have low arbitrage (cannot avoid the speech environment). The perspectival gap reveals that the dignitary harm reading's classification as tangled rope is not neutral — it reflects an analytical position that weights both the coordination function and the extraction overhead equally, whereas beneficiary and speaker perspectives weight them asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position relative to the speech restriction mechanism. Vulnerable minorities/beneficiaries: low d (0.15-0.25) because they benefit from the restriction and have arbitrage options (can organize, litigate test cases, shift enforcement pressure). Marginal speakers/victims: high d (0.80-0.90) because they bear costs from the restriction and have constrained exit options (cannot avoid the speech environment, face self-censorship). Enforcement institutions: moderate d (0.50-0.55) because they both benefit (expanded authority) and bear costs (boundary-policing overhead), and have constrained but meaningful exit options (can interpret law narrowly or broadly). The sigmoid function f(d) converts these d values into experienced extractiveness multipliers. Speaker's experienced χ is amplified by the sigmoid because d is high; beneficiary's χ is dampened or inverted because d is low. The institutional enforcer's χ sits in the middle because d is moderate, reflecting tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through explicit recognition of its hybrid character. The dignitary harm reading is not claiming to be pure coordination (it acknowledges extraction overhead) nor pure extraction (it acknowledges genuine coordination function). The tangled rope classification captures this: both the coordination benefit (enabling epistemic access for vulnerable groups) and the extraction overhead (institutional authority to police speech boundaries, chilling effects) are structurally real. The mandatrophy that could arise would be if this reading tried to claim pure coordination (denying the chilling effects and enforcement asymmetries) or tried to claim pure extraction (denying the genuine harm reduction and epistemic benefit). The reading avoids this by declaring beneficiaries (those protected from dignitary harm) AND victims (marginally political speakers facing chilling effects) in the same constraint. This forces recognition of the hybrid structure. The omega variables document empirical uncertainties about whether the coordination function justifies the extraction overhead — whether demonstrable harm can be reliably measured, whether speech actually constructs (rather than merely expresses) systemic oppression, and whether enforcement is asymmetric — but these uncertainties do not collapse the classification. They refine it. The reading is a tangled rope on its current evidence; it could be reclassified if omega resolutions shift the empirical ground.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrability_threshold_ambiguity,
    'What counts as ''demonstrable'' dignitary harm, and can cumulative psychological harm (from repeated exposure to bigoted speech) be empirically distinguished from speculative offense?',
    'Longitudinal studies measuring psychological outcomes (PTSD, depression, reduced civic participation) in targeted populations exposed to systematic hate speech; comparison with control groups; analysis of confounding factors (economic stress, police violence, institutional discrimination); expert testimony standards in civil rights litigation',
    'If cumulative harm is demonstrable: restriction threshold is achievable and the constraint functions as tangled rope with legitimate coordination. If cumulative harm cannot be reliably measured: ''demonstrable'' becomes a epistemic fiction, and the constraint drifts toward pure extraction (snare) from the speaker''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrability_threshold_ambiguity, empirical, 'Demonstrability of dignitary harm vs. speculative offense').

omega_variable(
    systemic_oppression_causation,
    'Does bigoted speech construct systemic oppression (it is constitutive of the exclusionary system), or does it merely express/reinforce pre-existing systemic oppression? Does restricting speech reduce the oppressive system''s efficacy?',
    'Comparative analysis of hate-speech restriction efficacy: does speech restriction correlate with reduced discriminatory practices in housing, employment, education, policing? Causal inference from quasi-experimental variation (jurisdictions with vs. without hate-speech law). Mechanism analysis: through what pathways would speech restriction reduce material oppression?',
    'If speech is constitutive and restriction reduces material oppression: the dignitary harm reading''s classification as tangled rope is justified (genuine coordination benefit). If speech merely reinforces pre-existing systems and restriction has limited material effect: the reading becomes more extractive (higher effective χ from speakers'' perspective), approaching snare from speaker position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_oppression_causation, conceptual, 'Whether bigoted speech constructs or merely expresses systemic oppression').

omega_variable(
    minority_speech_weaponization_risk,
    'Does hate-speech law enforcement systematically target minority communities'' political speech more than majority-group speech, even when formally neutral? Does the restriction mechanism function asymmetrically?',
    'Comparative enforcement data: proportion of cases prosecuted against minority speakers vs. majority speakers for equivalent speech; analysis of prosecutorial discretion patterns; feedback from minority legal defense organizations; historical documentation of law being weaponized against civil rights movements',
    'If enforcement is genuinely symmetric: the tangled rope classification holds — the restriction has both coordination and extraction elements but is not captured. If enforcement is systematically asymmetric: the constraint is actually a sophisticated snare disguised as civil rights protection, and the reading should be reclassified downward (higher χ from minority speaker perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_speech_weaponization_risk, empirical, 'Asymmetric enforcement of hate-speech restriction targeting minority expression').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the speech_protection_boundary kernel. What distinguishes this reading from the near_absolutist_reading and the balancing_reading? Where do the disagreements locate?',
    'Specification of this reading''s foundational axiom (dignitary_harm_justifies_restriction) and comparison with axioms of sibling readings (near_absolutist: expression_never_justifies_suppression; balancing: context_determines_protection_level). Analysis of what empirical findings would favor each reading.',
    'If dignitary harm claim is well-grounded: this reading''s coherence is supported and institutional actors should adopt its framework. If dignitary harm claim is contested or empirically weak: the reading remains live as a policy choice but cannot claim to represent the kernel''s true boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Distinguishing axioms and empirical claims separating this reading from siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignitary_harm_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhr_theater_t0, dignitary_harm_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(dhr_theater_t5, dignitary_harm_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(dhr_theater_t10, dignitary_harm_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(dhr_extract_t0, dignitary_harm_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dhr_extract_t5, dignitary_harm_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dhr_extract_t10, dignitary_harm_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignitary_harm_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dignitary_harm_reading, near_absolutist_reading).
narrative_ontology:affects_constraint(dignitary_harm_reading, balancing_reading).

% DUAL FORMULATION NOTE:
% The dignitary harm reading is part of a constraint family with the near_absolutist_reading and the balancing_reading. All three are readings of the same kernel (speech_protection_boundary) with different axioms and different classifications. The dignitary harm reading claims that demonstrable dignitary harm justifies restriction; the near_absolutist claims expression is nearly inviolable; the balancing reading claims context determines protection level. These readings coexist as live interpretations. This file documents ONLY the dignitary harm reading as a clean, ε-invariant constraint. The other readings are separate files linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignitary_harm_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
