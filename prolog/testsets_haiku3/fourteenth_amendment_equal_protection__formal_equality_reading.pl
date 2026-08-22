% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Fourteenth Amendment Equal Protection — Formal Equality Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The Fourteenth Amendment Equal Protection Clause is a contested kernel.
 *   This JSON instantiates the FORMAL EQUALITY READING: a constitutional
 *   doctrine holding that the Equal Protection guarantee prohibits explicit
 *   state racial and status classifications absent compelling justification.
 *   Under this reading, the state violates Equal Protection when it sorts
 *   citizens by protected characteristics for corrective purposes; the
 *   Amendment demands governmental color-blindness and formal neutrality. The
 *   competing anti-caste reading holds that Equal Protection mandates active
 *   dismantling of hierarchy through state corrective action — a
 *   fundamentally different structural claim. This constraint
 *   (formal-equality reading) benefits political majorities and formally
 *   unclassified dominant groups by preventing corrective-action programs; it
 *   extracts from groups subject to state corrective action by denying them
 *   access to explicit remedies. The formal-equality reading treats
 *   structural inequality as pre-constitutional background, not as a harm the
 *   Amendment addresses. The constraint is claimed as tangled_rope because it
 *   combines a genuine coordination function (uniform national rule against
 *   explicit classifications) with asymmetric extraction (benefiting some
 *   groups at the expense of others prevented from remedial access).
 *
 * KEY AGENTS:
 *   - Supreme Court majority — sets and enforces the formal-equality rule through doctrine
 *   - State legislatures and executives — constrained by judicial review; cannot implement race-conscious corrective action
 *   - Groups subject to state corrective action — bear the cost of structural inequality without access to explicit remedies
 *   - Formally unclassified dominant groups — benefit from protection against corrective classification
 *   - Political majority coalitions — benefit from avoiding explicit race-conscious governance
 *   - Civil-rights advocates and competing-reading constituencies — excluded from the adjudicating seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.68).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.72).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Fourteenth Amendment Equal Protection — Formal Equality Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'f3998145-f15c-419c-b331-a2c03a1c8c76').
narrative_ontology:cs_kernel_codification('f3998145-f15c-419c-b331-a2c03a1c8c76', fixed_text).
narrative_ontology:cs_authority_grounding('f3998145-f15c-419c-b331-a2c03a1c8c76', lineage).
narrative_ontology:cs_interpretation_layer_present('f3998145-f15c-419c-b331-a2c03a1c8c76').
narrative_ontology:cs_reading_relation('f3998145-f15c-419c-b331-a2c03a1c8c76', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('f3998145-f15c-419c-b331-a2c03a1c8c76', foundational, state_colorblindness_principle).
narrative_ontology:cs_axiom_status(state_colorblindness_principle, holdable).
narrative_ontology:cs_axiom_grounding('f3998145-f15c-419c-b331-a2c03a1c8c76', state_colorblindness_principle, deontological).
narrative_ontology:cs_axiom('f3998145-f15c-419c-b331-a2c03a1c8c76', foundational, formal_equality_sufficient_remedy).
narrative_ontology:cs_axiom_status(formal_equality_sufficient_remedy, holdable).
narrative_ontology:cs_axiom_grounding('f3998145-f15c-419c-b331-a2c03a1c8c76', formal_equality_sufficient_remedy, conventional).
narrative_ontology:cs_axiom('f3998145-f15c-419c-b331-a2c03a1c8c76', secondary, corrective_action_constitutes_invidious_classification).
narrative_ontology:cs_axiom_status(corrective_action_constitutes_invidious_classification, holdable).
narrative_ontology:cs_axiom_grounding('f3998145-f15c-419c-b331-a2c03a1c8c76', corrective_action_constitutes_invidious_classification, deontological).
narrative_ontology:cs_reference_frame('f3998145-f15c-419c-b331-a2c03a1c8c76', textual_colorblindness_principle).
narrative_ontology:cs_drift_state('f3998145-f15c-419c-b331-a2c03a1c8c76', contemporary_affirmative_action_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3998145-f15c-419c-b331-a2c03a1c8c76', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, political_majority_coalitions).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, formally_unclassified_dominant_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, groups_subject_to_state_corrective_action).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, victims_of_structural_inequality_excluded_from_remedy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, originalist_constitutional_scholars).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_legislatures_executives).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and adjudicates the Fourteenth Amendment Equal Protection Clause. In the formal equality reading, articulates and enforces the rule that explicit state racial/status classifications require compelling justification, strikes down affirmative action programs failing strict scrutiny, and treats structural inequality as pre-constitutional background. Sets the terms of constitutional permissibility for all state actors.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Bear the constraint through judicial nullification of remedial programs, reversal of affirmative action admissions and contracting policies, and the obligation to police their own classifications against strict scrutiny review. Cannot implement race-conscious corrective action without triggering invalidation. Face escalating legal risk from civil-rights litigation challenging diversity programs.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_legislatures_executives, payer,
    institutional, biographical, constrained, national).

% Face structural inequality rooted in slavery, segregation, and ongoing discrimination but are excluded from race-conscious remedies by the formal-equality reading. The constraint's operation prevents the state from explicitly accounting for their historical disadvantage in admissions, contracting, or hiring. They bear the cost of subordination without access to the tools the Equal Protection framework permits to address it.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, groups_subject_to_state_corrective_action, payer,
    powerless, generational, trapped, national).

% Benefit from the constraint by being protected against state corrective action targeting their historical advantage. They do not face explicit classifications that would trigger strict scrutiny; the formal-equality rule allows them to accumulate advantage through ostensibly neutral policies (legacy admissions, residential segregation, wealth transfer) that the reading treats as pre-constitutional. They access educational, professional, and economic goods without the risk of competing against corrective-action beneficiaries.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, formally_unclassified_dominant_groups, beneficiary,
    powerful, generational, arbitrage, national).

% Dominate legislatures and executive branches and benefit from the constraint's operation by avoiding the political cost of explicit race-conscious governance. The formal-equality reading permits them to maintain political coalitions without the friction of affirmative action programs; they can claim neutrality and colorblindness while the existing distribution of advantage persists.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, political_majority_coalitions, beneficiary,
    organized, biographical, arbitrage, national).

% Lose access to a constitutional tool they view as necessary to address structural inequality. The formal-equality reading constrains their remedial strategies and closes off corrective-action pathways. They are excluded from the seats that author the constraint's interpretation — their competing reading is not in the room as adjudicating authority, only as litigant.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocates, excluded).

% Apply the formal-equality rule as precedent; they are bound by Supreme Court doctrine even where district or circuit-level judges see the anti-caste reading as more faithful to the Amendment's history. They implement the constraint through decisions on admissions challenges, contracting disputes, and employment discrimination cases.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, lower_federal_courts, observer,
    institutional, biographical, constrained, national).

% Gain institutional authority and judicial platform from the formal-equality reading, which aligns with their hermeneutical claims about the text's meaning and historical ratification context. They produce scholarship and expert testimony supporting the reading; their professional incentive aligns with the constraint's perpetuation.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, originalist_constitutional_scholars, beneficiary,
    organized, biographical, mobile, national).

% Hold the anti-caste reading: Equal Protection requires active dismantling of hierarchy through state corrective action. They are structurally excluded from the seat of constitutional authorship; their interpretation does not adjudicate. They must contest the formal-equality reading through litigation and political pressure, without control over the Court's doctrine.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, competing_reading_constituencies, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, formally_unclassified_dominant_groups).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional rule against explicit state racial and status classifications, creating uniform standards for state conduct and judicial review across the nation. Coordinates expectation that government will treat all citizens without regard to race, ethnicity, or other suspect classifications. Provides a clear bright-line rule for what state action is permissible without requiring individualized justification for each social hierarchy.
% TRANSFER_FUNCTION: Moves constitutional legitimacy and political cover away from race-conscious remedies and toward ostensibly neutral policies. Transfers remedial authority from explicit corrective action (state-as-corrector) to market and private institutions. Allocates to dominant groups and majority coalitions the advantage of not being subject to corrective classification, while allocating to subordinated groups the burden of addressing structural inequality through non-explicit means.
% ABSENT_VOICES: The anti-caste reading and its constituencies are excluded from the adjudicating seat. Civil-rights organizations, critical race theorists, descendants of slavery and segregation who seek explicit corrective action, and scholars arguing the formal-equality reading serves as cover for perpetuated hierarchy are not in the room where the constraint is interpreted and applied. They appear only as litigants objecting to particular applications, not as co-authors of the framework.
% DISAPPEARANCE_RATIONALE: If the formal-equality reading and its precedent vanished overnight, state legislatures would immediately authorize affirmative action programs, universities would use race-conscious admissions, and contracting and employment remedies would expand. The educational and professional landscape would reorganize to account for historical disadvantage. The constraint's disappearance would remove the legal barrier preventing the state from pursuing corrective action; the fact that it persists depends on this reading's operation and the Supreme Court's enforcement of it.
% FOUNDING_PROBLEM: Reconstruction-era states were using explicit racial classifications to subordinate freed slaves and their descendants. The Fourteenth Amendment was drafted to prohibit such classifications and establish formal legal equality. The founding problem was state-authored racial oppression through explicit law.
% FOUNDING_PROBLEM_CORROBORATION: The reading's proponents cite Reconstruction history: explicit Jim Crow statutes classifying by race for segregation and subordination are the historical evil the Amendment addressed. The competing anti-caste reading argues that explicit subordination is only one form the problem takes; the Amendment was also written to address the underlying hierarchy that continues through ostensibly neutral means. Historical sources support both readings: the Amendment's text condemns 'the making or enforcement of any law which shall abridge the privileges or immunities'; Amendment framers testified about ending racial oppression AND about establishing corrective capacity. Academic consensus is divided; external corroboration from historians and constitutional scholars is split between the two readings.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The formal-equality reading extracts substantially (0.68 at interval end) because it operates to prevent remedial programs and protects existing advantage from corrective action. Suppression is high (0.72) because the reading must actively be maintained against the competing anti-caste interpretation; lower courts must police against applications that slip toward remedial reasoning, and political actors must resist the logic of corrective action. Theater is moderate (0.41) because the colorblindness principle is genuinely invoked in hundreds of judicial decisions yearly, but an increasing share of that activity defends the reading against the competing interpretation rather than addressing the claimed principle (neutrality) itself. The measurement series show rising extraction and suppression over the interval, reflecting decades of litigation solidifying the formal-equality doctrine and simultaneously building doctrinal walls against anti-caste reasoning. Accessibility_collapse (0.78) reflects that once the formal-equality rule is understood, corrective-action alternatives collapse legally; resistance (0.62) reflects sustained litigation and scholarly challenge from civil-rights constituencies, but the challenge has not dislodged the reading from the adjudicating seat.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's formal-equality seat, the constraint is a neutral rule against governmental classification — it treats all groups equally by forbidding race-consciousness. From the state-legislator seat constrained by the rule, it is a barrier to remedial action. From the powerless groups excluded from corrective action, it is an extraction mechanism that prevents the state from accounting for their structural inequality. From the dominant-group seat, it is a protection from corrective classification. From the civil-rights seat that endorses the competing reading, it is a false neutrality that perpetuates hierarchy by forbidding explicit response to it. The engine should compute these divergent types from the structural data: the formal-equality reading generates mountain-like appearance from the adjudicating seat (neutral rule, no extraction) and snare-like appearance from the victim seat (extraction through prevention of remedy). The authored metrics (0.68 extractiveness, 0.72 suppression) reflect the system-level operation, not the adjudicating seat's self-perception.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court majority holds the agenda-setter role: d near analytical (0.0), as the Court is positioned as the neutral interpreter of constitutional text. State actors pay the constraint through nullification of their remedial programs; they occupy d near 0.85 (institutional power tempered by constrained exit — they cannot ignore Supreme Court doctrine). Groups subject to corrective action are trappedu and powerless; they occupy d near 1.0 (full targets). Formally unclassified dominant groups benefit without paying; they occupy d near 0.0 (beneficiaries). The beneficiary/victim structure is asymmetric by design: the constraint coordinates a national rule (genuine coordination) while asymmetrically protecting some groups from corrective classification (pure extraction from the perspective of those excluded from remedy). This is the structural signature of tangled_rope: coordination + asymmetric extraction + active enforcement required to prevent the alternative (anti-caste) interpretation from taking hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The formal-equality reading addresses a founding problem (Reconstruction-era explicit racial classifications by state) that is genuinely dead. Jim Crow explicitly classified by race for subordination; that practice ended 60+ years ago. The constraint persists not because the founding problem is live, but because it now serves a different function: blocking corrective-action programs and protecting majority coalitions from political cost. This is mandatrophy — the constraint's mandate has outlived its function. The reading was justified as a tool against explicit oppression; it now operates as a tool against explicit remediation. The tangled_rope classification captures this: the coordination function (uniform national rule against classifications) is real, but it has become a vehicle for extraction (asymmetric protection of existing advantage from remedial response). The theater_ratio rising toward 0.41 reflects mandatrophy operating: increasing share of judicial work is devoted to maintaining the distinction between permissible neutral/background and impermissible explicit, a distinction that makes sense only if the founding problem were still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblindness_vs_structural_inequality,
    'Is formal governmental neutrality (colorblindness) the correct interpretation of Equal Protection, or does the Amendment require active state response to structural inequality rooted in the state''s own prior subordination?',
    'Historical-originalist analysis: examine Reconstruction framers'' intentions and the Amendment text''s language (''equal protection of the laws'' — does ''protection'' connote mere non-discrimination or also corrective action?). Empirical analysis: measure whether colorblind policies produce equal outcomes versus whether they perpetuate historical disadvantage under cover of neutrality.',
    'If colorblindness is correct, the formal-equality reading stands and corrective action is unconstitutional overreach. If the Amendment requires corrective response to structural inequality, the anti-caste reading becomes the proper interpretation and the formal-equality reading is revealed as a false neutral that serves extractive purposes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colorblindness_vs_structural_inequality, conceptual, 'Whether formal equality (colorblindness) or substantive equality (corrective action) is the Amendment''s true meaning.').

omega_variable(
    neutral_background_vs_state_subordination,
    'Is the existing distribution of advantage (legacy admissions, segregated housing, inherited wealth) properly characterized as pre-constitutional background or as the ongoing effect of state subordination that continues unless corrected?',
    'Historical trace: document the extent to which ostensibly neutral policies (zoning, taxation, university practices) were designed or deployed with racially subordinating intent. Counterfactual: what would the distribution of advantage look like in a world where the state had never engaged in explicit subordination?',
    'If existing advantage is truly neutral background, the formal-equality reading''s treatment of it as non-justiciable is appropriate. If existing advantage is the residue of state subordination, the formal-equality reading''s refusal to address it through corrective action becomes structurally complicit in perpetuating that subordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutral_background_vs_state_subordination, empirical, 'Whether advantage stems from neutrality or from ongoing state subordination.').

omega_variable(
    institutional_lock_in_reading_dominance,
    'Does the formal-equality reading persist as the correct constitutional interpretation, or does it persist because institutional actors (judges, scholars, politicians) have become locked into it through professional identity, precedent sunk costs, and political coalitions that benefit from its operation?',
    'Comparative institutional analysis: compare the formal-equality reading''s adherents to the anti-caste reading''s adherents, controlling for institutional position (judges in safe seats vs. contested ones; scholars with different professional incentive structures). Trace the history of doctrinal shifts (e.g., did individual justices change views when they moved seats?). Examine counterfactual: if the Court had adopted the anti-caste reading in 1986, would the formal-equality reading ever have developed institutional dominance?',
    'If lock-in is substantial, the formal-equality reading represents institutional path-dependency and professional incentive alignment, not constitutional truth. The constraint would shift from tangled_rope (coordination + asymmetric extraction) to closer-to-piton (maintained by inertia and beneficiary interest, losing functional justification). If the reading reflects genuine constitutional commitment independent of lock-in, institutional persistence is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in_reading_dominance, conceptual, 'Whether formal-equality reading persists from institutional-lock or from constitutional correctness.').

omega_variable(
    suppression_internalized_vs_structural,
    'Is the measured suppression (0.72) sustained by external legal barriers (the threat of case reversal, the force of precedent) or by internalized belief in the formal-equality reading''s correctness?',
    'Decompose suppression post-exit: if a judge or scholar moved to a jurisdiction where the anti-caste reading was dominant, would their thinking shift? Interview data on judicial and scholarly reasoning: how often is the formal-equality reading chosen because it is believed correct versus because it is institutional doctrine? Comparative jurisdictional analysis: do judges in different institutional environments reason about equal protection differently?',
    'If suppression is largely structural, removing legal barriers (e.g., through a constitutional amendment) would enable rapid shift to the anti-caste reading. If suppression is largely internalized, even legal permission would not dislodge the reading because the actors themselves are committed to it. This affects the cost of constraint removal and the trajectory of potential mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Whether suppression is maintained by external legal structure or internalized commitment.').

omega_variable(
    kernel_reading_identity_formal_vs_anti_caste,
    'This constraint instantiates the formal-equality reading of the contested Fourteenth Amendment kernel. The alternative reading (anti-caste) construes Equal Protection as requiring active dismantling of hierarchy through state corrective action. Are these readings genuine alternatives derivable from the text and history, or does one logically foreclose the other within a single coherent constitutional framework?',
    'Originalist analysis: examine whether both readings are defensible from Reconstruction-era sources and framers'' intent. Textual analysis: can ''equal protection'' bear both meanings, or does one interpretation exhaust the text''s meaning? Legal-philosophical analysis: within a single framework of constitutional authority, can both readings coexist, or are they incompatible commitments?',
    'If both readings are genuinely alternative construals, they coexist as competing interpretations held by different parties (coexists_with relation). If one logically forecloses the other, the relation is forecloses. This determines whether the kernel contest is an irresolvable disagreement or a logical contradiction. The classification affects how the engine models the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_formal_vs_anti_caste, conceptual, 'Logical status of formal-equality vs. anti-caste readings within the Equal Protection kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(four_tr_t20, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(four_tr_t30, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(four_tr_t50, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(four_be_t10, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(four_be_t20, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(four_be_t30, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(four_be_t50, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(four_su_t20, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(four_su_t30, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(four_su_t50, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__formal_equality_reading, 0.12).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% The Fourteenth Amendment Equal Protection Clause is a contested kernel with at least two structurally distinct readings. The formal-equality reading (this story) holds that Equal Protection prohibits explicit state racial/status classifications absent compelling justification, treating the constraint as a neutral rule against governmental discrimination. The anti-caste reading (sibling constraint) holds that Equal Protection requires active dismantling of racial, gender, and status hierarchy through state corrective action. These readings produce different ε values (the formal-equality reading extracts substantially by preventing corrective action; the anti-caste reading has lower extraction as it enables remedial tools), different beneficiary/victim structures (formal-equality benefits dominant groups and majorities; anti-caste benefits subordinated groups), and different classifications (formal-equality computes as tangled_rope; anti-caste should compute differently). Both readings share the same kernel text (the Fourteenth Amendment) and historical origins (Reconstruction) but instantiate fundamentally different constraints. They are linked via this network edge because the Supreme Court's adoption of one reading forecloses or constrains the institutional adoption of the other. As a matter of practical constitutional law, the formal-equality reading's dominance prevents the anti-caste reading from becoming adjudicating authority, even though both readings remain live as competing normative proposals. The family is ordered: formal-equality → anti-caste (the formal-equality reading's institutional dominance influences the anti-caste reading's development and reception).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
