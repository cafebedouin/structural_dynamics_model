% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: First Amendment Balancing Test for Speech Protection
 *   domain: constitutional_law/speech_regulation
 *
 * SUMMARY:
 *   The First Amendment balancing reading holds that speech protection is not
 *   absolute but determined case by case through judicial weighing of the
 *   speech interest against competing constitutional values and demonstrated
 *   harms. Emerging from the collapse of categorical rules (clear and present
 *   danger, fighting words, Chaplinsky categories), balancing became the
 *   dominant methodology from the 1960s onward — particularly for coded
 *   speech, hate speech, and systemic harm claims. The constraint coordinates
 *   speech protection by giving courts a structured framework for
 *   context-sensitive decisions, but extracts asymmetrically: speakers
 *   restricted by balancing bear concentrated costs, while vulnerable groups
 *   and government interests gain diffuse protection. The gatekeeper role is
 *   distributed across the judiciary rather than fixed in categorical rules,
 *   making judicial composition decisive. This reading coexists with
 *   absolutist and harm-limited readings in the contested kernel of the
 *   speech protection boundary.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: Primary agenda-setters (institutional/analytical) — author the balancing weights
 *   - lower_court_judges: Secondary agenda-setters / payers (institutional/constrained) — apply balancing daily
 *   - speakers_restricted_by_balancing: Primary payers (moderate/constrained) — bear concentrated restriction costs
 *   - marginalized_speakers_disfavored_by_courts: Secondary payers (powerless/trapped) — disproportionately restricted
 *   - vulnerable_groups_protected_from_harmful_speech: Primary beneficiaries (organized/identity_locked) — gain protection
 *   - government_interests_in_order: Secondary beneficiaries (institutional/arbitrage) — gain regulatory flexibility
 *   - legal_scholars: Observers (analytical/analytical) — shape intellectual legitimacy
 *   - civil_liberties_organizations: Observers (organized/mobile) — litigate and mobilize
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.45).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.35).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "First Amendment Balancing Test for Speech Protection").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, 'c334d2a0-762a-47aa-b8e9-cb72db8ded7d').
narrative_ontology:cs_kernel_codification('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', fixed_text).
narrative_ontology:cs_authority_grounding('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', lineage).
narrative_ontology:cs_interpretation_layer_present('c334d2a0-762a-47aa-b8e9-cb72db8ded7d').
narrative_ontology:cs_reading_relation('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', speech_protection_boundary__harm_limited_reading, influences).
narrative_ontology:cs_axiom('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', foundational, contextual_weighing_legitimate).
narrative_ontology:cs_axiom_status(contextual_weighing_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', contextual_weighing_legitimate, conventional).
narrative_ontology:cs_axiom('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', secondary, intermediate_scrutiny_for_coded_speech).
narrative_ontology:cs_axiom_status(intermediate_scrutiny_for_coded_speech, holdable).
narrative_ontology:cs_axiom_grounding('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', intermediate_scrutiny_for_coded_speech, conventional).
narrative_ontology:cs_reference_frame('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', first_amendment_textual_commitment).
narrative_ontology:cs_drift_state('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', contemporary_judicial_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c334d2a0-762a-47aa-b8e9-cb72db8ded7d', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, vulnerable_groups_protected_from_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, government_interests_in_order).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_restricted_by_balancing).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, marginalized_speakers_disfavored_by_courts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, lower_court_judges).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, judicial_balancing_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, contextual_speech_protection).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, intermediate_scrutiny_for_coded_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author the balancing framework through precedent; their votes determine which interests count and how heavily. They face no electoral accountability but are constrained by stare decisis and institutional legitimacy. Exit means retirement; their decisions bind all lower courts.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, supreme_court_justices, agenda_setter,
    institutional, generational, analytical, national).

% Apply Supreme Court balancing tests case by case; their discretion in weighing interests shapes outcomes on the ground. They bear reversal risk and docket pressure. Promotion depends on alignment with higher-court signals; exit to private practice is constrained by judicial ethics and career investment.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, lower_court_judges, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, lower_court_judges, payer).

% Individuals or groups whose speech is restricted after courts weigh it against competing values. They bear the direct cost of the balancing outcome — loss of platform, criminal liability, civil damages. Exit means self-censorship or litigation they often cannot afford; the chilling effect extends beyond the litigants.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers_restricted_by_balancing, payer,
    moderate, biographical, constrained, national).

% Speakers from historically disadvantaged groups whose speech is disproportionately restricted under balancing tests that weigh 'order' or 'dignity' against their expression. They lack litigation resources and face judicial premises that discount their harm. Exit is nearly impossible — the constraint shapes the very forums where they might contest it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, marginalized_speakers_disfavored_by_courts, payer,
    powerless, biographical, trapped, national).

% Communities that gain protection from hate speech, harassment, or incitement because balancing allows courts to restrict speech that targets them. Their identity is bound to the protection; they cannot 'exit' the need for it without abandoning the equality claims that define their political existence.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, vulnerable_groups_protected_from_harmful_speech, beneficiary,
    organized, generational, identity_locked, national).

% State actors (legislatures, prosecutors, regulators) who gain flexibility to regulate speech when courts accept their asserted interests in public order, national security, or administrative efficiency. They can forum-shop and reframe regulations to survive balancing; their exit options include legislative amendment or non-enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, government_interests_in_order, beneficiary,
    institutional, generational, arbitrage, national).

% Analyze, critique, and theorize the balancing framework across generations. They neither collect nor pay from the constraint's operation but shape its intellectual legitimacy. Their exit is intellectual — they can adopt alternative frameworks (absolutism, harm-limitation) without material cost.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Litigate test cases, file amicus briefs, and mobilize public opinion around balancing outcomes. They occupy a hybrid seat: they pay litigation costs but do not bear the speech restrictions directly; they benefit from favorable precedents but do not capture rents. They can shift strategy or focus to other rights without organizational death.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, civil_liberties_organizations, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves conflicts between speech and other constitutional values (equality, dignity, public order, privacy) through case-by-case judicial weighing rather than categorical rules, allowing context-sensitive protection that adapts to evolving harms and speech forms.
% TRANSFER_FUNCTION: Moves speech protection determinations from bright-line categories to judicial discretion, shifting interpretive power to courts to decide which speech gets protected and which interests prevail in each context — effectively transferring authority from legislative categorization to judicial balancing.
% ABSENT_VOICES: Future speakers chilled by precedent but not yet litigating; non-litigant communities affected by speech restrictions who lack standing; marginalized speakers without organizational or financial access to appellate courts; the 'silenced majority' whose self-censorship leaves no judicial record.
% DISAPPEARANCE_RATIONALE: If balancing vanished overnight, courts would revert to categorical rules (clear and present danger, fighting words, obscenity categories) or near-absolutist frameworks. The entire architecture of intermediate scrutiny for coded speech, contextual harm analysis, and judicially crafted exceptions would collapse, requiring legislative re-codification of speech boundaries or a shift to absolutist protection.
% FOUNDING_PROBLEM: How to protect speech in a pluralistic democracy while allowing regulation of genuinely harmful expression (incitement, harassment, threats, fraud) without granting government a blank check to suppress dissent — the categorical rules of the early 20th century proved both over-inclusive (suppressing political dissent) and under-inclusive (failing to address new harm forms).
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars across the ideological spectrum (Stone, Sunstein, Schauer, Post, Volokh) acknowledge the balancing problem persists; historical record shows categorical rules failed to address context-dependent harms like coded hate speech, digital harassment, and algorithmic amplification; even absolutist critics concede the founding problem remains unsolved by their preferred alternative.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).
:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that balancing transfers significant interpretive power to courts, enabling restriction of speech that categorical rules would protect — but the coordination function (resolving genuine value conflicts) is real, keeping ε below pure extraction. Suppression (0.35) is moderate because balancing requires active judicial enforcement but alternatives (legislative categorization, absolutism) remain conceptually available. Theater (0.25) captures performative balancing where courts recite factors but reach predictable ideological outcomes — a growing but not dominant share. Accessibility collapse (0.55) is moderate: categorical rules are conceptually accessible but politically discredited; absolutism is intellectually available but judicially marginal. Resistance (0.45) reflects sustained scholarly critique and periodic judicial pushback (e.g., categorical carve-outs for political speech).
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat, balancing is genuine coordination — a necessary framework for resolving irreducible value conflicts in a complex society. From the restricted speaker's seat, the same structure operates as asymmetric extraction — their speech is weighed against interests they cannot influence, by judges they cannot hold accountable. From the marginalized speaker's seat, extraction is amplified by identity-locked exit: the constraint shapes the forums where they might contest it. The engine computes this divergence from the structural power/exit asymmetries declared in stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Supreme Court justices are structural beneficiaries (d near 0.0) — they control the balancing weights, face no electoral accountability, and their institutional power grows with interpretive discretion. Lower court judges sit near symmetric (d ~0.5) — they wield discretion daily but bear reversal risk and career constraints. Speakers restricted by balancing are full targets (d near 1.0) — they pay the concentrated cost of restriction with constrained exit. Marginalized speakers disfavored by courts are trapped targets (d = 1.0 effectively) — identity-locked exit means they cannot escape the constraint's framing. Vulnerable groups protected from harm are identity-locked beneficiaries (d near 0.0) — their political existence is bound to the protection. Government interests are arbitrage-grade beneficiaries (d near 0.0) — they can reframe regulations and forum-shop. Legal scholars and civil liberties orgs are analytical observers (d = 0.5 by convention).
 *
 * MANDATROPHY ANALYSIS:
 *   The balancing test was built to solve the founding problem: categorical rules both over-suppressed (political dissent) and under-protected (new harm forms). That problem remains live — digital speech, coded hate, algorithmic amplification create harms no 1930s category anticipated. However, the constraint shows mandatrophy signals: theater ratio has crept up as balancing becomes ritualized; suppression requirement has stabilized rather than declined, suggesting the coordination function is not naturally attenuating. The engine's mandatrophy detection would flag the gap between the live founding problem and the ritualized application — but the claim/metric independence means this analysis does not tune the scores.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the balancing_reading a distinct constraint from the absolutist_reading and harm_limited_reading, or a single constraint measured differently?',
    'Apply ε-invariance test: if measuring speech protection via balancing weights yields a structurally different ε than measuring via categorical rules or harm thresholds, they are distinct constraints. The prompt''s structural delta (shifting boundary, distributed gatekeepers, intermediate scrutiny) confirms distinct ε.',
    'Confirms this JSON models one reading as a clean ε-invariant constraint per Rule 1; sibling readings are separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame confirmation that this is one reading of a contested kernel, not a parameterized measurement of one constraint.').

omega_variable(
    balancing_vs_categorical_boundary,
    'Where does the balancing test end and a de facto categorical rule begin — when does repeated balancing produce a settled category?',
    'Track precedent clusters: if a class of speech (e.g., true threats, commercial speech) receives consistent balancing outcomes over 20+ years, code the emergence of a categorical sub-rule and measure its ε separately.',
    'If balancing crystallizes into categories, the constraint''s coordination function shifts toward rope; if it remains open-ended, tangled_rope extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_vs_categorical_boundary, empirical, 'Whether case-by-case balancing inevitably generates its own categorical successors.').

omega_variable(
    judicial_discretion_extraction,
    'How much of the measured extractiveness stems from judicial discretion itself (ideological weighing) versus the inherent difficulty of coordinating speech boundaries?',
    'Compare outcomes across courts of different ideological composition on identical balancing factors; if variance correlates with composition, the discretion component is extractive.',
    'High discretionary variance would increase effective extraction for powerless speakers (their fate depends on judicial draw); low variance would support balancing as genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_discretion_extraction, empirical, 'Separation of coordination cost from ideological extraction in the balancing mechanism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of disfavored speech under balancing primarily structural (legal penalties, prior restraints) or internalized (chilling effect, self-censorship)?',
    'Post-restriction speech trajectory analysis: if speakers resume similar speech after favorable precedent, suppression was structural; if silence persists, internalized component is significant.',
    'If internalized, effective suppression for marginalized speakers exceeds the legal-penalty measure; the constraint''s extraction is amplified for identity-locked payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for speech-restricted payers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_balancing_tr_t1937, speech_protection_boundary__balancing_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(speech_balancing_tr_t1957, speech_protection_boundary__balancing_reading, theater_ratio, 1957, 0.2).
narrative_ontology:measurement(speech_balancing_tr_t1969, speech_protection_boundary__balancing_reading, theater_ratio, 1969, 0.22).
narrative_ontology:measurement(speech_balancing_tr_t1989, speech_protection_boundary__balancing_reading, theater_ratio, 1989, 0.24).
narrative_ontology:measurement(speech_balancing_tr_t2010, speech_protection_boundary__balancing_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(speech_balancing_tr_t2024, speech_protection_boundary__balancing_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(speech_balancing_be_t1937, speech_protection_boundary__balancing_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(speech_balancing_be_t1957, speech_protection_boundary__balancing_reading, base_extractiveness, 1957, 0.35).
narrative_ontology:measurement(speech_balancing_be_t1969, speech_protection_boundary__balancing_reading, base_extractiveness, 1969, 0.42).
narrative_ontology:measurement(speech_balancing_be_t1989, speech_protection_boundary__balancing_reading, base_extractiveness, 1989, 0.48).
narrative_ontology:measurement(speech_balancing_be_t2010, speech_protection_boundary__balancing_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(speech_balancing_be_t2024, speech_protection_boundary__balancing_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(speech_balancing_su_t1937, speech_protection_boundary__balancing_reading, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(speech_balancing_su_t1957, speech_protection_boundary__balancing_reading, suppression_requirement, 1957, 0.4).
narrative_ontology:measurement(speech_balancing_su_t1969, speech_protection_boundary__balancing_reading, suppression_requirement, 1969, 0.35).
narrative_ontology:measurement(speech_balancing_su_t1989, speech_protection_boundary__balancing_reading, suppression_requirement, 1989, 0.32).
narrative_ontology:measurement(speech_balancing_su_t2010, speech_protection_boundary__balancing_reading, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement(speech_balancing_su_t2024, speech_protection_boundary__balancing_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.1).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint (balancing_reading) decomposes the speech_protection_boundary kernel alongside absolutist_reading and harm_limited_reading. The ε values differ substantially: absolutist_reading ε ≈ 0.05 (near-zero extraction, categorical protection), harm_limited_reading ε ≈ 0.65 (high extraction, speech conditional on harm absence), balancing_reading ε = 0.45 (moderate extraction, coordination via judicial weighing). The three stories form a constraint family linked by mutual affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, institutional, 0.15).
constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
