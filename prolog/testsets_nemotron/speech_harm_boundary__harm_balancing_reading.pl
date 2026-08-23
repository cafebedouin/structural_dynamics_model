% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Harm-Balancing Reading of Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint story captures the harm_balancing_reading of the
 *   speech_harm_boundary kernel — the position that speech protection is
 *   presumptive but yields when the state demonstrates concrete harm through
 *   proportionality balancing. This reading dominates contemporary
 *   constitutional jurisprudence in Canada (Oakes test), Germany (practical
 *   concordance), South Africa, and the ECtHR. It creates a structured
 *   coordination function (protecting vulnerable groups from hate speech,
 *   harassment, group libel) while extracting compliance costs from speakers
 *   whose expression falls in expanding unprotected categories. The
 *   ε-invariance principle applies: this is a distinct constraint from the
 *   absolutist_reading (near-absolute protection, ε≈0.05) and dignity_reading
 *   (categorical unprotection of dignity-violating speech, ε≈0.55), each with
 *   different beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - targeted_groups: Primary beneficiaries (protected/vulnerable) — gain protection from hate speech, harassment, group libel
 *   - speakers_in_unprotected_categories: Primary payers (powerless/constrained) — bear restriction costs when their speech is balanced against demonstrated harm
 *   - marginal_speakers_chilled_by_balancing: Secondary payers (moderate/constrained) — self-censor near boundary due to uncertainty
 *   - controversial_publishers: Secondary payers (organized/constrained) — face licensing, prior restraint, or liability regimes
 *   - courts_balancing_authorities: Agenda setters (institutional/arbitrage) — administer the balancing test, define harm thresholds
 *   - legislatures: Agenda setters (institutional/constrained) — enact hate speech, harassment, group libel statutes subject to balancing review
 *   - civil_liberties_organizations: Observers/beneficiaries (organized/mobile) — monitor balancing for overreach, litigate boundary cases
 *   - state_security_apparatus: Beneficiaries (institutional/arbitrage) — gain regulatory tools for extremism, disinformation, public order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.38).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.42).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Harm-Balancing Reading of Speech Protection Boundary").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '08a8efaf-dbd2-445a-ab58-ebcc6acb703d').
narrative_ontology:cs_kernel_codification('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', formalized).
narrative_ontology:cs_authority_grounding('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', lineage).
narrative_ontology:cs_interpretation_layer_present('08a8efaf-dbd2-445a-ab58-ebcc6acb703d').
narrative_ontology:cs_reading_relation('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', foundational, speech_presumptive_but_yields_to_demonstrated_harm).
narrative_ontology:cs_axiom_status(speech_presumptive_but_yields_to_demonstrated_harm, holdable).
narrative_ontology:cs_axiom_grounding('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', speech_presumptive_but_yields_to_demonstrated_harm, conventional).
narrative_ontology:cs_axiom('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', foundational, proportionality_balancing_as_constitutional_method).
narrative_ontology:cs_axiom_status(proportionality_balancing_as_constitutional_method, holdable).
narrative_ontology:cs_axiom_grounding('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', proportionality_balancing_as_constitutional_method, conventional).
narrative_ontology:cs_reference_frame('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', postwar_constitutional_compromise).
narrative_ontology:cs_drift_state('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08a8efaf-dbd2-445a-ab58-ebcc6acb703d', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targeted_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, public_order_institutions).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_in_unprotected_categories).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, marginal_speakers_chilled_by_balancing).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, controversial_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, civil_liberties_organizations).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, state_security_apparatus).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, harm_principle_as_constitutional_limit).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, democratic_self_governance_requires_some_speech_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Racial, religious, gender, sexual orientation, and disability groups targeted by hate speech and harassment. They gain legal protection and state enforcement against expression that denies their dignity or incites violence against them. They cannot exit their vulnerability — their identity makes them permanent targets. The constraint's balancing test is their primary structural shield.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targeted_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Broader than targeted_groups: includes children, elderly, immigrants, refugees, and other groups disproportionately harmed by group libel, harassment, and exploitation speech. They benefit from the constraint's expansion of unprotected categories but lack political power to shape the balancing test. Their exit is identity-locked — they cannot stop being vulnerable.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, vulnerable_populations, beneficiary,
    powerless, generational, identity_locked, national).

% Police, prosecutors, intelligence agencies, and regulatory bodies that gain enforcement tools (hate speech laws, harassment statutes, extremism disruption orders) through the balancing framework. They can redirect resources across enforcement priorities (arbitrage-grade exit from specific applications) but are structurally committed to the regulatory architecture.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, public_order_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Speakers whose expression falls in judicially/legislatively defined unprotected categories: hate speakers, harassers, group libel publishers. They face criminal penalties, civil damages, prior restraint, and platform deamplification. Their exit is constrained: they can modify speech to stay within protected zones, but the boundary is defined by the balancing test they cannot control. Many are ideologically committed to their speech (identity_locked adjacent) but structurally classified as constrained because modification is formally possible.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_in_unprotected_categories, payer,
    powerless, biographical, constrained, national).

% Speakers near the boundary — activists, journalists, academics, artists, minority advocates — who self-censor because the balancing test's unpredictability makes the cost of error prohibitive. They are not targets of enforcement but bear the constraint's chilling effect. Their exit is constrained: they can speak but face disproportionate risk; they cannot access the agenda-setting power that defines the boundary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, marginal_speakers_chilled_by_balancing, payer,
    moderate, biographical, constrained, national).

% Media outlets, platforms, and publishers carrying controversial content who face licensing regimes, prior restraint orders, intermediary liability, and regulatory compliance costs. They have organizational resources to litigate but are structurally constrained by the balancing framework — they cannot exit the jurisdiction without losing audience. Their power is organized (collective litigation, industry associations) but their exit remains constrained.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, controversial_publishers, payer,
    organized, biographical, constrained, national).

% Constitutional courts, supreme courts, and human rights tribunals that administer the proportionality balancing test (Oakes test, practical concordance, ECtHR margin of appreciation). They define what counts as 'demonstrated harm,' 'pressing and substantial objective,' and 'minimal impairment.' They have arbitrage-grade exit: they can interpret factors differently across cases, distinguish precedents, and evolve the test. They neither purely benefit nor purely pay — they hold the structural pen.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, courts_balancing_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Parliaments and congresses that enact hate speech, group libel, harassment, and extremism statutes subject to judicial balancing review. They set the initial legislative boundary but are constrained by court review — they cannot fully control the constraint's shape. Their exit is constrained: they can amend laws but must survive proportionality review.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislatures, agenda_setter,
    institutional, generational, constrained, national).

% NGOs (ACLU, CCLA, Liberty, etc.) that monitor balancing for overreach, litigate boundary cases, and advocate for narrower unprotected categories. They benefit from the constraint's procedural protections (proportionality test requires state justification) but pay advocacy costs. They have mobile exit: they can shift jurisdictions, focus on different issues, or change tactics. Their role is primarily analytical/observational.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, civil_liberties_organizations, observer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, civil_liberties_organizations, beneficiary).

% Intelligence services, counter-terrorism units, and homeland security agencies that gain regulatory tools for extremism disruption, online radicalization monitoring, and disinformation countermeasures through the balancing framework's expansion. They have arbitrage exit across threat categories but are structurally committed to the regulatory architecture. They are distinct from public_order_institutions in their preventive (vs. punitive) orientation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, state_security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, courts_balancing_authorities).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of protecting vulnerable groups from hate speech, harassment, and group libel — harms that individual counterspeech cannot remedy and that markets under-provide protection against. The proportionality test coordinates state power, speaker interests, and group protection into a single adjudicative framework.
% TRANSFER_FUNCTION: Moves speech autonomy from speakers in unprotected categories (and marginal speakers chilled by uncertainty) to targeted groups and public order institutions in the form of protection from dignitary harm and regulatory authority. The state demonstrates harm; the speaker loses protection; the targeted group gains enforceable rights.
% ABSENT_VOICES: Future generations who will inherit the boundary precedents; speakers in authoritarian regimes who use balancing jurisprudence as cover for censorship; minority viewpoints that are structurally disadvantaged in 'demonstrated harm' proceedings because their speech is less legible to majority courts. These voices are excluded from the balancing calculus — they would object to the constraint's expansion but are not in the courtroom.
% DISAPPEARANCE_RATIONALE: If the harm-balancing framework vanished overnight, hate speech and harassment protections would collapse in Canada, Germany, South Africa, and ECtHR jurisdictions. Legislatures would face pressure to enact categorical bans (dignity_reading) or courts would revert to near-absolutism (absolutist_reading). The mobile software economy of speech regulation would reorganize around one of the two sibling readings — the world rearranges.
% FOUNDING_PROBLEM: Post-WWII constitutional orders needed to reconcile liberal speech protection with the lessons of Nazi propaganda and hate speech's role in genocide. The harm-balancing reading emerged as the middle path: neither the Weimar failure (insufficient regulation) nor the Soviet model (total control). It was built to prevent dignity-destroying speech while preserving a presumptive sphere of free expression.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by the UN Human Rights Committee (General Comment 34), the Venice Commission, and comparative constitutional scholars (e.g., David Beatty, Aharon Barak, Jeremy Waldron) — sources outside the direct beneficiary set (targeted groups, state institutions). Civil liberties organizations corroborate the problem is live but contest whether balancing is the solution. No major institutional actor claims the founding problem is dead; the debate is over scope, not existence.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects moderate but real compliance costs: speakers in unprotected categories (hate speech, harassment, group libel) face criminal/civil liability; marginal speakers self-censor; publishers navigate complex prior restraint regimes. Suppression (0.42) is active — enforcement machinery (hate speech tribunals, content moderation orders, criminal prosecutions) must be maintained. Theater ratio (0.18) is low: the balancing test is genuinely applied, not performative, though critics argue it masks value judgments. Accessibility collapse (0.45) is moderate: alternatives (counterspeech, private moderation, platform governance) exist but are legally subordinate to state balancing. Resistance (0.58) is significant: civil liberties challenges, platform resistance to state mandates, academic criticism of balancing's unpredictability. Claimed type tangled_rope: genuine coordination function (protecting vulnerable groups from dignitary and material harm) coexists with asymmetric extraction (speakers bear costs, state gains regulatory authority).
 *
 * PERSPECTIVAL GAP:
 *   From the targeted_groups seat (beneficiary, powerless/identity_locked), the constraint is a rope — genuine coordination solving the collective action problem of hate speech and harassment. From speakers_in_unprotected_categories seat (payer, powerless/constrained), it is a snare — extraction via criminal liability for expression the speaker views as legitimate. From courts_balancing_authorities seat (agenda_setter, institutional/arbitrage), it is a scaffold — transitional doctrine managing the transition from absolutist to dignity-based frameworks. The engine computes this divergence from structural data: beneficiaries get low directionality (d≈0.15), payers get high directionality (d≈0.85), agenda_setters sit near symmetric (d≈0.5).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: targeted_groups and vulnerable_populations gain protection from dignitary harm and material consequences of hate speech/harassment — they are structurally subsidized by the constraint (low d). Public_order_institutions gain regulatory authority — also subsidized. Victims: speakers_in_unprotected_categories bear direct restriction costs (fines, imprisonment, prior restraint) — high d. Marginal_speakers_chilled_by_balancing bear indirect costs via self-censorship — high d. Controversial_publishers bear compliance and liability costs — high d. Courts and legislatures (agenda_setters) administer the balancing; they neither purely benefit nor purely pay — they hold the structural power to define the boundary, placing them near d=0.5. Exit options differentiate: targeted_groups are identity_locked (cannot exit their vulnerability); speakers_in_unprotected_categories are constrained (can modify speech but not the rule); courts have arbitrage (can interpret balancing factors).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing harm from hate speech, group libel, harassment while preserving speech protection) remains live — new harms (online harassment, algorithmic amplification, disinformation) keep the balancing function active. However, mandatrophy risk exists in two directions: (1) mission creep where balancing expands to cover political dissent, whistleblowing, minority viewpoints (extraction accumulates, theater rises); (2) ossification where balancing becomes a ritual that always upholds restrictions (piton drift). The moderate theater ratio (0.18) suggests the coordination function is still genuine, but the rising extractiveness trajectory (0.22→0.38 over 80 years) warrants monitoring for mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the speech_harm_boundary kernel, and what would sibling readings change structurally?',
    'Comparative analysis of the three declared readings'' beneficiary/victim structures, extractiveness profiles, and coordination functions to confirm they instantiate different constraints from the same kernel.',
    'Confirms this reading''s ε-invariance: the harm_balancing_reading has moderate ε (0.38) with broader unprotected categories, while absolutist_reading would have near-zero ε and dignity_reading would have higher ε with categorical unprotection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this constraint is one reading of the speech_harm_boundary kernel, instantiating the harm_balancing_reading with siblings absolutist_reading and dignity_reading.').

omega_variable(
    harm_demonstration_threshold,
    'Where exactly does ''demonstrated harm'' threshold lie — what quantum and quality of harm triggers the override?',
    'Case law survey across jurisdictions using proportionality balancing (Canada, Germany, South Africa, ECtHR) mapping harm severity to restriction upheld.',
    'If threshold is low, constraint approaches snare (easy suppression); if high, approaches rope (genuine coordination). Current moderate ε reflects unsettled threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_demonstration_threshold, empirical, 'Structural ambiguity in the harm demonstration standard that determines extraction intensity.').

omega_variable(
    viewpoint_neutrality_of_balancing,
    'Is the proportionality balancing applied viewpoint-neutrally, or does it structurally disfavor certain speaker identities or ideologies?',
    'Empirical study of restriction outcomes by speaker category and content type; test for disparate impact on marginalized speakers.',
    'If balancing systematically targets disfavored viewpoints, the coordination function is cover and constraint is snare; if neutral, tangled_rope stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(viewpoint_neutrality_of_balancing, empirical, 'Whether the balancing mechanism itself extracts asymmetrically across speaker groups.').

omega_variable(
    chilling_effect_magnitude,
    'How much does the balancing regime chill protected speech near the boundary?',
    'Survey and behavioral studies of speakers self-censoring in balancing jurisdictions vs. absolutist jurisdictions.',
    'High chilling raises effective extraction on marginal speakers (payer seat) and suppresses alternatives, pushing toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Second-order extraction via self-censorship not captured in direct restriction metrics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t20, speech_harm_boundary__harm_balancing_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t60, speech_harm_boundary__harm_balancing_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_tr_t80, speech_harm_boundary__harm_balancing_reading, theater_ratio, 80, 0.18).

% Extraction over time
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t20, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t60, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_be_t80, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t20, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t60, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement(speech_harm_boundary__harm_balancing_reading_su_t80, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 80, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.1).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, platform_content_moderation_regimes).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, online_harassment_liability_frameworks).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, disinformation_regulation).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel decomposes into three constraint stories: absolutist_reading (ε≈0.05, rope/mountain), harm_balancing_reading (ε≈0.38, tangled_rope), dignity_reading (ε≈0.55, tangled_rope/snare). Each has distinct beneficiary/victim structures and coordination functions. The harm_balancing_reading influences both siblings: it creates the doctrinal infrastructure (proportionality test, harm demonstration standards) that the dignity_reading builds upon for categorical rules, and it provides the concession framework that the absolutist_reading must contest. All three are linked bidirectionally in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, institutional, 0.45).
constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, powerless, 0.85).
constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, moderate, 0.75).
constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
