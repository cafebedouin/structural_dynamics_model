% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: State Affirmative Duty to Eliminate Religious Practices Oppressing Marginalized Groups (Reformist Reading)
 *   domain: constitutional_law/religious_governance/political_theory
 *
 * SUMMARY:
 *   The reformist reading of constitutional secularism claims that the
 *   secular state has an affirmative duty to eliminate religious practices
 *   that oppress marginalized groups — particularly scheduled castes,
 *   religious minorities within orthodox traditions, and women subject to
 *   patriarchal religious authority. This reading supersedes claims of
 *   religious autonomy when autonomy claims are used to shield oppressive
 *   practices from state scrutiny. The constraint exhibits high extraction
 *   from the victim set (religious conservatives and faith communities
 *   resisting intervention) coupled with genuine coordination benefits for
 *   beneficiaries (the oppressed within religious communities and the state
 *   apparatus gaining legitimacy). The reading is extractive because it
 *   confiscates institutional autonomy from religious authorities and
 *   requires sustained suppressive enforcement. It is coordinating because it
 *   genuinely protects vulnerable agents from harm within their own
 *   communities. The theater ratio remains low (0.35) because the enforcement
 *   mechanism is relatively direct: courts order practices discontinued,
 *   regulatory bodies impose oversight, state agents monitor compliance.
 *   Unlike pure theater constraints (performative ritual with no real
 *   enforcement), this reading's enforcement is functional, though its
 *   sustainability is contested (see omega variables). This is one reading of
 *   a contested constitutional kernel; distinct from the strict-neutrality
 *   reading (state must not prefer secular to religious law) and the
 *   principled-intervention reading (state may intervene only when preventing
 *   direct harm, not to enforce gender equality or caste abolition within
 *   religion). The reformist reading is the most extractive and most
 *   expansive in scope.
 *
 * KEY AGENTS:
 *   - Scheduled Castes and Oppressed Communities: Primary beneficiaries (powerless/trapped) — the reformist reading exists to protect them from ritual-based discrimination enforced through community authority
 *   - Religious Conservatives and Tradition-Defending Leadership: Primary victims (organized/constrained) — their institutional autonomy and authority over community practice is the mechanism overridden by the duty
 *   - Religious Minorities and Internal Reformists: Secondary beneficiaries and mixed actors (moderate/constrained) — gain protection from the duty but face community sanctions for cooperation with state inspection
 *   - The State and Its Enforcement Apparatus: Institutional beneficiary and enforcer (institutional/mobile) — gains legitimacy and reach; also bears administrative costs and political friction
 *   - Rights-Based Constitutional Scholars and Advocates: Epistemic beneficiaries (analytical/arbitrage) — the reading is their intellectual and professional property; advise state enforcement bodies
 *   - Liberal Democratic Internationalism: Institutional maintainer (powerful/mobile) — the doctrine persists through Human Rights Watch, UN bodies, law schools; faces declining enforcement capacity and cultural pluralism backlash
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.58).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.72).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "State Affirmative Duty to Eliminate Religious Practices Oppressing Marginalized Groups (Reformist Reading)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/religious_governance/political_theory").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '9155a1f9-4481-4682-acfe-4a8eb608c24a').
narrative_ontology:cs_kernel_codification('9155a1f9-4481-4682-acfe-4a8eb608c24a', formalized).
narrative_ontology:cs_authority_grounding('9155a1f9-4481-4682-acfe-4a8eb608c24a', lineage).
narrative_ontology:cs_interpretation_layer_present('9155a1f9-4481-4682-acfe-4a8eb608c24a').
narrative_ontology:cs_reading_relation('9155a1f9-4481-4682-acfe-4a8eb608c24a', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('9155a1f9-4481-4682-acfe-4a8eb608c24a', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('9155a1f9-4481-4682-acfe-4a8eb608c24a', foundational, state_affirmative_protective_duty).
narrative_ontology:cs_axiom_status(state_affirmative_protective_duty, holdable).
narrative_ontology:cs_axiom_grounding('9155a1f9-4481-4682-acfe-4a8eb608c24a', state_affirmative_protective_duty, deontological).
narrative_ontology:cs_axiom('9155a1f9-4481-4682-acfe-4a8eb608c24a', foundational, harm_based_religious_practice_scrutiny).
narrative_ontology:cs_axiom_status(harm_based_religious_practice_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('9155a1f9-4481-4682-acfe-4a8eb608c24a', harm_based_religious_practice_scrutiny, empirically_contingent).
narrative_ontology:cs_reference_frame('9155a1f9-4481-4682-acfe-4a8eb608c24a', secular_state_protective_authority).
narrative_ontology:cs_drift_state('9155a1f9-4481-4682-acfe-4a8eb608c24a', contemporary_pluralism_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9155a1f9-4481-4682-acfe-4a8eb608c24a', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, religious_minorities_within_conservative_traditions).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_in_patriarchal_religious_contexts).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, faith_communities_resisting_state_intervention).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_autonomy_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCHEDULED CASTES AND WOMEN IN ORTHODOX RELIGIOUS CONTEXTS (SNARE) — Face no exit from discriminatory practices enforced through ritual authority and community exclusion. The reformist reading's affirmative duty to intervene is their only structural route out. Maximum extraction from both religious authority and state indifference. The trapped perspective sees the constraint as rescue.
constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNAL RELIGIOUS MINORITIES AND REFORMIST DISSENTERS (TANGLED ROPE) — Gain protection from the reformist duty (genuine coordination benefit) but at cost of state inspection of their religious practices and potential coercive enforcement against their own community's orthodox factions. Constrained by strong community sanctions against cooperation with state intervention. Mixed extraction and coordination.
constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED RELIGIOUS CONSERVATIVES AND TRADITION-DEFENDING LEADERSHIP (SNARE) — See the affirmative duty as confiscatory extraction of their authority over community practice. They are organized enough to mount legal resistance and maintain institutional capacity, but structurally trapped by the reading's core claim: the state has legitimate authority to override their autonomy claims when practices oppress marginalized groups. High suppression and high extraction (suppression of alternative authority claims, extraction of institutional autonomy).
constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE AND ENFORCEMENT APPARATUS (TANGLED ROPE) — Gains legitimacy and institutional reach through the affirmative duty (coordination benefit: the reading provides the doctrinal foundation for state intervention). Also bears coordination costs: monitoring religious practice is administratively expensive and creates friction with organized religious communities. Mobile exit option derives from the state's ability to selectively enforce or relax the duty depending on political coalition.
constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHTS-BASED CONSTITUTIONAL SCHOLARS AND ADVOCATES (ROPE) — Institutional beneficiaries (career, publishing, influence) of the reading's doctrinal framework. The affirmative duty doctrine is their intellectual and professional property. Arbitrage exit (can move to other doctrines if political tide shifts). Experience the constraint as pure coordination: advancing human rights law. Low experienced extraction because their benefits and the mechanism benefits align.
constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LIBERAL DEMOCRATIC INTERNATIONALISM AND HUMAN RIGHTS ESTABLISHMENT (PITON) — The affirmative duty doctrine was vibrant 1990–2015 (cascade from rights frameworks into constitutional text). Theater ratio has risen as enforcement encounters practical barriers: cases drag for decades, communities resist, cultural pluralism becomes unfashionable in law schools. The doctrine persists through institutional inertia (Human Rights Watch, UN bodies, law faculty positions) rather than functional verification. The international establishment sees its own doctrine as degraded — maintained because it anchors institutional identity, not because it effectively protects the marginalized.
constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_secularism__reformist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_secularism__reformist_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reading is moderately-to-highly extractive because it authorizes the state to override religious community self-governance on the ground that autonomy claims conceal oppression. The extraction flows from religious conservatives and faith communities to the state and the oppressed beneficiaries. The value reflects that the reading is not maximal extraction (pure snare would be 0.75+) because beneficiaries do obtain genuine protection and the mechanism is not purely coercive — some religious communities voluntarily reform. But it is substantially extractive because the suppression of alternative authority claims is high and the mechanism requires continuous enforcement. Suppression (0.72): High. The reading requires aggressive suppression of religious autonomy claims and enforcement against religious communities that resist. Religious courts, parallel governance systems, and community-policing mechanisms must be overridden or subordinated. The measurement trajectory (0.60 → 0.72) models enforcement intensification: as the duty is activated, suppression requirements increase because resistance hardens. Theater ratio (0.35): Low, but rising. The enforcement mechanism is functional (courts order practices ceased, regulatory bodies impose oversight) not purely performative. Theater rises over the interval as implementation encounters practical barriers (long case timelines, community resistance, cultural pluralism objections), but remains low because the core mechanism is real administrative action, not ritual. The low theater reflects genuine state capacity to enforce; the rising trajectory reflects the gap between initial enthusiasm and practical implementation.
 *
 * PERSPECTIVAL GAP:
 *   This reading demonstrates maximum perspectival divergence. The oppressed see rescue (snare perspective: the state is their only exit route). Religious conservatives see confiscation (snare perspective: their institutional autonomy is extracted). Internal reformists see protection with cost (tangled rope: genuine benefit but community sanction risk). The state sees institutional expansion (tangled rope: legitimacy gain and administrative burden). Rights advocates see doctrinal victory (rope: their framework dominates). The international establishment sees degraded doctrine (piton: the doctrine persists through institutional inertia as enforcement capacity wanes and cultural pluralism becomes unfashionable). The maximum perspectival gap reflects that the reading has restructured the very question 'who has authority to define religious practice' — different agents answer with entirely different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position relative to the constraint. Beneficiaries (scheduled castes, religious minorities, the state) experience low or negative d (high benefit relative to cost), producing low or negative f(d) values and negative or low χ (effective extraction appears protective from their perspective). Victims (religious conservatives) experience high d (high cost relative to benefit), producing high f(d) values and high χ (experienced extraction is maximal). The state's mobile exit option (can selectively enforce or relax the duty depending on political coalition) produces d ≈ 0.48, intermediate f(d) ≈ 0.60, and moderate χ. Religious conservatives' constrained exit (can resist in courts but face structural barriers to full legal or institutional autonomy) produces d ≈ 0.75, f(d) ≈ 1.25, and high χ. The reformist reading's efficacy depends on the state maintaining enforcement capacity; political shifts that shift political coalitions away from protective enforcement degrade the constraint toward Piton (performative due-process theater without real protection).
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading resolves mandatrophy by explicitly claiming that coordination and extraction are both present and both legitimate. The coordination benefit is genuine: internal reformers and the oppressed do gain protection from discriminatory practices. The extraction is also real: religious conservatives lose institutional autonomy and face suppressive enforcement. The reading mandatrophically claims: 'The extraction is justified because it prevents greater harm to the oppressed; the coordination benefit is real because it protects vulnerable agents.' This is not a sleight of hand — it is a coherent normative claim. The tangled_rope classification captures this duality. However, the reading is vulnerable to challenge from two directions: (1) strict-neutrality critics argue the extraction is unjustified because the state should remain neutral on religious truth claims; (2) conservative critics argue the coordination benefit is illusory because state enforcement delegitimizes internal reformers. The omegas document these challenges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oppression_identification_mechanism,
    'Who determines whether a religious practice constitutes oppression justifying state intervention? The reformist reading assumes an objective standard; critics argue this conceals value imposition.',
    'Comparative analysis of how different adjudicating bodies (constitutional courts, human rights bodies, religious authorities) classify the same practices. Examination of whether ''oppression'' classifications track material harm or cultural disapproval.',
    'If identification is genuinely objective: the reformist reading''s extraction mechanism is legitimate coordination enforcement. If identification tracks cultural dominance: the reading is extractive disguised as protective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oppression_identification_mechanism, conceptual, 'Objective vs. culturally-contingent basis for identifying oppressive practices').

omega_variable(
    internal_dissent_capture,
    'Does state intervention on behalf of internal religious minorities amplify or suppress authentic internal reform movements within faith communities?',
    'Historical case studies: comparative trajectory of reformist movements with and without state backing. Interview data from internal reformists in jurisdictions with vs. without affirmative-duty doctrines.',
    'If state backing strengthens internal reform: coordination benefit is real. If state backing delegitimizes internal reformers as state proxies: the mechanism is counterproductive to its ostensible goal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_dissent_capture, empirical, 'Whether state intervention amplifies or suppresses internal religious reform').

omega_variable(
    secular_state_legitimacy_grounding,
    'On what normative grounds does the reformist reading claim the secular state has authority to judge religious practice? Religious conservatives ground authority in divine mandate; the reading grounds it in harm-prevention and equality. These are incommensurable.',
    'Identification of which axiom (the foundational normative claim distinguishing this reading from its siblings) depends on this choice. Clarification of whether the reading forecloses or coexists with alternative legitimacy groundings.',
    'If religious and secular grounds are logically incommensurable (neither proves the other wrong): the reading coexists with strict-neutrality reading. If the harm-prevention ground forecloses divine-mandate grounding: the reading forecloses the conservative position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_state_legitimacy_grounding, conceptual, 'Whether secular and religious legitimacy grounds are logically commensurable').

omega_variable(
    reading_instantiation_contingency,
    'Is this reformist reading one possible interpretation of a constitutional secularism kernel, or is it the interpretation the kernel was designed to encode?',
    'Historical analysis of constitutional texts and legislative intent. Examination of whether the kernel permits multiple readings or constrains interpretation to the reformist reading.',
    'If multiple readings are permitted: the reading is one among several equally valid ones (coexists_with siblings). If the kernel was designed to encode this reading: the alternatives are precluded (forecloses siblings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_contingency, empirical, 'Whether this reading is one of multiple valid kernel interpretations or the intended encoding').

omega_variable(
    marginalized_group_autonomy_paradox,
    'Can the reformist reading protect the autonomy of marginalized groups within religious communities while also justifying state override of those communities'' self-governance? The paradox is fundamental: the reading claims to maximize autonomy for the oppressed while confiscating autonomy from the oppressors.',
    'Clarification of what ''autonomy'' means in the reading''s framework. Is it individual autonomy (the oppressed person''s right to exit the community without penalty)? Community autonomy (the community''s right to self-governance)? The reading must specify which takes priority and why.',
    'If individual autonomy is priority: the reading is coherent but rejects communal self-governance. If both are goods and they conflict: the reading must show how to balance them rather than simply prioritizing state intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalized_group_autonomy_paradox, conceptual, 'Autonomy paradox: protecting individuals vs. respecting communities').

omega_variable(
    reformist_enforcement_sustainability,
    'What sustains enforcement of the affirmative duty over time? Religious communities can be coercive and long-lived; state enforcement capacity is variable and dependent on political coalitions. Does the reading account for enforcement degradation?',
    'Longitudinal case analysis: does enforcement intensity increase, decrease, or oscillate over decades in jurisdictions that have adopted the reformist reading? Correlation with political composition of enforcement bodies.',
    'If enforcement is unstable: the constraint may degrade to Piton (theater without function) as enforcement capacity wanes. If enforcement is robust: the tangled_rope classification holds. If enforcement oscillates: the measurement profile should show cyclical extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_enforcement_sustainability, empirical, 'Sustainability of state enforcement capacity for affirmative duty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csrf_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(csrf_tr_t2, constitutional_secularism__reformist_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(csrf_tr_t4, constitutional_secularism__reformist_reading, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(csrf_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(csrf_be_t2, constitutional_secularism__reformist_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(csrf_be_t4, constitutional_secularism__reformist_reading, base_extractiveness, 4, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(csrf_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(csrf_su_t2, constitutional_secularism__reformist_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(csrf_su_t4, constitutional_secularism__reformist_reading, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, religious_autonomy_doctrinal_framework).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, internal_religious_reform_dynamics).

% DUAL FORMULATION NOTE:
% The constitutional secularism kernel gives rise to three structurally distinct constraints via different readings. The reformist reading (this file) is the most extractive from religious conservatives; it benefits the oppressed within religious communities and the state enforcement apparatus. The strict-neutrality reading constrains state authority to intervene. The principled-intervention reading occupies a middle position. Each reading has distinct ε, beneficiary/victim structures, and epistemic warrant. They are not observable-dependent measurements of a single constraint but rather genuinely different constraints instantiated by different interpretive communities of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
