% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) National Liberation Combatant Status Reading
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   Additional Protocol I to the Geneva Conventions (1977), Article 1(4),
 *   extends the scope of international armed conflict — and with it,
 *   conditional combatant/POW status — to peoples fighting against colonial
 *   domination, alien occupation, and racist regimes in the exercise of their
 *   right to self-determination, provided the fighting force meets
 *   organizational and command criteria under Article 43. This is the
 *   national-liberation reading of the broader combatant status kernel: it
 *   sits between the state-centric reading (which categorically excludes
 *   non-state actors) and the functional-protection reading (which makes
 *   status irrelevant to baseline humane treatment). The 2001 rise reflects
 *   the post-9/11 period in which counter-terrorism framing sharpened the
 *   dispute over which groups qualify, and several major military powers
 *   hardened their non-ratification position specifically because of this
 *   article.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.58).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.62).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) National Liberation Combatant Status Reading").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '67b7a3af-9632-4bb5-bb50-0374653ce4d4').
narrative_ontology:cs_kernel_codification('67b7a3af-9632-4bb5-bb50-0374653ce4d4', fixed_text).
narrative_ontology:cs_authority_grounding('67b7a3af-9632-4bb5-bb50-0374653ce4d4', distributed).
narrative_ontology:cs_reading_relation('67b7a3af-9632-4bb5-bb50-0374653ce4d4', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('67b7a3af-9632-4bb5-bb50-0374653ce4d4', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('67b7a3af-9632-4bb5-bb50-0374653ce4d4', foundational, self_determination_struggles_generate_combatant_status).
narrative_ontology:cs_axiom_status(self_determination_struggles_generate_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('67b7a3af-9632-4bb5-bb50-0374653ce4d4', self_determination_struggles_generate_combatant_status, deontological).
narrative_ontology:cs_axiom('67b7a3af-9632-4bb5-bb50-0374653ce4d4', secondary, organizational_command_control_suffices_absent_state_form).
narrative_ontology:cs_axiom_status(organizational_command_control_suffices_absent_state_form, holdable).
narrative_ontology:cs_axiom_grounding('67b7a3af-9632-4bb5-bb50-0374653ce4d4', organizational_command_control_suffices_absent_state_form, conventional).
narrative_ontology:cs_reference_frame('67b7a3af-9632-4bb5-bb50-0374653ce4d4', decolonization_era_self_determination_consensus).
narrative_ontology:cs_drift_state('67b7a3af-9632-4bb5-bb50-0374653ce4d4', post_9_11_counter_terrorism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('67b7a3af-9632-4bb5-bb50-0374653ce4d4', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, recognized_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, organized_insurgent_command_structures).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_administering_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, civilian_populations_in_contested_territory).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, civilian_populations_in_contested_territory).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, self_determination_doctrine).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, anti_colonial_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fighters organized under a responsible command structure resisting colonial domination, alien occupation, or racist regimes gain conditional access to POW status and combatant immunity if they meet Article 1(4)/Article 43 criteria — carrying arms openly, distinguishing themselves during military engagements, operating under an internal disciplinary system. Their access to this status depends on satisfying organizational tests that occupying powers dispute they meet.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, recognized_liberation_movements, beneficiary,
    organized, generational, constrained, regional).

% The command apparatus that must impose internal discipline and verifiable hierarchy to qualify its fighters for combatant status. It sets the terms of compliance with the laws of war internally, and its ability to do so credibly determines whether the movement's members receive protection or are treated as unlawful combatants subject to prosecution.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, organized_insurgent_command_structures, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, organized_insurgent_command_structures, agenda_setter).

% States administering occupied or colonial territory are obligated under this reading to extend combatant immunity and POW treatment to insurgents who would otherwise be treated as criminals or unlawful belligerents under domestic law. They bear the cost of releasing or humanely detaining fighters they regard as terrorists, and lose the deterrent value of criminal prosecution. Many have never ratified AP I specifically to avoid this obligation, but customary law claims and diplomatic pressure still constrain them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, biographical, constrained, national).

% States with colonial holdings at the time of Protocol I's drafting (and states inheriting similar postures) face a treaty regime explicitly built to delegitimize their characterization of independence fighters as domestic criminals. They lose the unilateral authority to define the conflict as internal law enforcement rather than international armed conflict.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_administering_states, payer,
    institutional, generational, constrained, continental).

% Benefit when combatant status incentivizes insurgent forces to distinguish themselves from civilians and follow disciplined command structures, reducing the risk of being mistaken for fighters. They pay when the relaxed distinction requirement (compared to Article 4) makes it harder for occupying forces to visually separate fighters from non-combatants, increasing risk during military operations.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, civilian_populations_in_contested_territory, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, civilian_populations_in_contested_territory, payer).

% Major military powers including the United States, Israel, and others have declined to ratify AP I substantially because of Article 1(4), arguing it politicizes the definition of combatant status and rewards groups the ratifying state considers terrorist organizations. They operate under customary law claims contested precisely because these powerful objecting states refuse to accept the norm as binding on them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, non_ratifying_states, excluded,
    institutional, generational, arbitrage, global).

% Monitor compliance with the criteria, advocate for the protective function of the provision, and assess whether specific armed groups meet the organizational threshold. They lack enforcement power but shape the interpretive record that later tribunals and states draw on.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc_and_humanitarian_monitors, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, recognized_liberation_movements).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends the coordination logic of combatant immunity (soldiers who follow the laws of war get POW protection instead of criminal prosecution) to organized non-state fighters in colonial, occupation, and racist-regime contexts, so long as they maintain command structure and distinguish themselves during engagements — incentivizing disciplined conduct in conflicts that would otherwise fall entirely outside the protective framework of the laws of war.
% TRANSFER_FUNCTION: Moves legal legitimacy and protective status from occupying/colonial/administering states (who lose the unilateral power to define the conflict as domestic criminal matter) to organized liberation movements (who gain conditional combatant immunity and POW treatment for their fighters).
% ABSENT_VOICES: Non-ratifying major military powers (the United States, Israel, and others) are structurally excluded from being bound by treaty text while still facing customary-law arguments premised on it; groups whose command structures are informal, decentralized, or contested (making the organizational threshold hard to meet) are excluded from protection even though the provision was written with irregular fighters in mind.
% DISAPPEARANCE_RATIONALE: Liberation movements and their advocates argue the world rearranges: without Article 1(4), colonial and occupation authorities would revert to treating captured fighters uniformly as criminals or unlawful combatants, eliminating a key legal lever for POW claims and diplomatic pressure. Occupying and non-ratifying states argue the world stays largely unchanged in practice, since they already treat the provision as non-binding customary law and continue prosecuting captured insurgents as unlawful combatants regardless of its formal existence.
% FOUNDING_PROBLEM: The problem of anti-colonial and national liberation wars falling entirely outside the 1949 Geneva Conventions' state-centric combatant definitions, leaving fighters against colonial domination, alien occupation, and racist regimes (the era's decolonization wars) without any path to POW status regardless of organization or conduct.
% FOUNDING_PROBLEM_CORROBORATION: Third World bloc states and the ICRC's own drafting history from the 1974-77 Diplomatic Conference attest the provision addressed a real gap for decolonization-era conflicts; however, major non-ratifying military powers and several Western international law scholars outside the beneficiary coalition attest the provision was drafted for a historical moment (active decolonization wars) that has largely concluded, and that its continued invocation in contemporary occupation and counter-terrorism contexts extends it well past its original design target.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, contested).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate-high 0.58: from the occupying/colonial-power seat, the obligation to grant combatant immunity to organized insurgents is a genuine transfer of legal and practical advantage they would not otherwise concede, and it is actively resisted (hence non-ratification by major powers). Suppression sits at 0.62 because enforcement depends on international pressure, tribunal precedent, and diplomatic recognition rather than any centralized compliance mechanism — it is real but contested and unevenly applied. Theater ratio is moderate (0.30): genuine humanitarian protective function exists for movements meeting the criteria, but a meaningful share of invocation is rhetorical political leverage in conflicts where the organizational threshold is doubtful. Accessibility collapse is lower (0.40) because states retain the practical alternative of simply not ratifying or not recognizing a given movement's qualification — the norm has not achieved anything close to universal closure.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized liberation movements and their command structures are the structural beneficiaries: the provision was drafted, over strong resistance, specifically to extend legal status to them. Occupying powers and colonial administering states are the structural targets: the obligation runs against their prior unilateral authority to define captured fighters as ordinary criminals. Civilian populations occupy an ambiguous middle position — protected by the incentive toward disciplined, distinguishable combat, but exposed by the relaxed distinction standard (compared to classical Article 4) that can make fighters harder to visually separate from non-combatants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — decolonization-era wars falling outside Geneva Convention combatant definitions — has substantially receded as a live global phenomenon since most formal decolonization concluded by the 1990s. But the article's function has not disappeared; it has been redirected toward contemporary occupation and self-determination disputes (framed differently by different parties) that its drafters plausibly did not fully anticipate. This is not simple mandatrophy (dead problem, persisting mandate) because the underlying self-determination principle it vindicates remains asserted as live in specific ongoing disputes — but the corroboration record shows genuine contestation about whether the mandate has outrun its founding target, which is exactly what the founding_problem_status: contested and the mismatch-consumption rule are built to surface rather than resolve by authorial fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_status_of_article_1_4,
    'Has Article 1(4)''s extension of combatant status to national liberation movements crystallized into binding customary international law that constrains even non-ratifying states, or does it remain conventional obligation binding only on treaty parties?',
    'Systematic survey of state practice and opinio juris among non-ratifying major military powers (US, Israel, and others); tribunal decisions (ICTY, ICC) explicitly addressing whether the provision reflects custom; ICRC customary IHL study findings on this specific rule versus contested rules.',
    'If customary, non-ratifying states are bound despite formal non-ratification, substantially raising effective extraction against occupying powers globally. If merely conventional, the constraint''s scope is limited to the roughly 174 states party to AP I, and non-ratifying powers face no direct legal obligation, only diplomatic and reputational pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_status_of_article_1_4, empirical, 'Whether Article 1(4)''s obligation binds non-ratifying states as customary law.').

omega_variable(
    organizational_threshold_manipulability,
    'Is the Article 43 organizational/command-control threshold a genuine, verifiable criterion that separates disciplined liberation forces from undisciplined militias, or is it manipulable enough that its application is effectively a political recognition decision dressed as a legal test?',
    'Comparative case analysis of tribunal and state determinations across multiple conflicts (Algeria, Rhodesia/Zimbabwe, South Africa, Western Sahara, Palestine) to assess whether the criteria are applied consistently or track political sympathy toward the movement''s cause.',
    'If genuinely verifiable, the reading functions closer to a rope with real coordination content (protecting disciplined forces, denying protection to undisciplined ones). If effectively political, the legal test is largely theater over an underlying political recognition decision, pushing the classification toward higher theater_ratio and more purely extractive dynamics against non-recognizing states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_threshold_manipulability, conceptual, 'Whether the command-control criterion is a real filter or a political proxy.').

omega_variable(
    decolonization_era_versus_contemporary_application,
    'Was Article 1(4) drafted for a historically bounded set of decolonization conflicts now largely concluded, such that its continued invocation in contemporary occupation disputes represents an extension beyond original design, or does the self-determination principle it codifies remain generatively applicable to any qualifying conflict regardless of era?',
    'Review of the 1974-77 Diplomatic Conference travaux préparatoires against the current roster of conflicts where the provision is actively invoked, cross-referenced with UN General Assembly resolution patterns on self-determination since 1990.',
    'If historically bounded, the provision''s contemporary application in occupation contexts represents doctrinal stretching that would support a mandatrophy or piton-adjacent reading over time. If the underlying principle is genuinely generative, its contemporary application is a live and legitimate extension rather than drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decolonization_era_versus_contemporary_application, conceptual, 'Whether the provision''s founding scope was era-bounded or principle-bounded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__national_liberation_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__national_liberation_reading, theater_ratio, 2001, 0.32).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__national_liberation_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(comb_tr_t2018, combatant_status_definition__national_liberation_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__national_liberation_reading, base_extractiveness, 1990, 0.47).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__national_liberation_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__national_liberation_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(comb_be_t2018, combatant_status_definition__national_liberation_reading, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__national_liberation_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__national_liberation_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__national_liberation_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(comb_su_t2018, combatant_status_definition__national_liberation_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the combatant_status_definition kernel. The state_centric_reading treats non-state actors as categorically excluded from combatant status (near-zero ε for occupying powers, since no obligation is recognized); this reading treats organized liberation movements as conditionally included (moderate ε for liberation movements, high ε for occupying powers who must now recognize an obligation they would not otherwise accept). The functional_protection_reading sidesteps combatant status entirely by grounding protection in Common Article 3's status-independent minimums, producing a different ε profile again (low ε, since baseline humane treatment is less contested than elevated POW status). The three stories share the fixed Article 1-4/Article 3/Article 4 kernel text but diverge sharply on who counts, what they are owed, and who bears the cost of extending it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
