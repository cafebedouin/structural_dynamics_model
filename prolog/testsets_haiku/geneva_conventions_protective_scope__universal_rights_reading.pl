% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Universal Geneva Protections and Human Rights Floor in Armed Conflict
 *   domain: international/legal/humanitarian
 *
 * SUMMARY:
 *   The universal rights reading of Geneva Convention protections asserts
 *   that Common Article 3 plus customary international human rights law
 *   create an absolute floor of protection for all persons affected by armed
 *   conflict, regardless of combatant status or conflict classification.
 *   Under this reading, even unprivileged belligerents (spies, saboteurs,
 *   irregular fighters) and civilians not meeting traditional combatant
 *   criteria receive full protection from torture, summary execution, and
 *   arbitrary detention. This reading benefits civilian populations and
 *   non-state armed groups; it constrains state military and intelligence
 *   operational flexibility by eliminating the legal cover for differential
 *   treatment based on combatant classification. The constraint is a tangled
 *   rope: it coordinates conduct expectations across diverse conflict parties
 *   (genuine coordination function) while simultaneously extracting
 *   operational restrictions from state militaries (asymmetric extraction
 *   from the powerful). The claim/metric gap is deliberate: this story
 *   instantiates only the universal reading; sibling readings (state-centric
 *   and hybrid proportionality) are separate constraints with their own ε
 *   values and stakeholder structures.
 *
 * KEY AGENTS:
 *   - state_military_command: institutional power, constrained exit, bears operational restrictions
 *   - state_intelligence_agencies: institutional power, constrained exit, bears accountability exposure for interrogation practices
 *   - civilian_populations: powerless, trapped, benefit nominally but enforcement depends on third-party verification
 *   - non_state_armed_groups: organized power, identity-locked, benefit from combatant status but bound by reciprocal obligations
 *   - international_criminal_court: institutional agenda-setter, enforces and interprets the constraint
 *   - icrc_and_humanitarian_bodies: powerful agenda-setter, administers the constraint through field presence
 *   - state_centric_reading_advocates: excluded institutional actors, prefer narrower protections
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.41).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Universal Geneva Protections and Human Rights Floor in Armed Conflict").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international/legal/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'a82696b1-fdd5-4df1-9324-1f98ca1e8b0d').
narrative_ontology:cs_kernel_codification('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', fixed_text).
narrative_ontology:cs_authority_grounding('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', lineage).
narrative_ontology:cs_interpretation_layer_present('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d').
narrative_ontology:cs_reading_relation('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', foundational, universal_human_dignity_non_derogable).
narrative_ontology:cs_axiom_status(universal_human_dignity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', universal_human_dignity_non_derogable, deontological).
narrative_ontology:cs_axiom('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', foundational, common_article_3_absolute_floor_all_conflicts).
narrative_ontology:cs_axiom_status(common_article_3_absolute_floor_all_conflicts, holdable).
narrative_ontology:cs_axiom_grounding('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', common_article_3_absolute_floor_all_conflicts, conventional).
narrative_ontology:cs_reference_frame('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', universal_humanitarian_protection_1949_text).
narrative_ontology:cs_drift_state('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', contemporary_post_icc_jurisdiction, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a82696b1-fdd5-4df1-9324-1f98ca1e8b0d', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detainees_unprivileged_status).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, international_human_rights_advocates).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_flexibility).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_security_doctrine_discretion).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_intelligence_detention_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_command).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_intelligence_agencies).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_security_strategists).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, human_dignity_universal_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, non_derogable_rights_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_imperative_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under a reading that treats Common Article 3 plus human rights law as an absolute floor applicable to all conflict situations, regardless of conflict classification or belligerent status. This restricts targeting decisions (must avoid strikes on detainees, medical personnel, hors de combat); requires humane treatment and due process even for unprivileged combatants; prohibits interrogation methods that violate non-derogable rights. The constraint narrows the operational menu available under older state-centric readings that distinguished between privileged and unprivileged combatants.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_command, payer,
    institutional, generational, constrained, global).

% Under the universal reading, detention and interrogation of all conflict participants must meet Common Article 3 standards (no torture, degrading treatment, or summary execution) regardless of whether the detainee meets Article 4 combatant criteria. This constrains interrogation techniques, extends procedural protections to irregular fighters, and exposes agencies to accountability for practices previously justified by unprivileged-combatant status. Exit options are constrained by treaty obligation and increasingly by human rights litigation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% Receive protection under the constraint as persons affected by armed conflict regardless of combatant status. The universal reading extends Common Article 3 protections to all civilians: no targeted killing, no summary execution, no arbitrary detention. However, the protection is often nominal — enforcement depends on third-party verification, state compliance incentives, and post-conflict accountability mechanisms, all of which are weak in ongoing conflicts. Trapped by geography and war; benefit is legal status but practical safety depends on combatants' choice to honor the reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, biographical, trapped, global).

% Under the universal reading, non-state armed groups receive the same Geneva protections as state militaries if they satisfy Common Article 3 conditions (responsible command, distinction, proportionality). This grants their members legal status as combatants (entitled to POW protections if captured) and imposes reciprocal obligations (humane treatment of prisoners, no targeting civilians). The constraint benefits them relative to state-centric readings that denied them combatant status, but also binds them to international standards. Identity-locked because organizational legitimacy increasingly rests on claim to legal/moral standing through humanitarian compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, payer).

% Under state-centric readings, detainees not meeting Article 4 criteria (spies, saboteurs, irregular fighters) fell outside Geneva protections and could be summarily executed or subjected to coercive interrogation. The universal reading extends Common Article 3 floor to them: they must be treated humanely, receive medical care, cannot be tortured or executed without trial. The benefit is nominal (detention in conflict zones is still coercive) but the constraint removes the legal justification for summary treatment and creates accountability hooks.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detainees_unprivileged_status, beneficiary,
    powerless, immediate, trapped, global).

% Advocacy organizations, international courts, and human rights bodies benefit from the universal reading because it provides legal standing to challenge state conduct across all conflict scenarios. The reading vindicates human dignity as a universal principle and creates grounds for accountability that state-centric readings denied. They use the constraint to press for compliance, document violations, and push for post-conflict justice.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_human_rights_advocates, beneficiary,
    powerful, generational, mobile, global).

% The universal reading constrains the strategic freedom to classify combatants into protected and unprotected categories, eliminating the legal cover for differential treatment based on uniform or command structure. Doctrine that historically justified broader targeting or detention authority based on combatant classification must now justify every decision by reference to the same Common Article 3 floor regardless of classification. Exit is constrained by treaty signature and customary international law status of the constraint.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_security_strategists, payer,
    institutional, generational, constrained, global).

% Military and security establishments in states that prefer state-centric reading (which permits differential treatment of unprivileged combatants, narrower victim set, broader operational discretion) are structurally excluded from shaping the constraint's interpretation. They argue for reading Common Article 3 as a minimum floor but Geneva as permitting classification-based differentiation. That argument is not in the room when the constraint is being applied — they bear the constraint but have no formal seat in its administration.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_centric_reading_advocates, excluded,
    institutional, generational, constrained, global).

% Enforces and interprets the constraint through prosecutions for war crimes, crimes against humanity, and crimes of genocide. The ICC's Rome Statute and jurisprudence instantiate the universal reading: all persons in armed conflict receive Common Article 3 protections; violations are prosecutable regardless of combatant status. The court sets standards, adjudicates disputes, and creates precedent that tightens the constraint's application over time.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_court, agenda_setter,
    institutional, generational, analytical, global).

% The International Committee of the Red Cross and humanitarian monitoring organizations administer and interpret the constraint through field operations, monitoring, and advocacy. They press for universal application of Common Article 3, document violations, and build legitimacy for the universal reading through consistent advocacy and moral authority. They shape how states understand their obligations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, icrc_and_humanitarian_bodies, agenda_setter,
    powerful, generational, mobile, global).

% Analyze the constraint's operation, identify tensions between treaty text and state practice, and document drift. Scholars can see the full structure — how states comply selectively, how they interpret Common Article 3 narrowly in practice while accepting it in principle, how non-state groups use the constraint to claim legitimacy.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, observer_international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal legal floor (Common Article 3 + non-derogable human rights) that all parties to armed conflict must respect, regardless of their status or the conflict's classification. This solves the coordination problem of preventing a race-to-the-bottom in conduct standards: if different combatant classes received different protections, each party would argue for the narrowest reading applicable to its opponents. A universal floor coordinates conduct expectations and reduces incentives to torture, execute, or abuse detainees.
% TRANSFER_FUNCTION: Transfers operational flexibility and discretion from state military and intelligence authorities to international legal standards and humanitarian principles. States must justify conduct by reference to universal standards rather than national security doctrine or combatant classification. The constraint moves authority over detention, interrogation, and targeting from state unilateral judgment to shared international standards (ICRC interpretation, ICC precedent, human rights law).
% ABSENT_VOICES: States that prefer the state-centric reading (narrower protections, classified combatant status, broader military discretion) have been largely excluded from shaping the universal reading's interpretation. Military and intelligence establishments in powerful states continue to argue for classification-based differentiation but are increasingly sidelined in formal interpretation (ICC, treaty bodies, ICRC). Their preferred reading persists in some state practice but has lost the battle for official treaty interpretation.
% DISAPPEARANCE_RATIONALE: If the universal reading and its enforcement machinery vanished, state military and intelligence operations would revert to treating unprivileged combatants and irregular detainees without Common Article 3 protections, creating immediate accountability gaps. Practices previously prosecuted (summary execution, torture of detainees not meeting Article 4 criteria) would resume legal cover. Non-state groups would lose one of their few claims to legitimacy. The protection floor would collapse to pre-1990s standards.
% FOUNDING_PROBLEM: Armed conflict historically created a legal vacuum for persons not meeting combatant criteria: spies, saboteurs, irregular fighters, and armed civilians fell outside Geneva combatant protections but were not clearly civilians either. States exploited this gap to justify summary execution and coercive interrogation. Human rights law emerged to fill the gap, establishing that ALL persons affected by conflict deserve protection from torture, summary execution, and arbitrary detention regardless of status. The founding problem was the legal excuse for abusive conduct of unclassified combatants.
% FOUNDING_PROBLEM_CORROBORATION: Documented evidence from conflict zones (Amnesty International, Human Rights Watch, UN fact-finding missions) attests that the problem remains live: states and non-state groups continue to exploit classification ambiguity to justify conduct violations. Post-conflict prosecutions (Sudan, DRC, Afghanistan) and ICC investigations confirm that without the universal reading's enforcement, gaps re-emerge. Advocacy organizations outside the state security apparatus attest that the founding problem persists and the universal reading is necessary to address it.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 in 1949 (when Geneva was fresh consensus) to 0.68 in 2025 (as human rights law hardened and ICC jurisprudence expanded application). The constraint becomes more extractive as interpretation tightens and enforcement capacity grows. Theater ratio remains low (0.22) because the functional purpose (preventing abuse of vulnerable detainees) is substantive — the constraint's operation is not mostly performative. Suppression requirement is moderate (0.41) because the constraint requires active enforcement against state resistance, but states largely comply in principle (reducing the suppressive force needed). The time series reflects historical moments: 1977 Additional Protocols expanded the reading's scope; 1998 ICC statute cemented enforcement; 2010 onwards witnessed intensified prosecutions. The measurement grid is shared across all three metrics (every metric measured at every time point) to enable lifecycle analysis.
 *
 * PERSPECTIVAL GAP:
 *   The state security seat and the civilian/ICRC seat should diverge sharply: the state perceives the constraint as extraction (reduced discretion, accountability risk); the beneficiary seats perceive it as protection (universal standards applied evenly). This divergence is structural, not opinionated — the constraint operates asymmetrically across power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   State military and intelligence agencies are full targets (d near 1.0): they bear the constraint's costs (operational restrictions, accountability exposure) and do not benefit from it. Civilian populations are near-beneficiaries (d near 0.0) but trapped, so exit options do not improve their position. Non-state armed groups sit near symmetric (d near 0.5): they benefit from combatant status but pay the price of binding obligations. International advocates are beneficiaries (d near 0.0). The derivation flows from beneficiary declarations (civilians, non-state groups, human rights advocates) and victim declarations (state operational flexibility, state discretion). No overrides are needed; the structural data produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding mandate (prevent abuse of irregular combatants and non-state fighters) remains live and substantive. The universal reading does not suffer mandate creep or function atrophy — enforcement has intensified, not declined. The constraint is not a piton (performance-laden degradation) but a tangled rope whose extraction component has grown as interpretation tightened. Theater ratio is low because the constraint's operation remains functionally tied to preventing abuse, not mostly maintaining institutional performance. No mandatrophy signal is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_state_compliance,
    'How much of observed state compliance with the universal reading reflects genuine internalization of the standard versus strategic performance (appearing to comply while evading through classification loopholes)?',
    'Post-conflict investigations, declassified interrogation records, and comparative analysis of state conduct in high-accountability vs. low-accountability settings. States comply more strictly when ICC jurisdiction is active and international monitoring is intense; slackening occurs in permissive environments.',
    'If compliance is mostly strategic (theater), the constraint''s actual extraction on state conduct is lower than the enforcement machinery suggests, and the real suppression cost is what states invest in maintaining plausible deniability. If compliance is genuine, extraction is higher because states truly restrict practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_state_compliance, empirical, 'Whether state compliance reflects internalized standards or performance under surveillance.').

omega_variable(
    universal_floor_versus_classification_practice,
    'Does the universal reading''s assertion of a single floor for all conflict participants contradict the actual practice of states and groups applying classification-differentiated standards?',
    'Field observation of how detention, interrogation, and targeting decisions are made in active conflicts; analysis of state military manuals and non-state group codes of conduct; comparison of stated interpretation (universal floor) to operational conduct.',
    'If actual practice retains strong classification-based differentiation despite the universal reading, the constraint''s real extractiveness is lower than the authored metrics suggest because states are evading through unobserved practice. The constraint would be high-suppression (requiring surveillance to maintain) rather than the authored moderate suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_floor_versus_classification_practice, empirical, 'Whether the universal floor''s assertion matches operational reality or is theatrically maintained while evasion persists.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the universal reading foreclose the state-centric reading, or do they coexist as live interpretive positions held by different institutional actors?',
    'Examine whether a party can adopt the state-centric reading''s core premise (unprivileged belligerents fall outside treaty scope) without logical contradiction within a single legal framework, or whether the universal reading''s commitment to universal rights makes that premise incoherent.',
    'True foreclosure would mean states formally accepting the universal reading have logically committed to rejecting classification-based differentiation. Coexistence would mean states can formally acknowledge the universal floor while preserving classification practice for operational purposes (which is what audit evidence suggests). Coexistence strengthens the omega — the constraint''s real function is coordination-with-evasion-tolerance, not pure protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the universal reading logically forecloses or merely competes with the state-centric reading.').

omega_variable(
    non_state_group_internalization_identity_lock,
    'For non-state armed groups, is the benefit from the universal reading''s grant of combatant status sufficient to bind them identity-locked to humanitarian compliance, or do groups strategically adopt the reading''s framing while maintaining practical violation?',
    'Ethnographic observation of non-state group decision-making, analysis of group conduct violations against stated commitments to humanitarian principles, and measurement of reputational cost when violations are documented.',
    'If identity-lock is strong, non-state groups internalizing the universal reading will self-police conduct to maintain legitimacy. If weak, the constraint functions as coordination for the audience (international observers) but not for internal group practice. This affects whether the constraint''s protective function extends to non-state-group-affected populations or is limited to state conduct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_state_group_internalization_identity_lock, empirical, 'Whether non-state group adoption of the universal reading produces genuine behavioral change or is instrumental performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.08).
narrative_ontology:measurement_basis(gene_tr_t1949, observed).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement_basis(gene_tr_t1977, observed).
narrative_ontology:measurement(gene_tr_t1998, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement_basis(gene_tr_t1998, observed).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement_basis(gene_tr_t2010, observed).
narrative_ontology:measurement(gene_tr_t2018, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement_basis(gene_tr_t2018, observed).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(gene_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.15).
narrative_ontology:measurement_basis(gene_be_t1949, observed).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement_basis(gene_be_t1977, observed).
narrative_ontology:measurement(gene_be_t1998, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1998, 0.52).
narrative_ontology:measurement_basis(gene_be_t1998, observed).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement_basis(gene_be_t2010, observed).
narrative_ontology:measurement(gene_be_t2018, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement_basis(gene_be_t2018, observed).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(gene_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement_basis(gene_su_t1949, observed).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1977, 0.32).
narrative_ontology:measurement_basis(gene_su_t1977, observed).
narrative_ontology:measurement(gene_su_t1998, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1998, 0.36).
narrative_ontology:measurement_basis(gene_su_t1998, observed).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement_basis(gene_su_t2010, observed).
narrative_ontology:measurement(gene_su_t2018, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement_basis(gene_su_t2018, observed).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2025, 0.41).
narrative_ontology:measurement_basis(gene_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_court_jurisdiction_over_conflict_crimes).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, human_rights_derogation_limits_armed_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one reading (universal_rights_reading) of the contested kernel geneva_conventions_protective_scope. The sibling readings (state_centric_reading and hybrid_proportionality_reading) are separate constraint stories with different ε values, beneficiary structures, and enforcement patterns. The universal reading expands victim set (all conflict participants) and raises extraction on state operations; the state-centric reading narrows victim set and permits classification-based discretion. The hybrid proportionality reading interpolates by scaling protections by conflict type. Each reading has its own ε-invariance, its own stakeholder structure, and its own CS commitments. Link them via network.affects_constraints to enable contamination analysis: if one reading's authority erodes (e.g., state-centric loses judicial support), the others feel structural pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
