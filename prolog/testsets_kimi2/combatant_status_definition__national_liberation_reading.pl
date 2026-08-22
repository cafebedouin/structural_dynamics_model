% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) National Liberation Combatant Status
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the national_liberation_reading of the
 *   contested kernel combatant_status_definition. It concerns AP I Article
 *   1(4), which classifies wars against colonial domination, alien
 *   occupation, and racist regimes as international armed conflicts and
 *   extends combatant statusâand thus POW protections upon captureâto
 *   organized non-state armed groups that meet command-and-control criteria.
 *   The reading treats this as a genuine legal innovation that closes a
 *   protection gap for anti-colonial fighters, while simultaneously
 *   recognizing that it structurally extracts sovereignty from occupying and
 *   colonial powers by removing their domestic criminal jurisdiction over
 *   captured insurgents. Sibling constraints include the
 *   state_centric_reading (GC III Article 4, categorical exclusion of
 *   non-state actors from combatant status) and the
 *   functional_protection_reading (Common Article 3, status-independent
 *   minimum guarantees). The claim/metric independence is maintained: the
 *   constraint is claimed as tangled_rope because it combines coordination
 *   (humanitarian protection) with asymmetric extraction (sovereignty cost to
 *   occupiers), and the metrics are authored descriptively without tuning to
 *   match the claim.
 *
 * KEY AGENTS:
 *   - national_liberation_movements: Primary beneficiary (organized/constrained) â receive conditional combatant immunity and POW status if criteria met
 *   - occupying_colonial_regimes: Primary payer (institutional/constrained) â lose domestic criminal jurisdiction over captured insurgents
 *   - ap_i_state_parties: Agenda-setter (institutional/mobile) â maintain the treaty regime and its interpretation
 *   - major_non_party_states: Excluded seat (institutional/mobile) â deliberately outside AP I to avoid the constraint
 *   - icrc: Analytical observer (institutional/analytical) â monitors compliance and promotes the rule
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.72).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.7).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) National Liberation Combatant Status").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, 'c60baed8-5604-43bf-9a03-da64ff549d9a').
narrative_ontology:cs_kernel_codification('c60baed8-5604-43bf-9a03-da64ff549d9a', formalized).
narrative_ontology:cs_authority_grounding('c60baed8-5604-43bf-9a03-da64ff549d9a', lineage).
narrative_ontology:cs_interpretation_layer_present('c60baed8-5604-43bf-9a03-da64ff549d9a').
narrative_ontology:cs_reading_relation('c60baed8-5604-43bf-9a03-da64ff549d9a', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c60baed8-5604-43bf-9a03-da64ff549d9a', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('c60baed8-5604-43bf-9a03-da64ff549d9a', foundational, wars_of_national_liberation_are_international_conflicts).
narrative_ontology:cs_axiom_status(wars_of_national_liberation_are_international_conflicts, holdable).
narrative_ontology:cs_axiom_grounding('c60baed8-5604-43bf-9a03-da64ff549d9a', wars_of_national_liberation_are_international_conflicts, conventional).
narrative_ontology:cs_axiom('c60baed8-5604-43bf-9a03-da64ff549d9a', foundational, non_state_liberation_combatants_entitled_to_pow_status).
narrative_ontology:cs_axiom_status(non_state_liberation_combatants_entitled_to_pow_status, holdable).
narrative_ontology:cs_axiom_grounding('c60baed8-5604-43bf-9a03-da64ff549d9a', non_state_liberation_combatants_entitled_to_pow_status, deontological).
narrative_ontology:cs_reference_frame('c60baed8-5604-43bf-9a03-da64ff549d9a', post_colonial_self_determination).
narrative_ontology:cs_drift_state('c60baed8-5604-43bf-9a03-da64ff549d9a', contemporary_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c60baed8-5604-43bf-9a03-da64ff549d9a', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_colonial_regimes).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, self_determination_as_legal_basis).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, anti_colonial_legal_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-state armed groups fighting colonial domination, alien occupation, or racist regimes. If they meet organization, command, and conduct criteria under AP I Articles 43 and 44, they receive combatant status and POW protections upon capture, shielding them from domestic criminal prosecution. They must carry arms openly, operate under responsible command, and comply with the laws of war. Exit means abandoning the legal shield and risking treatment as common criminals or unlawful combatants.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, biographical, constrained, regional).

% States exercising colonial domination, alien occupation, or enforcing racist regimes. They are obligated to treat captured members of qualifying liberation movements as POWs rather than prosecuting them under domestic criminal law. This constrains their sovereignty over security policy and removes a deterrent tool against insurgency. They resist the constraint through non-ratification of AP I, reservation, or reclassification of conflicts as non-international or counter-terrorism operations.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_colonial_regimes, payer,
    institutional, generational, constrained, global).

% States party to Additional Protocol I that collectively maintain the treaty regime. They set the legal standard through ratification and state practice. Post-colonial states in this group actively championed Article 1(4) as a mechanism to legitimize anti-colonial resistance. They administer the constraint through national implementation and diplomatic defense of the treaty, though they do not personally collect its extraction.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, ap_i_state_parties, agenda_setter,
    institutional, generational, mobile, global).

% Major military powersânotably the United States, Israel, and othersâthat declined to ratify AP I precisely to avoid the Article 1(4) extension of combatant status to non-state actors. They are structurally excluded from the treaty's formal obligations but are the primary target of customary-law extensions of the national liberation reading. Their absence shapes the constraint's uneven geographic scope and limited enforcement reach.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, major_non_party_states, excluded,
    institutional, generational, mobile, global).

% The International Committee of the Red Cross promotes and monitors compliance with AP I, including Article 1(4). It interprets the criteria for combatant status and advocates for POW protections in all armed conflicts. It neither benefits from nor pays the costs of the constraint's political asymmetries; its institutional purpose is to narrow the gap between the rule and practice through confidential dialogue and public reporting.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends the legal framework of international armed conflict to wars of national liberation, ensuring that organized non-state fighters receive combatant immunity and POW status rather than being treated as common criminals, thereby incentivizing compliance with the laws of war by liberation movements through the promise of legal recognition.
% TRANSFER_FUNCTION: Transfers legal status and immunity from domestic criminal prosecution from occupying and colonial regimes to organized non-state armed groups meeting command-and-control criteria; the occupying power loses exclusive jurisdiction over captured insurgents.
% ABSENT_VOICES: Occupying powers that have not ratified AP Iânotably the United States and Israelâare structurally excluded from the treaty framework but are the primary targets of its customary-law extension; captured insurgents themselves are rarely heard in the diplomatic fora where the rule's scope is negotiated and interpreted.
% DISAPPEARANCE_RATIONALE: If Article 1(4) vanished overnight, occupying powers would regain full domestic criminal jurisdiction over captured members of liberation movements; the legal architecture of AP I would contract back to state-only combatancy; and the incentive structure for non-state groups to organize under formal command hierarchies and carry arms openly would weaken significantly.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions failed to adequately protect non-state fighters in wars of national liberation, leaving them vulnerable to summary execution, torture, and criminal prosecution as 'terrorists' or 'bandits' by colonial and occupying powers that denied the conflicts international status.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and the ICRC attest the problem remains live in ongoing occupations and deny the protection gap has closed. Occupying powers and non-party Western states attest the problem was better addressed through Common Article 3 and human rights law, and that Article 1(4) creates more legal and security problems than it solves. Independent international law scholars are split, with significant corroboration from Global South academia but strong contestation from major military powers and their legal advisers.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint removes a core attribute of sovereigntyâcriminal jurisdiction over security threatsâfrom occupying powers and transfers legal immunity to non-state groups. Suppression (0.70) is high because the constraint's persistence depends on actively suppressing the alternative framework (treating insurgents as criminals or terrorists) through treaty obligation, diplomatic pressure, and ICC jurisdiction. Theater ratio (0.35) reflects moderate performativity: some states create parallel detention regimes (military commissions, status-review tribunals) that nominally comply with IHL while functionally denying POW protections. Accessibility collapse (0.42) is moderate because occupying powers retain partial exit via non-ratification, persistent-objector claims, and counter-terrorism reframing. Resistance (0.78) is very high due to sustained opposition from major military powers and the post-9/11 counter-terrorism paradigm.
 *
 * PERSPECTIVAL GAP:
 *   The national_liberation_movements seat experiences the constraint as protective subsidy (low directionality, negative effective extractionâthey gain legal status and immunity). The occupying_colonial_regimes seat experiences it as heavy extraction (high directionality, amplified by their global scopeâthey lose jurisdictional autonomy over security threats). The major_non_party_states seat sits near the beneficiary end despite their institutional power because they successfully exited the treaty; their mobile exit options mean they avoid extraction entirely. The engine will compute these divergent seat types from the structural data: the constraint looks like a tangled rope from the system level, like a subsidy from the liberation-movement seat, and like a snare from the occupying-power seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to national_liberation_movements, who receive the legal status, combatant immunity, and POW protections upon capture. Victim declarations map to occupying_colonial_regimes, who bear the sovereignty cost of losing domestic criminal jurisdiction over captured insurgents. The ap_i_state_parties are not declared as beneficiaries because their benefit is diffuse legitimization rather than concentrated rent; they administer the constraint but do not collect its extraction. The major_non_party_states are excluded from the constraint's formal operation and absent from beneficiary/victim arrays, though their structural position (mobile exit) means they experience negligible directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids snare classification because it carries a genuine coordination function: without Article 1(4), organized non-state fighters in anti-colonial wars would fall into a legal void, vulnerable to summary execution and torture. The incentivization of command hierarchy and open carriage of arms is a real coordination benefit that reduces civilian harm. It avoids rope classification because the extraction is asymmetric: occupying powers bear concentrated costs (loss of criminal jurisdiction, erosion of deterrent effect) that are not offset by equivalent benefits. The high resistance metric signals that the payer seat actively contests the arrangement, which would be unexpected for a pure rope. Tangled rope is the structurally accurate claim because both the coordination mechanism and the asymmetric extraction pass through the same legal form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does AP I Article 1(4) represent a legally effective extension of combatant status to non-state actors in national liberation conflicts, or does it remain a contested political framing without consistent state practice?',
    'Comparative state-practice analysis measuring how many AP I state parties actually grant POW status to captured members of national liberation movements in contemporary conflicts, versus how many invoke counter-terrorism or non-international conflict frameworks instead.',
    'If state practice consistently denies such status, the reading''s effective extraction is higher on paper than in reality (the constraint extracts asymmetrically only from weak states while strong states ignore it); if practice aligns, the coordination function is genuinely operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether the national liberation reading has achieved consistent legal effect or remains contested political framing.').

omega_variable(
    coordination_extraction_ambiguity,
    'Does Article 1(4) primarily coordinate humanitarian protection for non-state fighters, or does it primarily extract sovereignty from occupying and colonial powers by constraining their domestic criminal jurisdiction?',
    'Case-study analysis of conflicts where Article 1(4) was invoked: measure IHL compliance rates by liberation movements that received status versus criminal prosecution rates by occupying powers of those who did not.',
    'If liberation movements comply more with IHL when status is granted, the coordination function dominates; if occupying powers bear sovereignty costs without reciprocal compliance gains by insurgents, extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ambiguity, conceptual, 'Whether the constraint''s coordination or extraction component is structurally dominant.').

omega_variable(
    customary_law_extension,
    'Has Article 1(4) achieved customary international law status beyond AP I state parties, or does its effect remain confined to treaty parties?',
    'State practice and opinio juris survey across non-party states, focusing on whether they treat captured liberation fighters as POWs in relevant conflicts and whether they accept the international-armed-conflict classification for national liberation wars.',
    'If customary, the constraint''s spatial scope and effective extraction expand dramatically to include non-party occupying powers; if purely treaty-based, its extraction is limited to the state-party subset and exit via non-ratification remains structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_extension, empirical, 'Whether the constraint''s legal force extends beyond treaty parties via customary international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csd_nlr_tr_t0, combatant_status_definition__national_liberation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(csd_nlr_tr_t9, combatant_status_definition__national_liberation_reading, theater_ratio, 9, 0.2).
narrative_ontology:measurement(csd_nlr_tr_t18, combatant_status_definition__national_liberation_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(csd_nlr_tr_t27, combatant_status_definition__national_liberation_reading, theater_ratio, 27, 0.32).
narrative_ontology:measurement(csd_nlr_tr_t36, combatant_status_definition__national_liberation_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(csd_nlr_tr_t45, combatant_status_definition__national_liberation_reading, theater_ratio, 45, 0.42).

% Extraction over time
narrative_ontology:measurement(csd_nlr_be_t0, combatant_status_definition__national_liberation_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(csd_nlr_be_t9, combatant_status_definition__national_liberation_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(csd_nlr_be_t18, combatant_status_definition__national_liberation_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(csd_nlr_be_t27, combatant_status_definition__national_liberation_reading, base_extractiveness, 27, 0.65).
narrative_ontology:measurement(csd_nlr_be_t36, combatant_status_definition__national_liberation_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement(csd_nlr_be_t45, combatant_status_definition__national_liberation_reading, base_extractiveness, 45, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(combatant_status_definition__national_liberation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, functional_protection_reading).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel decomposes into three structurally distinct constraints: the state_centric_reading (GC III Article 4, state monopoly on combatancy), the national_liberation_reading (AP I Article 1(4), conditional non-state combatant status), and the functional_protection_reading (Common Article 3, status-independent minimum guarantees). They share the referent of 'who is protected in armed conflict' but have different epsilon values, beneficiary structures, and legal bases. This story covers the national_liberation_reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
