% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria_hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Normatively Conditioned Statehood Standard (Hybrid Reading)
 *   domain: international law/political philosophy/state theory
 *
 * SUMMARY:
 *   A dominant coalition of established liberal democracies administers
 *   admission to the state system behind a bar that extends the four classic
 *   Montevideo criteria with a governance test: democratic legitimacy, human
 *   rights performance, and non-aggression. The standard coordinates the
 *   coalition's recognition policy, but its application tracks geopolitical
 *   alignment as often as recorded compliance, and its justificatory
 *   vocabulary extends beyond admissions to sanctions and intervention. This
 *   file instantiates ONE reading of the montevideo_statehood_criteria
 *   kernel: the hybrid reading. Sibling readings (declaratory_reading,
 *   constitutive_reading) are separate constraints with separate epsilon
 *   values and victim structures; the contest between readings is carried in
 *   the omega variables, not inside this constraint. KEY AGENTS (by
 *   structural relationship): - established_liberal_democracies:
 *   Agenda-setter and collector (institutional/arbitrage) — sets the bar,
 *   collects discretionary ground and intervention cover -
 *   rival_patron_states: Secondary collector (powerful/mobile) — harvests
 *   double-standard cover while contesting the bar -
 *   incumbent_nondemocratic_governments: Target (powerful/constrained) —
 *   their sovereignty becomes conditional -
 *   nonliberal_secessionist_authorities: Primary target (moderate/trapped) —
 *   criteria-compliant but barred - populations_of_unrecognized_polities:
 *   Bearer of diffuse costs (powerless/trapped) -
 *   minority_nations_without_territorial_control: Excluded voice
 *   (powerless/trapped) — outside the conversation entirely -
 *   international_legal_theorists: Analytical observer — records the
 *   principle-application gap
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.66).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.54).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Normatively Conditioned Statehood Standard (Hybrid Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international law/political philosophy/state theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '2340bcd5-3685-40b6-a8e6-fed2c52d0a68').
narrative_ontology:cs_kernel_codification('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', fixed_text).
narrative_ontology:cs_authority_grounding('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', lineage).
narrative_ontology:cs_interpretation_layer_present('2340bcd5-3685-40b6-a8e6-fed2c52d0a68').
narrative_ontology:cs_reading_relation('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', montevideo_statehood_criteria__montevideo_statehood_declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', montevideo_statehood_criteria__montevideo_statehood_constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', foundational, normative_legitimacy_constitutes_statehood_entitlement).
narrative_ontology:cs_axiom_status(normative_legitimacy_constitutes_statehood_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', normative_legitimacy_constitutes_statehood_entitlement, deontological).
narrative_ontology:cs_axiom('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', secondary, conditional_recognition_promotes_peaceful_order).
narrative_ontology:cs_axiom_status(conditional_recognition_promotes_peaceful_order, holdable).
narrative_ontology:cs_axiom_grounding('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', conditional_recognition_promotes_peaceful_order, instrumental).
narrative_ontology:cs_reference_frame('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', criteria_plus_legitimacy_baseline).
narrative_ontology:cs_drift_state('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', contemporary_selective_application_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2340bcd5-3685-40b6-a8e6-fed2c52d0a68', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, rival_patron_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, nonliberal_secessionist_authorities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, populations_of_unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, incumbent_nondemocratic_governments).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, conditional_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively administer recognition policy through UN admission machinery, European Community guidelines, and coordinated diplomatic practice. They set the normative bar (democratic governance, human rights records, non-aggression) that new polities must meet beyond the four classic criteria. They gain discretionary ground from the bar: it justifies withholding recognition from adversaries' proteges and endorsing interventions framed as restoring legitimate order. Their costs are marginal and indirect, chiefly consistency demands that complicate alliances with non-conforming partners. Abandoning the standard outright would forfeit the ordering role the coalition currently monopolizes.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies, beneficiary).

% Hold veto power in the Security Council and maintain parallel blocs. They contest the normative bar publicly as double-standard politics while practicing their own selective recognition of allied breakaway territories. Every inconsistently applied case hands them rhetorical and legal cover for their own exceptions; they also pay when the bar is turned toward their partners or cited to sanction them. Their exit from the standard's jurisdiction is comparatively open through parallel institutions and alternative financial and security architectures.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, rival_patron_states, beneficiary,
    powerful, generational, mobile, global).

% Govern recognized states whose domestic arrangements fall short of the normative bar. Their sovereignty acquires a conditional quality: the same doctrine that gates new admissions justifies sanctions, isolation campaigns, and externally backed regime change framed as restoring legitimate order against them. Available responses are cosmetic liberalization, deeper repression, patron alignment, or normative counter-campaigns. Full exit would mean transforming their own systems of rule.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, incumbent_nondemocratic_governments, payer,
    powerful, generational, constrained, national).

% De facto authorities of breakaway territories that satisfy the four classic criteria (population, defined territory, effective government, capacity for external relations) but not the normative bar. Recognition is withheld regardless of demonstrated effectiveness; their entities persist in legal limbo with restricted access to finance, treaties, and diplomatic standing. Paths forward are narrow: convincing democratization under adverse conditions, absorption into a patron's orbit, or indefinite non-recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, nonliberal_secessionist_authorities, payer,
    moderate, biographical, trapped, regional).

% Residents of polities the recognizing community declines to admit. They carry the arrangement's costs concretely: no treaty protections, blocked development finance, travel barriers, and no seat in any forum where their own status is debated. Their options are emigration, endurance, or mobilization without a state to speak for them.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, populations_of_unrecognized_polities, payer,
    powerless, biographical, trapped, regional).

% Nations lacking consolidated territory cannot meet even the classic criteria, and the added normative bar places membership further out of reach. They are absent from admission deliberations entirely; their self-determination claims are processed as the internal affairs of existing states.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, minority_nations_without_territorial_control, excluded,
    powerless, generational, trapped, regional).

% Scholars and practitioners of international law who analyze recognition practice, adjudication, and doctrine. They observe the gap between stated principle and applied standard, publish the discrepancy record, and supply the vocabulary in which the contest is argued. Their position carries no enforcement lever.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_legal_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the community of recognizing states a shared, articulable standard for who may enter the state system, replacing case-by-case bilateral bargaining with a common bar tied to governance quality.
% TRANSFER_FUNCTION: Moves international legal personality (treaty access, finance, diplomatic standing, security assurances) away from polities failing the governance bar and toward the recognizing coalition's discretionary judgment; moves a justificatory vocabulary for intervention and regime change to that coalition, and existential uncertainty to non-conforming polities and their residents.
% ABSENT_VOICES: Populations of unrecognized polities and stateless nations without territory would object, to selective application and to the absence of any appeal forum, but no seat exists for them: admission committees comprise existing states, and General Assembly votes are cast by governments, not by the residents of the polities being judged.
% DISAPPEARANCE_RATIONALE: Recognition politics would revert to the declaratory-versus-constitutive contest: numerous limbo entities would gain or lose standing overnight, Security Council veto patterns over admissions would resurface as the binding constraint, and intervention justifications would lose their principal legal vocabulary, redrawing the state system's membership boundary around raw effectiveness and great-power assent.
% FOUNDING_PROBLEM: After the world wars, the problem was orderly recognition: which new entities deserved admission, preventing a recognition free-for-all, and refusing legitimacy to predatory or ineffective polities. The Cold War's end revived it for dissolving federations, producing the 1991 European Community guidelines that conditioned recognition of Yugoslav and Soviet successor polities on democracy, rule of law, and rights commitments.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholarship on the creation of states, the International Court of Justice's Kosovo advisory opinion treating effectiveness as the operative test, and Non-Aligned Movement statements attest that the classic recognition problem was substantially managed by existing practice and that the normative extension lacks independent corroboration as a solution to a still-live problem. No source outside the benefiting coalition attests that conditionality solved a problem ordinary recognition practice had left open.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.66 at interval end) reflects the decoupling of the governance bar from measurable harm: polities satisfying the classic criteria are denied legal personality on standards the applying coalition itself does not uniformly meet. Suppression (0.54) is moderate because enforcement operates by benefit-denial rather than direct coercion — nothing prevents a de facto polity from existing; what is suppressed is its participation. Theater ratio (0.38 and rising) tracks selective application: Kosovo admitted over objections the bar was said to answer, comparable cases elsewhere declined, coalition-aligned violators exempted — a growing share of the standard's activity defends the coalition's discretion rather than measuring governance. The suppression series documents enforcement-capacity change specifically: machinery built up through the 1990s and 2000s (sanctions regimes, admission committees, protection-doctrine institutionalization), peaked mid-interval, then decayed as Iraq 2003 and Libya 2011 eroded the enforcement coalition's own legitimacy — a legitimacy-driven build-and-decay arc, not intermittent reinforcement. Interval maps time point 0 to 1991 (European Community recognition guidelines) and time point 30 to roughly 2021; scalars report interval-end state. The claim (tangled_rope) and the metrics were authored independently: the standard-setting function is genuine, and the extraction riding on it is real; if the selective-application trend continues, computed type should migrate toward snare — that migration is the datum this story exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/collector seat experiences a principled club standard it wrote and staffs; the trapped payer seats experience a movable gatekeeper whose bar rises and falls with alignment; powerless residents of unrecognized polities experience simple abandonment, having no seat anywhere in the process. Same-level divergence is sharpest between established_liberal_democracies and rival_patron_states — nominally peer great-power blocs whose different experience of the identical standard is produced entirely by insider status and differential exit (arbitrage versus mobile): the insider writes the test, the outsider monetizes its hypocrisy. Coalition membership itself carries an identity dimension — the self-concept of a community of democracies makes abandoning the bar costly in ways unrelated to its results, which is why the enforcement series decays without the standard disappearing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive directionality near the beneficiary pole: the coalition's arbitrage-grade exit (it defines and staffs the test) anchors it at minimal d. Declared victims derive high d along a power-and-exit gradient: trapped, powerless residents of unrecognized polities sit nearest the full-target pole; trapped, moderate secessionist authorities slightly below them; powerful but exit-constrained incumbent governments lower still but clearly target-side. No directionality_overrides were authored: the derivation chain produces the right structure from the beneficiary/victim declarations, and the override mechanism keys on the power atom alone — differentiating rival_patron_states (nominally beneficiary-derived, structurally mixed: it collects cover from the standard's uneven application AND pays when the bar turns toward its partners) would require an override that collides with incumbent governments sharing that atom. The rival-patron ambivalence is therefore routed to the omega variables instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested rather than dead: the classic disorderly-recognition problem was largely managed by existing practice, but the coalition attests new threats that conditionality answers, and outsiders attest it answers none that recognition practice left open. Because founding_problem_status is contested alongside a world_rearranges disappearance verdict, the mismatch consumer registers no zombie flag; the arrangement is not administering a corpse. The classification work here is boundary maintenance in both directions: keeping the genuine coordination function (a shared admission standard) visible prevents misreading the arrangement as pure extraction, and keeping the victim set (criteria-compliant secessionists, abandoned populations, conditionally-sovereign incumbents) visible prevents misreading it as pure coordination. The rising theater series is the drift watch-item: it measures how much of the standard's activity now performs principle while allocating by alignment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contingency_of_the_victim_set,
    'This constraint instantiates the hybrid_reading of the montevideo_statehood_criteria kernel; how do the victim set and extraction profile shift under the sibling readings (declaratory_reading, constitutive_reading)?',
    'Regenerate the story under each sibling reading and compare: declaratory_reading removes the normative bar entirely (victims collapse to criterion-failing entities only); constitutive_reading replaces the bar with great-power consensus (victims become whoever the permanent-five coalition excludes).',
    'Under the declaratory reading, extraction from governance-failing-but-criteria-compliant polities approaches zero and the arrangement trends rope-like; under the constitutive reading, extraction concentrates in permanent-five discretion and trends snare-flavored. The victim set authored here — non-liberal secessionists above all — is a property of this reading, not of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency_of_the_victim_set, conceptual, 'Committer structure: victim-set membership depends on which reading of the statehood kernel is instantiated.').

omega_variable(
    selective_application_double_standard,
    'Is the normative bar applied in correspondence with recorded governance compliance, or does alignment with the recognizing coalition predict recognition outcomes better than compliance does?',
    'Systematic coding of recognition decisions against both compliance metrics and alliance-alignment indicators across the interval; regression of outcome on the two predictors.',
    'If alignment dominates, the standard''s extraction concentrates on adversary-aligned polities, the theater ratio understates the rot, and drift toward snare becomes live rather than hypothetical; if compliance predicts outcomes, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_application_double_standard, empirical, 'Whether the governance bar measures governance or sorts friends from adversaries.').

omega_variable(
    intervention_cover_causality,
    'Does normative conditionality causally unlock interventions and regime-change operations that would otherwise be blocked, or does it function as post-hoc justification for operations undertaken on other grounds?',
    'Pre-intervention legal memoranda and Security Council debate records (Iraq 2003, Libya 2011) examined for operational reliance on legitimacy deficits versus retrospective invocation.',
    'A causal finding materially raises effective extraction: the constraint would be transferring physical security from weak states to the recognizing coalition, not merely allocating diplomatic standing; a purely rhetorical finding leaves the transfer confined to the symbolic register.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_cover_causality, empirical, 'Whether the legitimacy vocabulary enables force or merely decorates it.').

omega_variable(
    coalition_identity_lock,
    'Is the recognizing coalition''s persistence under the standard identity-driven — a self-concept as a community of democracies that makes abandoning the bar unthinkable regardless of results?',
    'Track standard adherence through costly failure episodes (the Libya aftermath, the Afghanistan withdrawal): persistence without compensating benefit indicates identity lock; quiet abandonment where costs exceed returns indicates instrumentality.',
    'If locked, coalition members absorb the standard''s costs rather than shed it, sustaining suppression internally and shifting member-seat exit classifications toward identity_locked; the arrangement becomes harder to dismantle than its results alone would predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_identity_lock, conceptual, 'Identity-fusion mechanism binding the enforcing coalition to its own standard.').

omega_variable(
    authority_framing_underdetermination,
    'Is the adjudicating authority the Montevideo-lineage text as interpreted through UN organs and treaty practice (lineage framing, as authored here), or the accumulated enforcement practice itself (a practice-grounded, implicit-kernel framing in which state practice IS the standard)?',
    'Test whether textual-doctrinal interpretation or accumulated recognition practice better predicts outcomes; if practice dominates, reframe cs_structure as implicit kernel with practice-grounded authority.',
    'Under the practice framing, selective application is the kernel rather than drift from it — the arrangement classifies as captured-by-practice at the commitment-system level, and the practice_drift vector authored here would be re-read as constitutive behavior rather than deviation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'Two coherent framings of the authority structure produce different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msc_hybrid_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(msc_hybrid_tr_t0, observed).
narrative_ontology:measurement(msc_hybrid_tr_t5, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(msc_hybrid_tr_t5, observed).
narrative_ontology:measurement(msc_hybrid_tr_t10, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(msc_hybrid_tr_t10, observed).
narrative_ontology:measurement(msc_hybrid_tr_t15, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(msc_hybrid_tr_t15, observed).
narrative_ontology:measurement(msc_hybrid_tr_t20, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(msc_hybrid_tr_t20, observed).
narrative_ontology:measurement(msc_hybrid_tr_t25, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement_basis(msc_hybrid_tr_t25, observed).
narrative_ontology:measurement(msc_hybrid_tr_t30, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(msc_hybrid_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(msc_hybrid_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(msc_hybrid_be_t0, observed).
narrative_ontology:measurement(msc_hybrid_be_t5, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(msc_hybrid_be_t5, observed).
narrative_ontology:measurement(msc_hybrid_be_t10, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(msc_hybrid_be_t10, observed).
narrative_ontology:measurement(msc_hybrid_be_t15, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(msc_hybrid_be_t15, observed).
narrative_ontology:measurement(msc_hybrid_be_t20, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(msc_hybrid_be_t20, observed).
narrative_ontology:measurement(msc_hybrid_be_t25, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(msc_hybrid_be_t25, observed).
narrative_ontology:measurement(msc_hybrid_be_t30, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(msc_hybrid_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(msc_hybrid_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(msc_hybrid_su_t0, observed).
narrative_ontology:measurement(msc_hybrid_su_t5, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(msc_hybrid_su_t5, observed).
narrative_ontology:measurement(msc_hybrid_su_t10, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(msc_hybrid_su_t10, observed).
narrative_ontology:measurement(msc_hybrid_su_t15, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(msc_hybrid_su_t15, observed).
narrative_ontology:measurement(msc_hybrid_su_t20, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(msc_hybrid_su_t20, observed).
narrative_ontology:measurement(msc_hybrid_su_t25, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement_basis(msc_hybrid_su_t25, observed).
narrative_ontology:measurement(msc_hybrid_su_t30, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(msc_hybrid_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_r2p_doctrine).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Montevideo statehood criteria' decomposes into three structurally distinct readings with distinct epsilon values and victim sets. The declaratory reading (objective criteria establish statehood as legal fact) is the upstream baseline — highest empirical confidence, negligible extraction — and is routinely cited as settled foundation beneath the other two. This hybrid reading is downstream: it preserves the declaratory criteria as necessary conditions while adding a normative sufficiency bar, which imports a victim set the baseline lacks (non-liberal secessionists, their populations) and exports intervention-cover effects to the R2P/intervention doctrine node. The constitutive reading is a third constraint in which recognition itself constitutes statehood; it shares no victim structure with either sibling. Each file links the others via network.affects_constraints; epsilon differences across the family are the point of the decomposition, not an artifact to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
