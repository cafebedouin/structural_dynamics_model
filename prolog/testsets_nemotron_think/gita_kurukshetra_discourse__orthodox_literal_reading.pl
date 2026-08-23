% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading: Gita Mandates Caste Duty and Righteous Violence
 *   domain: religious/textual/philosophical
 *
 * SUMMARY:
 *   The Bhagavad Gita's Kurukshetra discourse is a contested kernel. This
 *   constraint story instantiates the orthodox literal reading: the text
 *   mandates varna (caste) duty as eternal divine law (BG 4.13, 18.41-44) and
 *   legitimates violence when performed as svadharma by kshatriyas (BG
 *   2.31-38, 18.43). The reading claims Mountain status — the arrangement is
 *   sanatana dharma, emerging naturally from cosmic order. Analytically, the
 *   constraint shows high extractiveness (0.72) and suppression (0.78): it
 *   concentrates interpretive authority in brahmins, martial legitimacy in
 *   kshatriyas, and extracts labor, status, and lives from lower castes and
 *   war dead. The theater ratio (0.42) reflects that the coordination
 *   function (cosmic order maintenance) is real but increasingly performative
 *   as the extraction apparatus (caste enforcement, war mobilization)
 *   dominates. The claim/metric divergence is deliberate: the reading claims
 *   Mountain; the metrics reveal Tangled Rope or Snare structure. The engine
 *   computes this divergence; the author does not reconcile it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, mountain).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading: Gita Mandates Caste Duty and Righteous Violence").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/textual/philosophical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).
domain_priors:emerges_naturally(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '72ba0c40-43e4-469f-bd81-7a6cdfdf70eb').
narrative_ontology:cs_kernel_codification('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', fixed_text).
narrative_ontology:cs_authority_grounding('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', lineage).
narrative_ontology:cs_interpretation_layer_present('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb').
narrative_ontology:cs_reading_relation('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', gita_kurukshetra_discourse__universalist_devotional_reading, forecloses).
narrative_ontology:cs_axiom('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', foundational, varna_dharma_eternal).
narrative_ontology:cs_axiom_status(varna_dharma_eternal, holdable).
narrative_ontology:cs_axiom_grounding('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', varna_dharma_eternal, theological).
narrative_ontology:cs_axiom('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', foundational, kshatriya_violence_righteous).
narrative_ontology:cs_axiom_status(kshatriya_violence_righteous, holdable).
narrative_ontology:cs_axiom_grounding('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', kshatriya_violence_righteous, theological).
narrative_ontology:cs_axiom('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', secondary, brahmin_interpretive_monopoly).
narrative_ontology:cs_axiom_status(brahmin_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', brahmin_interpretive_monopoly, theological).
narrative_ontology:cs_reference_frame('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', kaliyuga_varnashrama_dharma).
narrative_ontology:cs_drift_state('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', modern_secular_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('72ba0c40-43e4-469f-bd81-7a6cdfdf70eb', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_subjects).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_in_dharmic_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varnashrama_dharma_eternal).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, karma_phala_tyaga).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, svadharma_superior_to_paradharma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold interpretive monopoly over the text through parampara (disciplic succession); derive authority, livelihood, and ritual status from maintaining that the Gita literally establishes varna hierarchy and sanctions violence when performed as svadharma. Their identity is fused with the textual authority they transmit — exit means relinquishing the only ground of their epistemic and social standing.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters, beneficiary).

% Receive scriptural legitimation for their martial role and the violence it entails; the Gita tells them fighting a righteous war is superior to renunciation. They also pay the cost — death in battle — but the text frames this cost as spiritual gain (svarga or moksha). Exit from the role is constrained by birth-ascribed identity and the cosmological claim that svadharma is inescapable.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, payer).

% Assigned to serve the twice-born varnas by birth; the Gita's varna-vyavastha (BG 4.13, 18.41-44) presents this as divinely ordained. They bear the material extraction (labor, ritual pollution, exclusion from sacred knowledge) and the ideological extraction (internalized inferiority). Exit is identity-locked: the constraint defines what they *are*, not merely what they do; leaving the hierarchy requires rejecting the cosmology that constitutes their self-understanding.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_subjects, payer,
    powerless, generational, identity_locked, continental).

% Killed in wars justified as 'dharmic' by the text's authority. The Gita (2.31-38) frames death in righteous battle as gateway to heaven, making the ultimate extraction — life itself — appear as spiritual profit. They have no exit; the constraint operates on them posthumously through the legitimating narrative.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_in_dharmic_conflict, payer,
    powerless, immediate, trapped, local).

% Read Kurukshetra as metaphor for inner struggle; argue the text's violence is spiritual, not physical. They are excluded from the orthodox interpretive space because their reading undermines the literal mandate that sustains brahmin authority and kshatriya violence. They can and do maintain parallel interpretive communities.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_readers, excluded,
    organized, biographical, mobile, global).

% Read the Gita as teaching bhakti accessible to all regardless of caste; dharma is surrender to Krishna, not varna duty. Excluded from orthodox space because their reading dissolves the caste hierarchy that the orthodox reading treats as the text's core mandate. They maintain parallel communities (e.g., Gaudiya Vaishnavism, ISKCON) with institutional independence.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_readers, excluded,
    organized, biographical, mobile, global).

% Sees the full structure: a fixed-text kernel (the Gita) with an interpretive tradition that treats varna hierarchy and righteous violence as eternal divine law. Observes that the arrangement extracts labor, status, and lives from lower castes and war dead while concentrating interpretive authority and martial legitimacy in brahmin and kshatriya hands. The constraint's persistence depends on active enforcement (social exclusion, ritual policing, state patronage of orthodoxy) and on the identity-lock that makes exit from varna identity existentially costly.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains cosmic and social order (rta/dharma) by assigning each person a birth-determined duty (svadharma) that, when performed without attachment to fruit, sustains the universe's moral architecture. The Gita presents this as the solution to Arjuna's paralysis: action aligned with one's nature (svabhava) and station (varna) is superior to inaction or another's duty.
% TRANSFER_FUNCTION: Moves ritual purity, epistemic authority, and legitimate violence upward to brahmins and kshatriyas; moves labor, pollution, risk of death, and ideological subordination downward to vaishyas, shudras, and those outside varna. War dead transfer their lives to the cosmic order, framed as spiritual profit.
% ABSENT_VOICES: The voices of shudras and outcastes who never had access to the text's Sanskrit original or the commentarial tradition; women of all varnas whose svadharma in the orthodox reading is subsumed under patriarchal duty; Buddhist, Jain, and Carvaka critics who rejected varna hierarchy and the sanctification of violence; modern anti-caste movements (Phule, Ambedkar, Periyar) who identify the Gita as a primary ideological weapon of Brahminism.
% DISAPPEARANCE_RATIONALE: If the orthodox literal reading vanished overnight, the scriptural linchpin of varna hierarchy would be gone. Caste would lose its most authoritative theological warrant (BG 4.13, 18.41-44). The legitimation of violence as svadharma would collapse. Indian constitutional law (which already abolishes untouchability and guarantees equality) would lose its most potent religious opponent. The social order would rearrange — not because caste would disappear instantly, but because its divine mandate would be severed.
% FOUNDING_PROBLEM: How to maintain social cohesion and cosmic order in Kali Yuga when human nature has degraded? The Gita's answer: a birth-assigned duty system (varnashrama dharma) where each person acts according to their guna-determined nature, offering all action to the Divine, thereby preventing societal disintegration.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox commentaries (Shankara, Ramanuja, Madhva, Prabhupada) attest the problem is live — Kali Yuga persists, varna dharma remains the prescribed remedy. Modern historians (Romila Thapar, Wendy Doniger) and anti-caste intellectuals (Ambedkar, Kancha Ilaiah) attest the problem is a constructed ideology serving brahminical power — the 'founding problem' is a retrospective rationalization for hierarchy. No corroboration exists outside the beneficiary tradition that the varna system was ever a functional solution rather than an extraction mechanism.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, ExtMetricName, E),
    domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gita_kurukshetra_discourse__orthodox_literal_reading),
    narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the arrangement transfers material and symbolic capital upward (ritual authority, land control, state patronage to brahmins; war spoils, political power to kshatriyas) while externalizing costs downward (manual labor, ritual pollution, battlefield death). Suppression is high because the constraint's persistence depends on active enforcement: scriptural citation to block reform, social ostracization of dissenters, state violence against anti-caste movements, and the internalized identity-lock that makes varna appear as ontology rather than imposition. Theater ratio is moderate: the coordinate function (social stability through role differentiation) is genuine but increasingly a cover for extraction — the varna system's actual operation in medieval and modern periods shows diminishing coordination and escalating extraction. Accessibility collapse is near-total (0.88): once the text is accepted as divine revelation, alternatives (buddhist rejection of varna, bhakti egalitarianism, modern equality) appear as adharma. Resistance is moderate (0.35): resistance exists (anti-caste movements, heterodox traditions) but is structurally disadvantaged by the identity-lock and the text's canonical status.
 *
 * PERSPECTIVAL GAP:
 *   The brahmin_interpreter seat experiences the constraint as Mountain — they are the custodians of eternal law, their authority is the constraint's operation. The kshatriya seat experiences it as Rope with beneficiary tilt — they receive legitimate violence authorization but pay with their lives. The lower_caste_subjects seat experiences it as Snare — extraction is total, exit is identity-locked, the coordination story is transparent cover. The war_dead seat experiences it as pure extraction with no coordination return — they pay the ultimate cost for a narrative that frames their death as spiritual profit. The gandhian and universalist readers experience it as a contested Mountain they must climb around — their readings coexist but are structurally excluded from the orthodox interpretive space. The analytical_observer sees the full gradient: a constraint that claims natural law while operating as engineered extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmins are full beneficiaries (d ≈ 0.05): they collect interpretive rents, ritual fees, and epistemic monopoly; their identity is fused with the constraint's authority. Kshatriyas are asymmetric beneficiaries (d ≈ 0.25): they receive violence legitimation and political authority but bear mortality costs in war; their exit is constrained by birth-ascribed svadharma. Lower castes are full targets (d ≈ 0.95): they bear material and ideological extraction with identity-locked exit — the constraint constitutes their social being. War dead are trapped targets (d ≈ 1.0): no exit, no benefit, pure extraction framed as spiritual gain. Gandhian and universalist readers are mobile excluded agents (d ≈ 0.4): they bear no direct extraction from this reading but are excluded from its interpretive authority; they maintain parallel frameworks. The analytical observer sits at d ≈ 0.5 (symmetric analytical cost/benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining order in Kali Yuga) is contested: orthodox tradition says it remains live; historical and anti-caste analysis says it was always an extraction rationale. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) signals mandatrophy: the arrangement persists after its claimed justification is disputed. The constraint prevents mislabeling coordination as pure extraction by maintaining a genuine (though atrophied) coordination function — varna does organize labor and ritual — but the extraction-to-coordination ratio has shifted decisively toward extraction. The identity-lock on lower castes is the primary extraction amplifier: it converts what could be a negotiable social contract into an existential trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is this constraint a genuine natural law (Mountain) as the reading claims, or a constructed extraction mechanism (Tangled Rope/Snare) that the reading''s authority structure presents as natural law?',
    'Compare the reading''s metric profile against the False Summit Mountain signature: if extractiveness and suppression are substantially non-zero while emerges_naturally is claimed, and beneficiaries (brahmins, kshatriyas) are identifiable, the FSM signature triggers reclassification. The engine''s computation will reveal the divergence between claimed_type and computed per-seat types.',
    'If FSM triggers, the constraint reclassifies to tangled_rope (default override) — exposing the coordination-extraction hybrid. If not, the Mountain claim holds. This omega records the committer-frame uncertainty: the same kernel produces radically different ε across readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Committer-frame ambiguity: one kernel, multiple readings, divergent ε. This reading claims Mountain; siblings claim Snare/Tangled Rope. The engine''s per-seat computation resolves the contest.').

omega_variable(
    natural_law_vs_constructed_hierarchy,
    'Is varna hierarchy a structural feature of reality (cosmic rta) or a human construction legitimated by textual authority?',
    'Historical analysis of varna''s emergence: if varna categories and hierarchy appear in the archaeological/epigraphic record as gradual social formations rather than revealed eternal order, the natural law claim fails. Genetic/anthropological evidence on endogamy''s timeline (Reich et al. 2009: endogamy ~2000 BP) vs. textual claims of primordial origin.',
    'If constructed, the Mountain claim is false summit; the constraint is Tangled Rope (coordination + extraction) or Snare. The beneficiary set (brahmins, kshatriyas) would be identified as constructors/maintainers, not custodians.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_hierarchy, empirical, 'FSM-triggering omega: natural-law claim vs. historical construction evidence.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of lower castes primarily structural (legal, economic, violent enforcement) or internalized (varna identity fused with self-concept, making exit unthinkable)?',
    'Post-exit trajectory analysis: if lower-caste individuals who convert to egalitarian traditions (Buddhism, Christianity, Sikhism, secular modernity) still experience varna-based discrimination, structural suppression dominates. If discrimination diminishes but internalized inferiority persists, internalized suppression is significant. Ambedkar''s conversion to Buddhism and the persistence of caste among converts is key evidence.',
    'If internalized, effective suppression is higher than structural measures suggest — the target carries the constraint after formal exit. This amplifies χ for identity-locked agents beyond the structural d-value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in caste hierarchy — determines whether exit_options=''identity_locked'' fully captures the constraint''s grip.').

omega_variable(
    coordination_function_genuineness,
    'Does the varna system genuinely solve a coordination problem (social stability, labor allocation) or is the coordination story entirely cover for extraction?',
    'Counterfactual comparison: societies without varna (tribal, modern egalitarian) achieve social coordination through other mechanisms. If varna''s coordination function is replaceable without societal collapse, the coordination story is not necessary — it is either contingent or cover. Historical test: post-independence India abolished varna legally but maintained social coordination (democracy, markets, bureaucracy).',
    'If coordination is replaceable, the constraint is Snare (coordination story is cover). If varna provides unique irreplaceable coordination, it is Tangled Rope. Current evidence favors Snare classification for the modern period; early period may have had genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether the coordination function is necessary or contingent — determines Tangled Rope vs. Snare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement(gita_tr_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(gita_tr_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(gita_tr_t2500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2500, 0.42).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 500, 0.45).
narrative_ontology:measurement(gita_be_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(gita_be_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1500, 0.65).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(gita_be_t2500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2500, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gita_su_t500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(gita_su_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(gita_su_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1500, 0.72).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(gita_su_t2500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2500, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, manusmriti_varna_enforcement).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, hindu_personal_law_caste_privileges).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, anti_caste_constitutional_provisions).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gita_kurukshetra_discourse kernel. The gandhian_allegorical_reading and universalist_devotional_reading are sibling constraints with different ε, beneficiary/victim sets, and claimed types. The orthodox reading's literal mandate of varna and righteous violence forecloses the universalist reading's egalitarian bhakti within a single hermeneutic framework (forecloses relation), but coexists with the Gandhian reading as a parallel interpretive tradition held by different parties (coexists_with relation). All three share the kernel's fixed text but diverge on authority_grounding and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, institutional, 0.05).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerful, 0.25).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
