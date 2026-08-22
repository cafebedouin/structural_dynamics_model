% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Quranic Jihad as Defensive Spiritual and Armed Struggle
 *   domain: religious/political theology
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested Quranic jihad
 *   corpus: the defensive-spiritual reading. The Quranic text addresses both
 *   internal spiritual struggle and armed defense; it permits defense against
 *   aggression and provides ethical rules for conduct in armed conflict, but
 *   the reading's core claim is that the primary and greater form of jihad is
 *   internal spiritual struggle (jihad al-nafs) and that armed jihad is
 *   permitted only defensively, requiring state authority, proportionality
 *   assessment, and protection of non-combatants. This reading is the
 *   mainstream consensus in classical Islamic jurisprudence and contemporary
 *   scholarly institutions. It competes with two sibling readings: the
 *   expansionist-legalist reading (which permits offensive jihad to establish
 *   Islamic governance under specific jurisprudential conditions) and the
 *   revolutionary-vanguard reading (which claims immediate individual
 *   obligation overrides state authority under emergency circumstances). The
 *   constraint's structure privileges state authority, scholarly consensus,
 *   and ethical constraints, thereby excluding non-state actors and
 *   unaccountable individuals from legitimate jihad declaration. Non-Muslims
 *   are protected unless they are aggressors, creating a victim set that does
 *   not automatically include them.
 *
 * KEY AGENTS:
 *   - Islamic scholarly consensus (institutional agenda-setter and beneficiary): maintains the interpretive authority that grounds this reading and enforces gatekeeping on legitimate jihad claims
 *   - Muslim communities under aggression (beneficiary): gain recognized defensive capacity under this constraint while remaining bounded by proportionality and non-combatant immunity
 *   - States defending territory (institutional beneficiary): authorized to declare and execute armed jihad on behalf of their populations, with centralized decision-making authority
 *   - Non-Muslim populations (beneficiary by exclusion from victim set): protected by the non-combatant immunity norm unless they are aggressors
 *   - Unaccountable armed actors (excluded): militias, individuals, and splinter groups lack the institutional standing to declare jihad under this reading
 *   - International humanitarian law observers (analytical seat): compare this reading's structural constraints with secular proportionality and combatant/non-combatant distinction norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.28).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.41).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Quranic Jihad as Defensive Spiritual and Armed Struggle").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/political theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '3d50a3f9-0118-41e1-814e-415816aa4c55').
narrative_ontology:cs_kernel_codification('3d50a3f9-0118-41e1-814e-415816aa4c55', fixed_text).
narrative_ontology:cs_authority_grounding('3d50a3f9-0118-41e1-814e-415816aa4c55', lineage).
narrative_ontology:cs_interpretation_layer_present('3d50a3f9-0118-41e1-814e-415816aa4c55').
narrative_ontology:cs_reading_relation('3d50a3f9-0118-41e1-814e-415816aa4c55', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d50a3f9-0118-41e1-814e-415816aa4c55', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('3d50a3f9-0118-41e1-814e-415816aa4c55', foundational, internal_spiritual_struggle_primary).
narrative_ontology:cs_axiom_status(internal_spiritual_struggle_primary, holdable).
narrative_ontology:cs_axiom_grounding('3d50a3f9-0118-41e1-814e-415816aa4c55', internal_spiritual_struggle_primary, deontological).
narrative_ontology:cs_axiom('3d50a3f9-0118-41e1-814e-415816aa4c55', foundational, state_authority_requirement_legitimacy).
narrative_ontology:cs_axiom_status(state_authority_requirement_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3d50a3f9-0118-41e1-814e-415816aa4c55', state_authority_requirement_legitimacy, conventional).
narrative_ontology:cs_axiom('3d50a3f9-0118-41e1-814e-415816aa4c55', secondary, noncombatant_immunity_absolute).
narrative_ontology:cs_axiom_status(noncombatant_immunity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3d50a3f9-0118-41e1-814e-415816aa4c55', noncombatant_immunity_absolute, deontological).
narrative_ontology:cs_reference_frame('3d50a3f9-0118-41e1-814e-415816aa4c55', quranic_proportional_defense_framework).
narrative_ontology:cs_drift_state('3d50a3f9-0118-41e1-814e-415816aa4c55', contemporary_jihadist_proliferation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d50a3f9-0118-41e1-814e-415816aa4c55', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholarly_consensus).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, states_defending_territory).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_under_aggression).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive tradition grounding jihad in internal struggle (jihad al-nafs) as primary and armed defense as secondary, strictly conditioned. Scholarly bodies issue fatwa (legal rulings) that reinforce proportionality, non-combatant immunity, and state authority requirements. This reading privileges their authority to adjudicate legitimate armed response and excludes counter-readings from unaccountable actors.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholarly_consensus, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholarly_consensus, beneficiary).

% Communities suffering military aggression, territorial occupation, or systematic persecution gain legitimacy for organized armed defense under this framework. The reading permits them to mount resistance against aggressors while maintaining constraint against retaliation against non-combatants and against unilateral declaration without oversight. Their defensive capacity is recognized but bounded.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_under_aggression, beneficiary,
    organized, generational, trapped, regional).

% Nation-states claiming legitimacy under this reading can marshal armed forces against invasion and aggression while remaining within the constraint. The reading requires state authority for legitimate armed jihad, which consolidates defense decision-making and excludes non-state actors (militias, individuals) from unilateral declaration. States benefit from the constraint's gatekeeping function.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, states_defending_territory, beneficiary,
    institutional, generational, mobile, national).

% Non-Muslims not party to aggression are excluded from the victim set and protected by the non-combatant immunity norm. They live under the jurisdictions where this reading applies and depend on the scholarly consensus enforcement of these protections to limit violence directed at them. They cannot be targeted as such under this reading, though they may be affected by collateral consequence of legitimate defense.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_populations, beneficiary,
    powerless, biographical, constrained, regional).

% Non-state militias, individuals, and splinter groups that claim jihad authority without scholarly consensus or state sanction are structurally excluded from legitimate action under this reading. They lack the institutional standing and oversight to declare or execute armed jihad. Their exclusion is what the enforcement mechanism (scholarly authority, state gatekeeping) exists to maintain.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, unaccountable_armed_actors, excluded,
    moderate, biographical, trapped, regional).

% International humanitarian law scholars and bodies (UN bodies, ICRC, academic analysts) analyze whether this reading aligns with norms of proportionality, combatant/non-combatant distinction, and legitimate armed response under international law. Their analysis tests whether the reading's constraints are structurally coherent with secular law principles.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_international_law_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for legitimate armed response by Muslim communities and states against aggression while constraining that response through proportionality, non-combatant immunity, state authority requirement, and prioritizing internal spiritual struggle as the primary form of jihad. Solves the problem: how can Muslims defend themselves and their faith while remaining within a coherent ethical and legal structure that prevents unlimited violence and authorizes only those with institutional standing and knowledge to declare armed struggle.
% TRANSFER_FUNCTION: Transfers authority to declare and execute armed jihad from individual believers to state authorities and established scholarly consensus. Moves the understanding of jihad from exclusively internal-spiritual work to a bounded category that includes defensive armed action, but only under strict conditions. Moves constraint from permitting any believer to wage war (under the expansionist reading) to requiring state sanction, proportionality review, and non-combatant immunity.
% ABSENT_VOICES: Revolutionary vanguard actors (takfir practitioners, unaccountable militias, individuals who claim direct divine authorization) are structurally excluded; they would argue that immediate individual obligation overrides state authority and scholarly gatekeeping in emergency situations. Expansionist legalist scholars who read the tradition as permitting offensive jihad to establish Islamic governance are marginalized in this reading. Their absence from the consensus is enforced by the same institutional machinery (scholarly authority) that the reading depends on.
% DISAPPEARANCE_RATIONALE: If this constraint (the binding authority of the defensive-spiritual reading, the state authority requirement, the non-combatant immunity norm) vanished overnight, the interpretive field would rapidly shift: revolutionary readings and expansionist legalist interpretations would gain institutional claim, non-Muslim populations would lose the protection that this reading's consensus provides, and armed actors would lack a binding reference frame for constraining their violence. The absence would reorganize the landscape of legitimate and illegitimate jihad claims.
% FOUNDING_PROBLEM: The Quranic corpus contains both verses emphasizing internal spiritual struggle (the greater jihad / jihad al-nafs) and verses permitting armed defense against aggression and addressing military strategy. The founding problem is: how to read these verses coherently such that armed struggle is permitted but bounded, internal struggle is privileged, and non-combatants are protected—thereby reconciling the permission for defense with the ethical constraints the tradition elsewhere upholds.
% FOUNDING_PROBLEM_CORROBORATION: This reading is attested by centuries of mainstream Islamic jurisprudence (Maliki, Hanafi, Shafi'i, Hanbali schools) and contemporary scholarly consensus bodies (Al-Azhar, International Islamic Fiqh Academy, major hadith scholars). However, the reading is contested: expansionist legalists cite historical Umayyad and Abbasid-era jurisprudence permitting offensive jihad; revolutionary vanguard actors cite emergency-jurisprudence (darurah) traditions and takfir doctrine to override the state authority gate. Secular international law bodies and human rights organizations recognize the proportionality and non-combatant immunity elements but are external corroborators, not internal tradition voices—they verify structural alignment, not the reading's standing within Islam.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the reading's core beneficiaries are institutional (the scholarly consensus, the state defending its territory, the Muslim communities) and the constraint does not direct extraction to a specific rent-capturing seat. The scholarly consensus benefits from maintaining interpretive authority, but that benefit is the maintenance of epistemic gatekeeping, not material extraction—it is the price of having a coherent, bounded tradition. Suppression is moderate (0.41) because the constraint actively suppresses counter-readings (revolutionary vanguard, expansionist legalism) through institutional gatekeeping and scholarly authority, but this suppression is structural to the constraint's operation, not primarily coercive: it works through legitimacy claims and interpretive authority rather than force. Theater ratio is low (0.22) because the constraint's functional elements (proportionality review, non-combatant immunity, state authority gatekeeping) are genuinely performed and serve real constraint functions, though some institutional performance of consensus is theatrically maintained. Accessibility collapse is moderate (0.62) because while the defensive-spiritual reading is the mainstream consensus, alternative readings remain live in some scholarly circles and revolutionary actors reject the state authority gate—alternatives have collapsed for institutional Islam but not completely across the Muslim-majority world. Resistance is high (0.58) because revolutionary actors, some splinter groups, and expansionist scholars actively resist this reading's constraints and mount counter-arguments from the same corpus. The measurements show slight increases in extractiveness, theater, and suppression over the 50-year interval, reflecting increasing institutional pressure to maintain the consensus against proliferating counter-claims and rising armed-actor activity outside the state framework.
 *
 * PERSPECTIVAL GAP:
 *   From the scholarly consensus seat: this is a genuine coordination mechanism solving the problem of coherent Quranic interpretation and protecting civilian life. From the revolutionary-vanguard seat: this is institutional gatekeeping that wrongly subordinates direct divine obligation to human authority. From the non-Muslim population seat: this is a protection mechanism (non-combatant immunity) but depends on whether the warrior actors accept the constraint. The engine computes these gaps from power/exit asymmetry; no additional clarity is needed—the structural data captures the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic scholarly consensus and states sit near the beneficiary end (d ~0.2–0.3): they maintain the reading's authority, set the interpretation, and gain institutional standing. Muslim communities under aggression sit near symmetric (d ~0.5): they benefit from the constraint's permission for defense and its protection through non-combatant immunity, but they also bear the constraint's cost in the requirement for state authority and scholarly validation—they cannot act unilaterally. Non-Muslim populations benefit from protection (non-combatant immunity) but bear the cost of living under a constraint they did not author—d is complex here, closer to 0.4 (slight net benefit from protective norm). Unaccountable armed actors sit at the target end (d ~0.75–0.85): the constraint suppresses their claims, excludes them from legitimate action, and forces them to operate outside the recognized framework. Revolutionary scholars have d ~0.65–0.75: suppressed but not entirely trapped (they can publish, teach, argue; they are not physically excluded), but their readings are delegitimized by the consensus gate. No directionality overrides are necessary; the derivation from beneficiary/victim + power + exit aligns with the structural situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to read the Quranic corpus coherently) remains live, but the founding problem has partially shifted: the primary now-disputed question is not whether jihad can be defensive and spiritually internal, but whether the state authority requirement and scholarly gatekeeping are legitimate constraints on the right to defend oneself. The constraint prevents confusion between its coordination function (settling the reading of the corpus) and its extraction function (gatekeeping authority over legitimate jihad). The reading's classification as rope (coordination-dominated) is defensible because the core function is establishing a coherent, bounded interpretation that serves coordination within Islamic tradition. However, the suppression score (0.41) and the exclusion of unaccountable actors indicate a tangled-rope component: the reading coordinates the scholarly consensus but extracts authority from non-institutional actors who would claim direct divine authorization. The classification CLAIMED is rope; the metrics suggest tangled-rope is defensible. The mandatrophy check: if the constraint vanished, would the coordination problem persist? Yes—the Quranic corpus would still require interpretation, and without this constraint, the field would splinter across expansionist, revolutionary, and defensive readings all claiming legitimacy. That persistent need-for-coordination supports the rope classification. However, if the suppression function vanished (no gatekeeping against revolutionary readings), the scholarly consensus would lose its authority to settle interpretation, and some institutional extraction would be lost. This is the tangled-rope component: coordination + asymmetric authority extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the Quranic text deterministic on the relative primacy of internal spiritual jihad versus armed jihad, or does the text support multiple coherent readings?',
    'Systematic corpus analysis of Quranic verses addressing both forms, examining whether any single verse or chapter provides a clear hierarchy, and whether the text''s own apparent tensions (permission for defense alongside emphasis on patience and forbearance) can be resolved into a single reading or require acknowledging genuine ambiguity.',
    'If the text is deterministic, this reading has higher natural-law character; if genuinely ambiguous, the reading is an authorized interpretation among others, and the constraint''s persistence depends partly on institutional maintenance rather than textual inevitability. This feeds the authority-grounding question in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel (Quranic corpus) univocally grounds this reading or supports multiple readings.').

omega_variable(
    state_authority_requirement_legitimacy,
    'Is state authority a requirement grounded in the Quranic text itself, or is it a jurisprudential interpretation added by classical scholars to constrain violence?',
    'Textual analysis of Quranic verses addressing authority for armed action, cross-referenced with classical jurisprudential commentary to identify where the state-authority gate was introduced and on what textual basis.',
    'If state authority is textually grounded, it is more resistant to challenge from rival readings; if it is a jurisprudential addition, revolutionary readers can more plausibly claim that textual reading supports their challenge to the gate. This determines whether the constraint''s suppression of revolutionary readings is defending a textual core or a jurisprudential construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_requirement_legitimacy, empirical, 'Whether state authority is a Quranic requirement or a jurisprudential imposition.').

omega_variable(
    institutional_consensus_enforcement_mechanism,
    'How does the scholarly consensus maintain authority to gatekeep legitimate jihad claims in contexts where revolutionary actors and splinter groups have direct access to the Quranic text and unmediated authority claims?',
    'Ethnographic and institutional analysis of how Al-Azhar, international Islamic fiqh academies, and state-sponsored Islamic authorities enforce consensus in contexts where non-institutional actors proliferate. Examine what happens when consensus enforcement fails (e.g., during wars, state collapse, or rapid urbanization).',
    'If consensus enforcement is strong, the constraint persists as institutional rope (coordination through authority). If enforcement is weak, the constraint degrades into a contested interpretation among many, and revolutionary readings gain ground. This affects the suppression measurement and whether the constraint persists or undergoes mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_consensus_enforcement_mechanism, empirical, 'What mechanisms maintain scholarly gatekeeping authority over jihad legitimacy.').

omega_variable(
    non_combatant_immunity_contested_application,
    'Does the constraint''s non-combatant immunity norm provide genuine protection in practice, or is the norm interpreted flexibly (e.g., through extended notions of what constitutes ''combatants'') such that suppression of civilians can be justified?',
    'Case analysis of historical and contemporary applications of the constraint by state and non-state actors claiming this reading: examine whether non-combatants are protected, whether collateral damage is constrained by proportionality review, or whether the norm is used as cover for de facto unlimited violence.',
    'If the norm provides genuine protection, the constraint serves non-Muslim populations and constrains violence. If the norm is interpreted away in practice, the constraint''s extractiveness rises and suppression increases (institutional gatekeeping becomes performative)—theater ratio rises and the constraint risks degradation to piton or reclassification as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_combatant_immunity_contested_application, empirical, 'Whether non-combatant immunity is genuinely protected or reinterpreted to permit civilian targeting.').

omega_variable(
    sibling_reading_suppression_mechanism,
    'By what mechanism does this reading suppress the expansionist-legalist and revolutionary-vanguard readings?',
    'Institutional analysis of how scholarly consensus bodies marginalize alternative readings: through counter-argument, authority claims, control of educational curriculum, institutional exclusion from publishing and teaching platforms, or force. Examine whether suppression is epistemic (argument) or coercive (exclusion from voice).',
    'If suppression is primarily epistemic (superior argument, scholarly consensus based on textual analysis), the constraint is defensible rope. If suppression is coercive (exclusion, institutional gatekeeping without refutation), the constraint is tangled_rope (coordination + extraction). This affects whether mandatrophy analysis can identify institutional inertia masking authority extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_suppression_mechanism, empirical, 'The suppression mechanism maintaining this reading''s dominance over sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(jiha_su_t50, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 50, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the jihad_quranic_corpus kernel family. The kernel is the Quranic text and its interpretive tradition; this story instantiates the defensive_spiritual_reading. Sibling stories instantiate expansionist_legalist_reading and revolutionary_vanguard_reading, each with its own ε, victim set, and suppression mechanism. The three readings share the same referent (the Quranic corpus) but represent different structural claims about what the text permits and requires. The defensive_spiritual reading has the lowest extractiveness (0.28) and emphasizes protection of non-combatants; the expansionist reading has moderate extractiveness and justifies offensive campaigns; the revolutionary reading has the highest extractiveness (unaccountable authority claims) and suppresses state gatekeeping. These are not the same constraint viewed differently—they are structurally distinct claims with different ε values, victim sets, and authority structures. The upstream reading (defensive_spiritual, the scholarly consensus position) influences both downstream readings (expansionist, revolutionary) by establishing the reference frame that both must argue against, but neither is logically foreclosed by the other—all three coexist as live positions in contemporary Islamic discourse. They form a network of constraint family linked by kernel contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
