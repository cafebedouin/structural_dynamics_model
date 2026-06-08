% ============================================================================
% CONSTRAINT STORY: incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incoherence_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: incoherence_reading
 *   human_readable: Institutional Tolerance of Ontological Incoherence in Shinbutsu-Shugo
 *   domain: religious_studies/japanese_history/institutional_pragmatism
 *
 * SUMMARY:
 *   Shinbutsu-shugo (神仏習合), the institutional coexistence of Shinto and
 *   Buddhism in medieval and early-modern Japan, presents three structurally
 *   distinct readings of the same historical phenomenon. The INCOHERENCE
 *   READING claims that no stable ontological commitment existed — that
 *   institutional tolerance of theological contradiction (kami and
 *   bodhisattvas presented as both unified and separate, sometimes in the
 *   same ritual) was not backed by a coherent synthesis but by pragmatic
 *   indifference to the question. Communities performed both traditions,
 *   shrine and temple personnel collected income from both, and nobody was
 *   forced to articulate a unified metaphysics. This reading interprets
 *   shinbutsu-shugo as institutionalized evasion rather than syncretism. The
 *   constraint is the structure that maintained this tolerance: extraction
 *   flows to institutional pragmatists (priests, administrators) who benefit
 *   from flexibility; costs flow to doctrinal populations (believers seeking
 *   coherent theology) who bear cognitive dissonance; and the state apparatus
 *   enforces the arrangement by refusing to standardize doctrine. The
 *   incoherence reading is one lens among three (syncretic reading: kami are
 *   bodhisattvas' local manifestations; partition reading: kami and
 *   bodhisattvas occupy separate ontological domains). Each reading is
 *   empirically defensible from the historical record; each is grounded in
 *   different institutional actors' interests; each would classify the
 *   constraint differently. The incoherence reading instantiates a tangled
 *   rope at the analytical level where the observer risks being trapped in
 *   the oracle gap (Theorem 4) — the framework to see the incoherence
 *   requires refusing both synthesis and partition, which appears to be a
 *   commitment to incoherence itself, paradoxically.
 *
 * KEY AGENTS:
 *   - Doctrinal Believers (powerless/trapped): Seek coherent Buddhist or Shinto theology; cannot exit participation without losing community status and ritual access
 *   - Local Religious Specialists (moderate/identity-locked): Priests, shrine keepers, monks; benefit from dual income and institutional flexibility; identity fused with dual-tradition role
 *   - Pragmatist Administration (institutional/arbitrage): State bureaucrats, temple administrators; benefit from avoiding doctrinal standardization and maintaining institutional autonomy
 *   - Syncretist Intellectuals (organized/constrained): Theological systematizers building explicit kami-bodhisattva coherence frameworks; constrained by state resources and audience expectations
 *   - Meiji State Rationalization (institutional/arbitrage): Post-1868 state authority imposing doctrinal coherence ideology (Shinto nationalism) while enforcing separation, performing doctrinal mandates
 *   - Analytical Observer (analytical/identity-locked): Refuses both syncretic and partition readings; risks oracle gap by holding the 'no commitment' position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incoherence_reading, 0.35).
domain_priors:suppression_score(incoherence_reading, 0.42).
domain_priors:theater_ratio(incoherence_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incoherence_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(incoherence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(incoherence_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incoherence_reading, tangled_rope).
narrative_ontology:human_readable(incoherence_reading, "Institutional Tolerance of Ontological Incoherence in Shinbutsu-Shugo").
narrative_ontology:topic_domain(incoherence_reading, "religious_studies/japanese_history/institutional_pragmatism").

domain_priors:requires_active_enforcement(incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(incoherence_reading, '7c43b92c-b3d9-42d9-9e8b-a3712bd36a60').
narrative_ontology:cs_kernel_codification('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', distributed).
narrative_ontology:cs_authority_grounding('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', practice).
narrative_ontology:cs_interpretation_layer_present('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60').
narrative_ontology:cs_reading_relation('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', incoherence_reading__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', incoherence_reading__partition_reading, coexists_with).
narrative_ontology:cs_axiom('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', foundational, no_transcendent_unity).
narrative_ontology:cs_axiom_status(no_transcendent_unity, holdable).
narrative_ontology:cs_axiom_grounding('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', no_transcendent_unity, conventional).
narrative_ontology:cs_axiom('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', foundational, institutional_tolerance_supreme).
narrative_ontology:cs_axiom_status(institutional_tolerance_supreme, overridden).
narrative_ontology:cs_axiom_grounding('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', institutional_tolerance_supreme, instrumental).
narrative_ontology:cs_reference_frame('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', pragmatic_institutional_tolerance).
narrative_ontology:cs_drift_state('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', meiji_state_centralization, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7c43b92c-b3d9-42d9-9e8b-a3712bd36a60', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incoherence_reading, state_authority).
narrative_ontology:constraint_beneficiary(incoherence_reading, institutional_pragmatists).
narrative_ontology:constraint_victim(incoherence_reading, theological_coherence).
narrative_ontology:constraint_victim(incoherence_reading, doctrinal_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(incoherence_reading, local_religious_specialists).
narrative_ontology:constraint_victim(incoherence_reading, doctrinal_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents who seek coherent Buddhist or Shinto doctrine. They participate in shrine and temple rituals because community membership and spiritual access require it, but they experience ongoing cognitive dissonance from contradictory theological teachings. Cannot exit without losing religious community, social status, and ritual access to kami and bodhisattvas. Bear the full cost of sustained theological incoherence.
narrative_ontology:constraint_stakeholder(incoherence_reading, doctrinal_believers, payer,
    powerless, biographical, trapped, national).

% Priests, shrine keepers, monks, and ritual specialists who benefit from institutional tolerance through dual income streams (both shrine and temple revenue), communal authority (seen as bridge between traditions), and operational autonomy (no doctrinal standardization imposed from above). Identity fused with the role of dual-tradition mediator. Cannot abandon the arrangement without losing professional identity in the community's eyes. Set the institutional agenda by maintaining both traditions as equally legitimate.
narrative_ontology:constraint_stakeholder(incoherence_reading, local_religious_specialists, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(incoherence_reading, local_religious_specialists, agenda_setter).

% State bureaucrats, regional administrators, and temple management systems that benefit from avoiding doctrinal standardization. The institutional tolerance arrangement solves the coordination problem of maintaining dual religious infrastructure without centralizing authority or forcing a doctrinal choice. Can exit the arrangement by imposing doctrinal standardization (as Meiji does) without cost to institutional survival. Set the administrative agenda by refusing to standardize doctrine.
narrative_ontology:constraint_stakeholder(incoherence_reading, pragmatist_administration, agenda_setter,
    institutional, immediate, arbitrage, national).

% Theologians, scholars, and religious intellectuals building explicit coherence frameworks (kami-as-bodhisattva, original-essence metaphysics) to explain how Shinto and Buddhism can be unified. Constrained by state resources, audience expectations, and the need to maintain both traditions' authority. View the incoherence tolerance as a transitional stage whose function is to enable belief during the intellectual journey toward systematic synthesis. Set scholarly and doctrinal agendas through textual interpretation and metaphysical systematization.
narrative_ontology:constraint_stakeholder(incoherence_reading, syncretist_intellectuals, agenda_setter,
    organized, generational, constrained, national).

% The post-1868 state authority imposing doctrinal coherence ideology (Shinto nationalism, divine emperor cult) and officially mandating separation of Shinto from Buddhism (shinbutsu-bunri). Benefits from the ideological narrative of doctrinal purity and coherent state religion. Can exit the constraint by forcibly separating the traditions, which it does. Sets the national religious agenda through law and ideological dissemination.
narrative_ontology:constraint_stakeholder(incoherence_reading, meiji_state_rationalization, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The abstract good of doctrinal systematization and intellectual coherence. Not an agent; listed as a non-agent entry to capture the cost side. The constraint tolerates (and thus implicitly suppresses) theological coherence as a institutional value. Cannot organize, cannot exit, cannot collect rents. Bears structural invisibility in the pragmatist arrangement.
narrative_ontology:constraint_stakeholder(incoherence_reading, theological_coherence, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(incoherence_reading, theological_coherence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining dual religious authority structures (shrine and temple networks) across a geographically distributed feudal society without centralizing doctrinal control or forcing regional power centers to choose between Shinto and Buddhist legitimation.
% TRANSFER_FUNCTION: Income and authority flow toward local religious specialists (dual revenue streams, dual ritual control). Doctrinal coherence flows AWAY from believers (they bear dissonance without resolution). State autonomy flows toward pragmatist administration (they avoid doctrinal standardization costs).
% ABSENT_VOICES: Purists from both traditions (strict Buddhists, strict Shintoists) are systematically absent from the institutional conversation — they object to dual participation but are not seated in administration or local priesthood. Post-Meiji state rationalizers are also absent from Edo-Tokugawa arrangements until 1868.
% DISAPPEARANCE_RATIONALE: If the incoherence tolerance disappeared overnight, regional power centers would face forced doctrinal choice (Shinto-only legitimacy or Buddhist-only legitimacy), potentially fragmenting regional coalitions. Local communities would reorganize around purely Shinto or purely Buddhist institutions. The Meiji state does exactly this (shinbutsu-bunri, 1868) and the world rearranges: shrine and temple networks separate, institutional income streams diverge, and doctrinal coherence (Shinto nationalism) becomes state-mandated ideology. The constraint is load-bearing for the pre-Meiji arrangement.
% FOUNDING_PROBLEM: Regional feudal lords in medieval/early-modern Japan required both Shinto legitimacy (native kami, imperial connection) and Buddhist legitimacy (continental sophistication, philosophical authority) to rule. Forcing a choice between them destabilized regional coalitions. Institutional tolerance of dual participation solved this: lords could support both shrines and temples without choosing.
% FOUNDING_PROBLEM_CORROBORATION: Meiji state reformers explicitly declare the founding problem obsolete: a unified modern nation-state requires doctrinal coherence and Shinto purity (to strengthen imperial authority and national identity). Regional power centers no longer need dual legitimacy; they answer to the central state. Historians of Meiji modernization (Sakamoto Koremaru, Ketelaar, Josephson) corroborate that the founding problem — regional feudal legitimation requiring dual traditions — is historically specific to the Edo-Tokugawa period and terminates with centralization.
narrative_ontology:disappearance_verdict(incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(incoherence_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINAL BELIEVER (SNARE) — Adherents seeking coherent Buddhist or Shinto doctrine cannot exit the contradiction without abandoning either temple participation or doctrinal commitment. Trapped between institutional demand (perform both rites) and theological demand (reconcile the ontologies). Bears full cost of sustained cognitive dissonance.
constraint_indexing:constraint_classification(incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL RELIGIOUS SPECIALIST (TANGLED ROPE) — Priests and shrine keepers benefit from institutional tolerance (both income streams, communal authority). Identity fused with dual-tradition role. Cannot abandon the arrangement without becoming 'untraditional' in the community's eyes. Genuine coordination (managing community rituals) embedded within extraction (beneficiaries shield themselves from doctrinal pressure).
constraint_indexing:constraint_classification(incoherence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: PRAGMATIST ADMINISTRATION (ROPE) — State bureaucrats and temple administrators benefit from flexibility: no doctrinal standardization needed, no forced choice between traditions, maximal institutional autonomy. The arrangement solves the coordination problem of maintaining dual religious infrastructure without centralizing authority. Benefits flow clearly to institutional pragmatists.
constraint_indexing:constraint_classification(incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SYNCRETIST INTELLECTUALS (SCAFFOLD) — Theologians and religious scholars build explicit coherence frameworks (kami-as-bodhisattva, original-essence metaphysics) with a sunset: as modernity arrives, rigorous doctrinal systematization will replace pragmatic tolerance. The framework is designed as transitional — its stated function is to enable belief during the transition from feudal tolerance to modern intellectual standardization.
constraint_indexing:constraint_classification(incoherence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEIJI STATE RATIONALIZATION (PITON) — The incoherence tolerance persists after its functional justification decays. Under Meiji state building, doctrinal coherence becomes ideologically valued; shinbutsu-shugo is officially dissolved (1868) yet local practices persist through institutional inertia. The constraint becomes mostly theater — communities maintain dual rites through habit while the state's coherence mandate is performed (Shinto nationalism) rather than enforced locally.
constraint_indexing:constraint_classification(incoherence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER WITH IDENTITY LOCK (TANGLED_ROPE) — From a civilizational view, the incoherence reading itself is ontologically locked. The 'no stable commitment' framing only becomes visible if you refuse both the syncretic reading (commitment to kami-bodhisattva unity) and the partition reading (commitment to separate essences). But refusing both commitments requires accepting permanent instability as the commitment — a paradox that instantiates the oracle gap (Theorem 4). This perspective sees the incoherence reading as capturing something real (pragmatic tolerance without synthesis) while being unable to articulate what the commitment IS without contradicting itself.
constraint_indexing:constraint_classification(incoherence_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incoherence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incoherence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incoherence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(incoherence_reading, TR),
    TR >= 0.70.

:- end_tests(incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts value from doctrinal populations (who bear dissonance costs without resolution) toward institutional pragmatists (who pocket income without philosophical burden). But the extraction is not severe because the tolerance is genuinely profitable for religious specialists — they are not merely extracting from believers, they are also providing valued ritual services. The asymmetry is real but not maximal. Suppression (0.42): Moderate. Communities tolerate the incoherence rather than being forcibly suppressed, but suppression exists in the form of state refusal to standardize (which would require choosing one tradition over the other) and institutional pressure against doctrinal dissent. Believers have alternatives (choose pure Buddhism, choose pure Shinto) but face community costs for choosing. Theater Ratio (0.68): Moderate-high. By the Edo-Tokugawa period, much dual-tradition performance becomes ritualistic habit rather than theological engagement. The performative layer increases over the interval as intellectual coherence-seeking fades and institutional routine hardens. Measurements show suppression and theater increasing over time (t=0 to t=250) while extractiveness stabilizes after early growth, indicating a constraint that hardens its performative shell as its original functional justification (pragmatic flexibility) decays.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge sharply on whether the incoherence tolerance is a feature or a bug. The doctrinal believer experiences it as pure extraction (Snare): their intellectual needs are unmet and their exit is blocked. The local specialist experiences it as coordination with benefits (Tangled Rope/Rope): the arrangement enables their livelihood and community authority. The pragmatist administrator experiences it as pure coordination (Rope): solving the problem of maintaining two traditions without imposing unified doctrine. The syncretist intellectual experiences it as a transitional problem with a solution (Scaffold): systematic theology will resolve the incoherence. The Meiji state experiences it as a performative legacy to be officially rejected while locally tolerated (Piton). The analytical observer risks seeing themselves as trapped in incoherence by refusing both the syncretic and partition readings — this is the oracle gap instantiation. None of these perspectives contradict each other empirically; they are genuinely different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from their structural relationship to the extraction flow. Doctrinal believers: full targets (d≈1.0) because the constraint prevents them from accessing either coherent theology. They bear the dissonance cost with no option to exit the religious community. Local specialists: moderate targets despite being beneficiaries (d≈0.35-0.45) because their identity is locked into the arrangement — they cannot enjoy the income without maintaining the incoherence. Pragmatist administrators: full beneficiaries (d≈0.0) with maximum exit options (arbitrage) — they can walk away from the dual-tradition arrangement without cost. Syncretist intellectuals: constrained beneficiaries (d≈0.30-0.40) because their coherence frameworks are resources valued by the state, but they are constrained by state resource allocation and don't fully control how their syntheses are deployed. Meiji state: beneficiary (d<0.0) using the constraint's dissolution for ideological gain (Shinto nationalism). Analytical observer: identity-locked (e≈0.50-0.60) because accepting the incoherence reading requires holding a position that appears self-contradictory.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is institutional religious flexibility without doctrinal standardization. The functional justification: in a feudal context with autonomous regional power centers, maintaining both Shinto and Buddhist authority structures avoids forcing a doctrinal choice that would displease either tradition or create doctrinal instability. By the Meiji period (1868+), this functional justification has died — the state demands doctrinal standardization for national coherence. Yet the constraint persists in local practice through institutional inertia (piton). The mandatrophy is thus sharp: the constraint continues despite the mandate becoming historically obsolete. This is precisely the signal the piton classification captures: the performance of incoherence tolerance continues, but the function it was built for is gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tolerance_as_commitment,
    'Does institutional tolerance of incoherence constitute a stable commitment, or does treating incoherence as tolerable require denying that it''s a commitment at all?',
    'Examination of institutional boundary-setting: what contradictions WERE tolerated vs. what contradictions provoked institutional response? If incoherence tolerance is selective, it is tacitly grounded in a hidden coherence principle (foreclosing the incoherence reading). If truly comprehensive, the ''commitment to no commitment'' becomes logically unstable.',
    'If incoherence tolerance is universal: the reading is coherent and the constraint is Rope (institutional pragmatism). If tolerance is bounded: the reading masks a hidden syncretic or partition commitment, and the constraint is actually Tangled Rope (false incoherence). This is the core uncertainty between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tolerance_as_commitment, conceptual, 'Whether tolerance of incoherence is itself a coherent institutional commitment').

omega_variable(
    cognitive_dissonance_sustainability,
    'How many generations can doctrinal populations sustain belief-participation under ontological incoherence? Does dissonance accumulate or stabilize?',
    'Generational cohort analysis of shrine and temple patronage, ritual participation rates, and doctrinal knowledge transmission. Measure dissonance directly via confessional sources, correspondence, and ritual deviation patterns.',
    'If dissonance accumulates (generations 1→2→3 show declining coherence-seeking): the constraint is extractive (Snare) and unsustainable without state enforcement. If dissonance stabilizes: the constraint is a genuine Rope with psychological adaptation mechanisms, and communities develop practical tolerance skills.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_dissonance_sustainability, empirical, 'Intergenerational sustainability of ontological incoherence participation').

omega_variable(
    meiji_foreclosure_mechanism,
    'Does the Meiji state''s explicit rejection of shinbutsu-shugo (1868) foreclose the incoherence reading, or does local persistence of dual practice demonstrate that no single reading ever achieved institutional stability?',
    'Genealogy of post-1868 local religious practice in shrine-temple partnerships; examination of whether communities actively preserved incoherence or simply failed to implement state mandates due to institutional friction.',
    'If Meiji rejection is genuine foreclosure: the incoherence reading is historically bounded (Edo-Tokugawa era only), and the constraint should be reclassified as a Scaffold with known sunset. If post-1868 practice shows continued tolerance: the incoherence reading persists beneath state ideology, and Meiji represents performative state building (piton) rather than actual institutional change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_foreclosure_mechanism, empirical, 'Whether Meiji state building foreclosed the incoherence reading or masked its continuation').

omega_variable(
    sibling_reading_empirical_distinguishability,
    'Can the three sibling readings (incoherence, syncretic, partition) be distinguished by examining actual institutional and textual evidence, or do all three readings fit the observable record equally well?',
    'Comparative analysis: collect examples of (1) explicit theological coherence-building (supports syncretic reading), (2) sharp doctrinal boundary maintenance (supports partition reading), and (3) institutional tolerance without synthesis (supports incoherence reading). Map their frequencies and institutional grounding.',
    'If one reading dominates the evidence: the kernel has a natural attractor state, and the alternatives are minority positions. If all three readings have equivalent evidential support: the kernel is genuinely contestable, and no reading forecloses the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_distinguishability, empirical, 'Empirical distinguishability of the three ontological commitment readings').

omega_variable(
    incoherence_as_oracle_gap_instantiation,
    'Does the incoherence reading instantiate Deferential Realism''s oracle gap (Theorem 4: the analytical observer''s native instruments cannot detect the structure that cross-position analysis reveals), or is the reading''s logical instability a sign that the incoherence thesis is simply false?',
    'Structural analysis of whether the incoherence reading can be stabilized through commitment-system logic (kernel codification, authority grounding, reference frames) without self-contradiction. If it can be stabilized, the oracle gap is real. If it cannot, the reading is incoherent rather than coherent-about-incoherence.',
    'If oracle gap is instantiated: the incoherence reading is a genuine, stable position that other readings cannot see from within their own frameworks — this is precisely what the oracle gap predicts. If the reading is simply incoherent: it should be abandoned in favor of syncretic or partition readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incoherence_as_oracle_gap_instantiation, conceptual, 'Whether incoherence reading instantiates the oracle gap or is logically untenable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incoherence_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incoh_theater_t0, incoherence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(incoh_theater_t150, incoherence_reading, theater_ratio, 150, 0.52).
narrative_ontology:measurement(incoh_theater_t250, incoherence_reading, theater_ratio, 250, 0.68).

% Extraction over time
narrative_ontology:measurement(incoh_extract_t0, incoherence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(incoh_extract_t150, incoherence_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(incoh_extract_t250, incoherence_reading, base_extractiveness, 250, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(incoh_suppress_t0, incoherence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(incoh_suppress_t150, incoherence_reading, suppression_requirement, 150, 0.42).
narrative_ontology:measurement(incoh_suppress_t250, incoherence_reading, suppression_requirement, 250, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(incoherence_reading, 0.12).
narrative_ontology:affects_constraint(incoherence_reading, syncretic_reading).
narrative_ontology:affects_constraint(incoherence_reading, partition_reading).
narrative_ontology:affects_constraint(incoherence_reading, meiji_state_doctrinal_standardization).
narrative_ontology:affects_constraint(incoherence_reading, shinto_nationalism_ideology).

% DUAL FORMULATION NOTE:
% The three readings (incoherence, syncretic, partition) of the shinbutsu_ontological_commitment kernel are separate constraint stories with different ε values and different beneficiary structures. The incoherence reading models institutional indifference to reconciling the two traditions; the syncretic reading models active theological synthesis; the partition reading models explicit boundary maintenance. All three readings operate within the same historical period (Edo-Tokugawa) and the same empirical record (dual-tradition institutions), but with different epsilon values reflecting different ontological commitments. They are linked by network dependency: Meiji state doctrinal standardization (forced Shinto/Buddhism separation) forecloses the incoherence reading and the syncretic reading (demands partition logic), and simultaneously enables Shinto nationalism ideology (which requires Shinto purity, also partition logic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incoherence_reading, analytical, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
