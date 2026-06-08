% ============================================================================
% CONSTRAINT STORY: hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanafi_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Qiyas and Istihsan as Valid Sources
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Hanafi jurisprudential method, founded by Abu Hanifa (d. 767 CE) and
 *   systematized by his students, privileges systematic analogical reasoning
 *   (qiyas) and juristic preference (istihsan) as valid sources of law
 *   alongside Qur'an and Hadith. This reading of Islamic legal methodology
 *   became dominant in the Ottoman Empire, Central Asia, and South Asia,
 *   shaping legal practice for hundreds of millions of Muslims. The
 *   constraint exhibits tangled-rope structure: it solves genuine
 *   coordination problems (applying fixed texts to novel circumstances,
 *   enabling commercial law development) while simultaneously extracting from
 *   textualist interpretive traditions by institutionalizing rationalist
 *   juristic authority. The method requires active enforcement through
 *   madhhab loyalty, institutional training in Hanafi usul, and scholarly
 *   gatekeeping that marginalizes alternative interpretive approaches.
 *   Theater ratio (0.42) reflects moderate performative content: some
 *   invocations of qiyas and istihsan serve to legitimate predetermined
 *   conclusions rather than genuinely reason from principles, but much of the
 *   method's application is functionally rigorous. Extractiveness (0.35) and
 *   suppression (0.48) both increased during the Ottoman consolidation period
 *   (time_point 3-6) as state patronage institutionalized Hanafi dominance,
 *   then stabilized in the contemporary period as alternative madhhabs
 *   regained institutional space.
 *
 * KEY AGENTS:
 *   - Rationalist Jurists: Primary beneficiaries (institutional/arbitrage) — gain interpretive authority through qiyas and istihsan; can synthesize across legal schools
 *   - Commercial Actors: Secondary beneficiaries (institutional/mobile) — benefit from flexible contract law and pragmatic commercial rulings enabled by istihsan
 *   - Urban Legal Practitioners: Secondary beneficiaries (institutional/mobile) — benefit from systematic method and established precedent in urban legal centers
 *   - Strict Textualists: Primary victims (powerless/identity_locked) — scholarly identity constituted through textualist interpretive tradition; experience rationalist method as illegitimate innovation
 *   - Rural Traditional Communities: Secondary victims (moderate/constrained) — bear costs when istihsan-based rulings override local customary practice; benefit from some legal stability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination function and asymmetric extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.35).
domain_priors:suppression_score(hanafi_reading, 0.48).
domain_priors:theater_ratio(hanafi_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hanafi_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hanafi_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi Jurisprudential Method: Qiyas and Istihsan as Valid Sources").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanafi_reading, '20965949-bd4a-446b-bcce-d297a55b9ed7').
narrative_ontology:cs_kernel_codification('20965949-bd4a-446b-bcce-d297a55b9ed7', formalized).
narrative_ontology:cs_authority_grounding('20965949-bd4a-446b-bcce-d297a55b9ed7', lineage).
narrative_ontology:cs_interpretation_layer_present('20965949-bd4a-446b-bcce-d297a55b9ed7').
narrative_ontology:cs_reading_relation('20965949-bd4a-446b-bcce-d297a55b9ed7', hanafi_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('20965949-bd4a-446b-bcce-d297a55b9ed7', hanafi_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('20965949-bd4a-446b-bcce-d297a55b9ed7', hanafi_reading__hanbali_reading, influences).
narrative_ontology:cs_axiom('20965949-bd4a-446b-bcce-d297a55b9ed7', foundational, reason_as_independent_legal_source).
narrative_ontology:cs_axiom_status(reason_as_independent_legal_source, holdable).
narrative_ontology:cs_axiom_grounding('20965949-bd4a-446b-bcce-d297a55b9ed7', reason_as_independent_legal_source, deontological).
narrative_ontology:cs_axiom('20965949-bd4a-446b-bcce-d297a55b9ed7', foundational, istihsan_juristic_preference_validity).
narrative_ontology:cs_axiom_status(istihsan_juristic_preference_validity, holdable).
narrative_ontology:cs_axiom_grounding('20965949-bd4a-446b-bcce-d297a55b9ed7', istihsan_juristic_preference_validity, conventional).
narrative_ontology:cs_axiom('20965949-bd4a-446b-bcce-d297a55b9ed7', secondary, qiyas_broad_analogical_scope).
narrative_ontology:cs_axiom_status(qiyas_broad_analogical_scope, holdable).
narrative_ontology:cs_axiom_grounding('20965949-bd4a-446b-bcce-d297a55b9ed7', qiyas_broad_analogical_scope, instrumental).
narrative_ontology:cs_reference_frame('20965949-bd4a-446b-bcce-d297a55b9ed7', abu_hanifa_founding_method).
narrative_ontology:cs_drift_state('20965949-bd4a-446b-bcce-d297a55b9ed7', contemporary_post_ottoman, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20965949-bd4a-446b-bcce-d297a55b9ed7', '').
narrative_ontology:cs_kernel_id(hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(hanafi_reading, commercial_actors).
narrative_ontology:constraint_beneficiary(hanafi_reading, urban_legal_practitioners).
narrative_ontology:constraint_victim(hanafi_reading, strict_textualists).
narrative_ontology:constraint_victim(hanafi_reading, rural_traditional_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hanafi_reading, rural_traditional_communities).
narrative_ontology:constraint_vindicates(hanafi_reading, reason_as_independent_source).
narrative_ontology:constraint_vindicates(hanafi_reading, juristic_discretion_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars trained in Hanafi usul who set legal agendas through qiyas and istihsan application. Control interpretive authority in major legal centers. Can synthesize across madhhabs or operate in contexts where Hanafi method is not dominant. Primary beneficiaries of the method's flexibility and institutional prestige.
narrative_ontology:constraint_stakeholder(hanafi_reading, rationalist_jurists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Merchants, traders, and business entities operating under Hanafi commercial law. Benefit from istihsan-enabled pragmatic rulings on contracts, partnerships, and dispute resolution. Can shift to alternative legal regimes or negotiate jurisdictional choice. Do not set the legal agenda but collect from its flexibility.
narrative_ontology:constraint_stakeholder(hanafi_reading, commercial_actors, beneficiary,
    institutional, biographical, mobile, national).

% Judges, muftis, and legal advisors in urban centers where Hanafi method is institutionally dominant. Benefit from systematic precedent and established interpretive framework. Mobile within the broader Islamic legal ecosystem but constrained by madhhab loyalty norms within specific jurisdictions.
narrative_ontology:constraint_stakeholder(hanafi_reading, urban_legal_practitioners, beneficiary,
    institutional, biographical, mobile, national).

% Scholars whose interpretive identity is constituted through strict textualist methodology. Experience Hanafi rationalism as illegitimate innovation (bid'ah). Cannot exit without abandoning their intellectual framework. Bear the cost of marginalization as rationalist method dominates institutional legal discourse.
narrative_ontology:constraint_stakeholder(hanafi_reading, strict_textualists, payer,
    powerless, biographical, identity_locked, regional).

% Communities geographically and institutionally distant from urban legal centers. Bear costs when istihsan-based rulings override local customary practice ('urf). Also benefit from legal stability and predictable rulings when Hanafi precedent is applied consistently. Constrained by distance from legal institutions and limited capacity to challenge rulings.
narrative_ontology:constraint_stakeholder(hanafi_reading, rural_traditional_communities, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hanafi_reading, rural_traditional_communities, beneficiary).

% Comparative legal scholar or historian examining the Hanafi method from outside any madhhab commitment. Sees both the genuine coordination function (enabling legal reasoning in novel contexts) and the asymmetric extraction structure (rationalist authority institutionalized through state patronage and scholarly gatekeeping). Neither collects from nor pays into the constraint.
narrative_ontology:constraint_stakeholder(hanafi_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Hanafi method coordinates legal reasoning across diverse contexts by providing systematic analogical principles (qiyas) and juristic discretion mechanisms (istihsan) for applying fixed textual sources to novel circumstances. Solves the genuine problem of legal adaptation without requiring new revelation.
% TRANSFER_FUNCTION: The arrangement transfers interpretive authority from textualist scholars (who ground rulings in explicit textual indication) to rationalist jurists (who ground rulings in analogical reasoning and juristic preference). Also transfers legal flexibility to commercial actors and urban practitioners who benefit from istihsan-enabled pragmatic rulings.
% ABSENT_VOICES: Strict textualist scholars who reject rationalist premises are marginalized in institutional legal discourse, especially in regions where Hanafi method achieved state patronage (Ottoman Empire, Mughal India). Rural communities whose customary practices ('urf) are overridden by istihsan-based rulings have limited voice in urban-centered legal institutions. These seats are present in the broader Islamic legal ecosystem but structurally excluded from Hanafi institutional centers.
% DISAPPEARANCE_RATIONALE: If the Hanafi method disappeared, legal practice would rearrange around alternative interpretive frameworks. Commercial law would lose flexibility (istihsan-enabled pragmatic rulings). Rationalist jurists would lose institutional authority. Textualist scholars would gain relative authority. The coordination problem (applying fixed texts to novel contexts) would remain but would be solved through different methods (Maliki custom-based reasoning, Shafi'i strict textualism, Hanbali hadith primacy). The world would not stay the same — arrangements depend on this specific interpretive framework.
% FOUNDING_PROBLEM: The founding problem was enabling legal reasoning beyond explicit Qur'anic and Hadith sources to address novel circumstances in rapidly expanding Islamic civilization (8th century CE). Abu Hanifa faced cases with no direct textual precedent in newly conquered territories with diverse customary practices. The problem was: how to derive law systematically when revelation is fixed but circumstances change?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live: contemporary Islamic legal practice continues to face novel circumstances (bioethics, financial instruments, digital technology) requiring reasoning beyond explicit textual sources. Corroboration: comparative legal scholars (Wael Hallaq, Mohammad Fadel) document ongoing debates over analogical reasoning scope across madhhabs. Rationalist jurists within the Hanafi tradition affirm the problem's persistence. Critically, textualist scholars (Hanbali tradition, contemporary Salafi critics) also corroborate that the problem is live — they simply dispute that rationalist methods are the legitimate solution. The problem's liveness is not contested; the solution is.
narrative_ontology:disappearance_verdict(hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(hanafi_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRICT TEXTUALIST (SNARE) — Identity-locked within a textualist interpretive tradition that sees qiyas expansion and istihsan as illegitimate innovation. Cannot exit without abandoning scholarly identity. Experiences the Hanafi method as extractive: their textual authority is systematically subordinated to rationalist juristic preference. The constraint suppresses alternative interpretive pathways by institutional dominance of Hanafi method in key legal centers.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL TRADITIONAL COMMUNITY (TANGLED ROPE) — Constrained by geographic and institutional distance from urban legal centers where Hanafi method dominates. Benefits from some coordination (predictable legal rulings, established precedent) but bears costs when istihsan-based rulings override local customary practice ('urf). Mixed experience: the method provides legal stability but extracts by marginalizing non-rationalist interpretive traditions.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RATIONALIST JURIST (ROPE) — Primary beneficiary with arbitrage-level exit options (can move between legal schools or synthesize methods). Experiences the Hanafi method as pure coordination: qiyas and istihsan solve the genuine problem of applying fixed texts to novel circumstances. The method's flexibility enables commercial law development, urban governance, and cross-cultural legal synthesis. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: COMMERCIAL ACTOR (ROPE) — Benefits from Hanafi method's flexibility in contract law, partnership structures, and commercial dispute resolution. Istihsan enables pragmatic rulings that accommodate market realities. Mobile exit options (can operate under different legal regimes or negotiate jurisdictional choice). Experiences the constraint as coordination mechanism enabling complex economic activity.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the Hanafi method coordinates legal reasoning across diverse contexts (genuine coordination function) while simultaneously extracting from textualist traditions by institutionalizing rationalist interpretive authority. The method requires active enforcement through madhhab loyalty, institutional training, and scholarly gatekeeping. Asymmetric extraction is structural: rationalist jurists gain interpretive authority; textualists lose it. This is the claimed type.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Hanafi method extracts from textualist traditions by institutionalizing rationalist interpretive authority, but the extraction is not severe — textualist scholars retain institutional space within the broader Islamic legal ecosystem, and the method's flexibility generates genuine legal innovation that benefits multiple stakeholders. The value reflects real asymmetry (rationalist jurists gain authority at textualists' expense) without overstating the severity. Suppression (0.48): Moderate-high. Significant barriers to alternative interpretive approaches include madhhab loyalty norms, institutional training requirements, state patronage of Hanafi institutions (historically), and scholarly gatekeeping. But suppression is not total — other madhhabs coexist, textualist critiques circulate, and contemporary legal pluralism has reduced institutional dominance. The suppression trajectory shows Ottoman-era peak (0.50 at time_point 6) followed by modest contemporary decline (0.48). Theater ratio (0.42): Moderate. Some qiyas and istihsan invocations are performative (legitimating predetermined conclusions), but much of the method's application involves genuine analogical reasoning and principled juristic discretion. The theater has increased over the interval as institutional consolidation created incentives for formulaic application, but remains substantially below piton threshold.
 *
 * PERSPECTIVAL GAP:
 *   The Hanafi method demonstrates indexical classification across power and exit dimensions. Rationalist jurists with arbitrage options see pure coordination (rope) — the method solves the genuine problem of legal reasoning in novel contexts. Commercial actors with mobile options also see coordination — istihsan enables pragmatic commercial law. Strict textualists with identity-locked exit see extraction (snare) — their interpretive authority is systematically subordinated. Rural communities with constrained exit see mixed coordination and extraction (tangled_rope) — legal stability alongside marginalization of customary practice. The analytical observer sees tangled_rope at the civilizational scale: genuine coordination function (applying fixed texts to changing circumstances) coexisting with asymmetric extraction (rationalist authority institutionalized at textualist expense). The gap is not a measurement error — it reflects real differences in structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist jurists are primary beneficiaries with arbitrage-level exit options — they can move between legal schools, synthesize methods, or operate in contexts where Hanafi dominance is weak. The engine derives low d (beneficiary status + arbitrage exit) producing low or negative effective extraction. Commercial actors are secondary beneficiaries with mobile exit options — they benefit from Hanafi flexibility but can also operate under other legal regimes. Moderate d, low effective extraction. Strict textualists are primary victims with identity-locked exit — their scholarly identity is constituted through textualist interpretive tradition, making exit from the constraint (accepting rationalist premises) equivalent to abandoning their intellectual framework. The engine derives high d (victim status + identity_locked exit) producing high effective extraction. Rural traditional communities are secondary victims with constrained exit — they bear costs when istihsan overrides customary practice but also benefit from legal stability and have some capacity to negotiate local accommodations. Moderate-high d, moderate effective extraction. The perspectival gap is structural: beneficiaries experience coordination (rope), victims experience extraction (snare or tangled_rope), and the analytical observer sees both functions operating simultaneously (tangled_rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanafi reading resolves mandatrophy by demonstrating that the same jurisprudential method can be both coordination mechanism and extraction structure depending on observational context. The method's founding mandate — enabling legal reasoning beyond explicit textual sources — remains live: novel circumstances continue to require analogical extension and juristic discretion. But the method's institutional dominance (especially during Ottoman period) layered extraction onto coordination: rationalist jurists gained interpretive authority not merely through reasoning quality but through state patronage and institutional gatekeeping. The constraint is not pure coordination (rope) because textualist alternatives are actively suppressed. It is not pure extraction (snare) because the coordination function is genuine and benefits multiple stakeholders. Tangled_rope classification captures both structural features: requires_active_enforcement (madhhab loyalty, institutional training), beneficiaries (rationalist jurists, commercial actors), and victims (strict textualists, rural communities). The analytical perspective's tangled_rope classification is the claimed type because it acknowledges both the method's genuine legal innovation and its asymmetric power structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qiyas_scope_boundary,
    'What distinguishes legitimate analogical extension (qiyas) from illegitimate innovation (bid''ah) in the Hanafi framework?',
    'Historical analysis of accepted vs rejected qiyas applications; identification of meta-principles governing analogy scope; comparison with sibling madhhab boundaries',
    'If boundary is principled and stable: coordination function dominates. If boundary shifts with jurist preference: extraction mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_scope_boundary, conceptual, 'Boundary between legitimate qiyas and illegitimate innovation').

omega_variable(
    istihsan_constraint_mechanism,
    'Does istihsan (juristic preference) operate as constrained discretion within textual limits, or as independent rationalist authority?',
    'Textual analysis of istihsan invocations in Hanafi corpus; identification of cases where istihsan overrides clear textual indication vs cases where it fills textual gaps',
    'If constrained: Hanafi method is coordination mechanism with low extraction. If independent: method is rationalist authority structure extracting from textualist traditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(istihsan_constraint_mechanism, empirical, 'Whether istihsan is constrained discretion or independent authority').

omega_variable(
    madhhab_kernel_ambiguity,
    'Is the usul al-fiqh kernel a single contested commitment (one kernel, multiple readings) or multiple distinct commitments (separate kernels)?',
    'Analysis of whether the four Sunni madhhabs share a common foundational text/tradition they interpret differently, or whether each madhhab grounds itself in a distinct founding commitment. Cross-madhhab recognition patterns: do jurists treat sibling madhhabs as alternative readings of shared sources, or as separate legal systems?',
    'If single kernel: this constraint is one reading among siblings (current framing). If multiple kernels: each madhhab is a separate constraint, not a reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(madhhab_kernel_ambiguity, conceptual, 'Whether usul al-fiqh is one contested kernel or multiple distinct kernels').

omega_variable(
    institutional_vs_epistemic_extraction,
    'Does the Hanafi method''s dominance in certain regions reflect epistemic superiority (better legal reasoning) or institutional path-dependence (Ottoman state adoption)?',
    'Historical analysis of Hanafi adoption patterns: correlation with state patronage vs independent scholarly consensus; comparison of legal outcomes across madhhabs for similar cases',
    'If epistemic: lower effective extraction (dominance reflects genuine coordination advantage). If institutional: higher effective extraction (dominance reflects power asymmetry, not reasoning quality).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_epistemic_extraction, empirical, 'Whether Hanafi dominance reflects epistemic or institutional factors').

omega_variable(
    committer_frame_alternative,
    'Is the Hanafi reading''s rationalist premise (reason as independent source) a foundational axiom distinguishing it from siblings, or a secondary methodological choice within a shared textualist framework?',
    'Analysis of whether Maliki, Shafi''i, and Hanbali readings reject the rationalist premise itself or merely constrain its application scope. If siblings accept reason as valid but differ on scope, the readings coexist within a shared framework. If siblings reject the premise, the Hanafi reading forecloses textualist alternatives.',
    'If foundational: reading_relations should include ''forecloses'' edges to strict textualist siblings. If secondary: all relations are ''coexists_with'' or ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_alternative, conceptual, 'Whether rationalist premise is foundational or secondary in madhhab differentiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_theater_founding, hanafi_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanafi_theater_consolidation, hanafi_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(hanafi_theater_ottoman, hanafi_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(hanafi_theater_contemporary, hanafi_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(hanafi_extract_founding, hanafi_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hanafi_extract_consolidation, hanafi_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(hanafi_extract_ottoman, hanafi_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(hanafi_extract_contemporary, hanafi_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_suppress_founding, hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hanafi_suppress_consolidation, hanafi_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(hanafi_suppress_ottoman, hanafi_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(hanafi_suppress_contemporary, hanafi_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The Hanafi reading is one of four major Sunni madhhab readings of the usul al-fiqh kernel. Each reading has its own extractiveness value reflecting its institutional history and power structure. The Hanafi reading's moderate extractiveness (0.35) reflects Ottoman-era dominance tempered by contemporary legal pluralism. Sibling readings have different extractiveness profiles based on their own institutional trajectories. Network edges represent mutual influence: each reading's institutional success or failure affects the legitimacy conditions for siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
