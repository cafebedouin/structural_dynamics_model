% ============================================================================
% CONSTRAINT STORY: pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatic_incoherence_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pragmatic_incoherence_reading
 *   human_readable: Pragmatic Incoherence in Simultaneous Veneration (Kami-Hotoke Syncretism)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   During the Edo period (1603–1868), simultaneous veneration of kami
 *   (Shinto deities) and hotoke (Buddhist entities) was normative practice in
 *   Japan: the same shrine housed both, practitioners made offerings to both,
 *   priests conducted both Shinto and Buddhist rituals, and theological
 *   systems were never synthesized into coherence. This constraint interprets
 *   that historical phenomenon as pragmatic incoherence — practitioners held
 *   contradictory beliefs simultaneously without pressure to resolve them.
 *   The 'incoherence' was not hidden; it was visible in daily practice. What
 *   was suppressed was the articulation of the contradiction: practitioners
 *   acted on both belief frames without ever naming them as incompatible.
 *   This reading argues that the constraint was sustained by institutional
 *   neglect of enforcement pressure (Edo authorities had neither capacity nor
 *   incentive to force doctrinal coherence), by priestly benefits from
 *   maintaining dual authority structures, and by practitioners'
 *   identity-fusion with syncretic practice itself. The Meiji shinbutsu-bunri
 *   (separation of kami and hotoke, 1868) revealed the latent incoherence by
 *   forcibly resolving it: the constraint's extractiveness collapsed
 *   immediately when enforcement pressure increased. This reading contrasts
 *   with alternative interpretations (the sibling readings) that argue either
 *   for ontological fusion (kami and hotoke were coherently fused in
 *   practitioners' cosmology) or domain partition (kami and hotoke operated
 *   in naturally separate domains, making coherence implicit rather than
 *   suppressed).
 *
 * KEY AGENTS:
 *   - Practitioners (Village communities): Primary victims (powerless/identity_locked) — held contradictory beliefs simultaneously; cognitive freedom suppressed through unarticulation norm; identity fused with syncretic practice
 *   - Priestly Elites (Shinto and Buddhist priests): Primary beneficiaries (institutional/arbitrage) — maintained dual authority structures, derived income from parallel ritual systems, experienced syncretism as pure coordination
 *   - Political Authorities (Tokugawa shogunate, imperial court): Secondary beneficiaries (powerful/constrained) — drew legitimacy from both Shinto and Buddhist institutional frameworks; constrained by necessity to enforce neither doctrine consistently
 *   - Theological Coherence (Abstract collective good): Primary victim (powerless/trapped) — unresolved contradictions prevented theological development; systematic unarticulation prevented canonical synthesis
 *   - Meiji Reformers (State modernizers): Organized agents (organized/constrained) — imposed shinbutsu-bunri as institutional rupture; perceived prior syncretism as temporary dysfunction requiring resolution
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes pragmatic incoherence as coordination mechanism with extractive suppression costs; distinguishes from sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatic_incoherence_reading, 0.58).
domain_priors:suppression_score(pragmatic_incoherence_reading, 0.72).
domain_priors:theater_ratio(pragmatic_incoherence_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pragmatic_incoherence_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(pragmatic_incoherence_reading, "Pragmatic Incoherence in Simultaneous Veneration (Kami-Hotoke Syncretism)").
narrative_ontology:topic_domain(pragmatic_incoherence_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(pragmatic_incoherence_reading, distributed).
narrative_ontology:cs_authority_grounding(pragmatic_incoherence_reading, practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, priestly_elites).
narrative_ontology:constraint_beneficiary(pragmatic_incoherence_reading, political_authorities).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, theological_coherence).
narrative_ontology:constraint_victim(pragmatic_incoherence_reading, practitioners_cognitive_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTITIONER (SNARE) — Holds contradictory beliefs simultaneously (kami as Shinto essence; hotoke as Buddhist entity; both present in same shrine) without resolution capacity. Structurally mobile but identity-locked: religious identity fused with simultaneous veneration practice. Cannot articulate or resolve contradiction without threatening identity. Experiences maximum suppression through absence of permission to notice incoherence.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRIESTLY ELITES (ROPE) — Benefit from coordinating both Shinto and Buddhist functions without doctrinal synthesis. Syncretism enables priests to maintain dual authority structures, derive income from parallel ritual systems, and avoid choice between Buddhist institutional power and Shinto local legitimacy. Experiences the constraint as pure coordination: the unresolved incoherence is the mechanism that enables both systems to operate simultaneously. Net beneficiary with high arbitrage (can switch between frameworks contextually).
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL AUTHORITY (TANGLED ROPE) — Benefits from syncretism (draws legitimacy from both Shinto imperial descent and Buddhist universalism) while remaining constrained by necessity to enforce neither doctrine consistently. Genuine coordination function: simultaneous veneration enables integration of regional Shinto practices with centralized Buddhist institutional framework. But asymmetric extraction: constraints practitioners' cognitive clarity to maintain political flexibility. Constrained by risk of choosing either doctrine — schism would fragment authority.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: VILLAGE COMMUNITY (SNARE) — Trapped in perpetual oscillation between kami-focused and hotoke-focused practice with no institutional pressure to resolve or even acknowledge contradiction. Local communities lack exit: cannot adopt single coherent theology without splitting from both institutional systems. Suppression through normalization of contradiction-as-practice. Maximum extraction: cognitive labor of maintaining incoherence without framework for articulation.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: MEIJI REFORMERS (SCAFFOLD) — Organized state actors implementing shinbutsu-bunri (separation of kami and hotoke) as deliberate institutional rupture. Perceive simultaneous veneration as a temporary, dysfunctional state requiring resolution via policy enforcement. The constraint has a sunset built into it: once separation is imposed, the ambiguity dissolves. This perspective views the prior pragmatic incoherence as a scaffold phase — unsustainable without enforcement, intentionally dismantled by political authority.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes that simultaneous veneration was never coherent as a theological system but was highly functional as a social coordination mechanism. The 'incoherence' was not hidden (practitioners acted on both belief frames daily) but was systematically unarticulated. Genuine coordination function: syncretism integrated local and imperial legitimacy, regional and national authority, without forcing choice. Extraction mechanism: sustained practitioners in cognitive state that prevented unified resistance or alternative institutional organization. Meiji separation reveals the constraint's latent extractiveness.
constraint_indexing:constraint_classification(pragmatic_incoherence_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatic_incoherence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pragmatic_incoherence_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reading's empirical signature is the trajectory from early-Edo tolerance (ε~0.35, incoherence normalized but not yet entrenched) through mid-Edo accumulation (ε~0.48, practitioners more identity-locked as generations inherited the practice) to late-Edo peak (ε~0.58, maximum suppression of articulation as institutional norms hardened). The sharp collapse after Meiji separation (ε~0.12 by 1868) is diagnostic for this reading: if incoherence were natural law or implicit domain partition, enforcement pressure should have minimal effect. The dramatic drop suggests suppression was institutional, not structural or cognitive. Suppression (0.72): Very high. The primary suppression mechanism is the institutional absence of pressure to articulate contradiction — simultaneous veneration was 'just how things were' without doctrinal pressure from political authorities or enforced choice between kami and hotoke veneration. Practitioners were suppressed not by explicit prohibition on asking uncomfortable questions but by normalization of unexamined coexistence. This manifests as identity-lock: practitioners' religious identity was constituted through the practice of holding both belief frames without resolution. Theater ratio (0.68): High. The performative content of simultaneous veneration increased over the Edo period as the practice became more elaborate and less directly functional. Early-Edo syncretism may have reflected genuine theological exploration or pragmatic accommodation; by late-Edo, much ritual activity was ceremonial maintenance of the dual system for its own sake, with decreasing emphasis on either kami or hotoke theology. Post-Meiji separation shows a dramatic drop in theater (0.18), indicating that when enforcement pressure increased, performative maintenance collapsed rapidly, revealing that much of the prior system was theater rather than functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is maximal. Priestly elites experience syncretic incoherence as coordination (Rope): the mechanism enables them to maintain parallel authority structures and derive income from both systems without choosing. Practitioners experience it as extraction and suppression (Snare): their cognitive coherence is suppressed, their identity-fusion prevents exit, and they bear the cost of maintaining incoherence. Political authorities experience it as functional hybrid (Tangled Rope): syncretism provides coordination benefit (dual legitimacy, integration of regional and centralized authority) but with extraction cost (must constrain both doctrines to avoid schism). Meiji reformers experience it as a temporary problem with a sunset (Scaffold): the Edo arrangement is unsustainable without enforcement; separation is the resolution. The analytical observer at civilizational scope (Tangled Rope) recognizes that the prior system was functional as coordination precisely because it suppressed articulation of incoherence — the mechanism breaks when forced to coherence (Meiji), suggesting it was pragmatic suppression, not implicit coherence.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from structural position and exit options. Practitioners are identity_locked (structurally mobile but identity-fused; cannot exercise exit without identity rupture) — the engine derives high d (0.89) from victim status + identity_locked exit, producing high f(d) (~1.28). They experience maximum extraction because the suppression is internalized: the constraint doesn't prevent exit through external barriers but through cognitive framing that makes exit unthinkable. Priestly elites are beneficiaries with arbitrage options (institutional/arbitrage) — low d (~0.15), negative f(d) (~-0.01). They experience the constraint as pure benefit: syncretism IS their institutional function. Political authorities are beneficiaries with constrained exit (powerful/constrained) — moderate d (~0.48), f(d) ~0.60. They benefit from dual legitimacy but cannot choose either doctrine without institutional schism. Theological coherence as victim gets trapped classification (powerless/trapped) — d ~0.95, f(d) ~1.42. The analytical observer (analytical/analytical) has canonical d ~0.72, f(d) ~1.15. The Meiji reformers as organized agent (organized/constrained) derive d ~0.40, f(d) ~0.40 — they see the constraint as a problem to solve, not as extraction mechanism, because their power is concentrated and their intent is coherence enforcement, not benefit extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that tangled_rope classification is correct: simultaneous veneration had a genuine coordination function (enabling both Shinto and Buddhist institutions to coexist without imperial choice) AND asymmetric extraction (suppressing practitioners' cognitive freedom and theological development). The constraint is not 'really' pure coordination (Rope) or pure extraction (Snare); it is the hybrid. The measurement trajectory (extractiveness rising from 0.35 to 0.58 over 268 years) and the Meiji rupture signature (collapse to 0.12 when enforcement pressure increased) show that the hybrid structure was maintained by institutional neglect, not by genuine coherence-fusion. If incoherence had been coherently synthesized (sibling reading: ontological_fusion), enforcement pressure should not have caused rapid dissolution. If domains had naturally partitioned (sibling reading: domain_partition), institutional separation should have been gradual and low-cost. Instead, Meiji separation was violently rapid and high-cost — showing that the prior system relied on suppressed contradiction, not synthesis or partition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_coherence_framing,
    'Was pragmatic incoherence the actual phenomenology of simultaneous veneration, or was there an implicit coherence frame (domain partition, ontological fusion, or hierarchical nesting) that made the practice coherent to practitioners, which modern observers misread as incoherence?',
    'Textual analysis of Edo-period theological writings, practitioner accounts, and ritual manuals. Evidence: (a) explicit theological synthesis attempts (suggests coherence seeking); (b) no record of cognitive distress despite contradiction (suggests either coherence or suppression); (c) Meiji reformers'' characterizations of prior syncretism (was it described as confused or as elegant coordination?). Cross-cultural comparison with other syncretic systems (e.g., Brazilian Candomblé, Haitian Vodou) to test whether simultaneous veneration is structurally resolvable under alternative framings.',
    'If coherence frame is discovered: reclassify to domain_partition_reading or ontological_fusion_reading constraint (different files, different ε). If incoherence is confirmed: tangled_rope classification holds; extraction derives from enforced unarticulation. If evidence is ambiguous: incoherence itself becomes the extractive mechanism — practitioners held both frames while being unable to articulate either, maximizing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_coherence_framing, empirical, 'Whether simultaneous veneration had implicit coherence or genuine incoherence').

omega_variable(
    suppression_mechanism_enforcement,
    'Was the absence of enforcement pressure a necessary condition for sustaining incoherence, or was incoherence actively suppressed through social norms, taboo on theological questioning, or institutional incentives to not articulate contradiction?',
    'Historical records of temple-shrine disputes, doctrinal debates, and censorship; comparison of theological output before vs. during periods of enforcement pressure. If enforcement absence was merely permissive (incoherence persisted regardless of pressure level), suppression mechanism is endogenous (social norm, identity lock). If incoherence rapidly resolved when enforcement pressure increased (as in Meiji), suppression was structural — kept in place by institutional neglect.',
    'If endogenous suppression: identity_locked exit classification is correct; the binding is cognitive/identity-based. If structural suppression: trapped classification may be more accurate; the binding is institutional absence. This affects whether practitioners are understood as cognitively captured or structurally trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_enforcement, empirical, 'Mechanism of suppression: institutional neglect vs. active social norm enforcement').

omega_variable(
    meiji_separation_as_revelation_vs_rupture,
    'Was shinbutsu-bunri (Meiji separation) a revelation of latent incoherence in the prior system, or a rupture that created coherence by forcibly resolving what had been a genuinely functional (if incoherent) coordination mechanism?',
    'Comparison of pre-Meiji and post-Meiji practitioner accounts, theological coherence, and institutional stability. If separation revealed hidden incoherence: practitioners would have experiences of relief or clarity post-separation, theological arguments would have been ''clarified'' not ''created,'' institutional conflict would have decreased. If separation created coherence by imposing choice: practitioners would report loss of flexibility, theological synthesis would have required post-Meiji innovation, institutional conflict would have initially increased then resolved.',
    'If revelation: the constraint''s extractiveness derives from suppressed incoherence being maintained; Meiji forced articulation. If rupture: the constraint''s extractiveness derives from imposed choice; Meiji created clarity at cost of institutional complexity. This determines whether ε should decrease (latent structure revealed) or increase (coordination mechanism destroyed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_revelation_vs_rupture, empirical, 'Whether Meiji separation revealed or created coherence').

omega_variable(
    identity_lock_specificity,
    'Which component of simultaneous veneration was identity-locked for practitioners? Was it: (a) the practice itself (simultaneous veneration as identity-constituting ritual); (b) the unresolved theological state (holding contradiction as identity); (c) the refusal or inability to articulate the contradiction (identity threatened by coherence-seeking)?',
    'Practitioner interviews (if historical records exist), analysis of who experienced separation as loss vs. clarification, examination of resistance to Meiji policy by different practitioner groups. If practice itself was identity-locked: practitioners post-separation would have maintained syncretism despite policy. If theological incoherence was identity-locked: practitioners would have experienced separation as violation of coherence they had unconsciously constructed. If unarticulation was identity-locked: practitioners would have experienced coherence-seeking as threatening.',
    'Determines whether identity_locked exit classification correctly captures the binding mechanism, or whether a different exit option (constrained, trapped, or mobile with high cost) is more accurate. This is diagnostic for the oracle gap: the practitioners'' own framework prevented them from seeing what institutional separation revealed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_specificity, empirical, 'Specific locus of identity-lock in simultaneous veneration').

omega_variable(
    pragmatic_incoherence_vs_sibling_readings,
    'Is this constraint (pragmatic incoherence reading) the correct interpretation of simultaneous veneration, or should another reading (ontological_fusion_reading or domain_partition_reading) be considered as the primary account with this as a secondary perspective?',
    'Textual evidence from Edo-period theology, practitioner testimony, Meiji reformers'' rhetoric, and post-separation institutional outcomes. But empirical evidence will be interpreted differently depending on which reading is adopted — this omega documents the frame-dependency.',
    'If pragmatic incoherence reading is primary: constraint is tangled_rope from analytical perspective; suppression is high because unarticulation is sustained. If ontological fusion is primary: constraint is rope or tangled_rope but with lower ε and different suppression profile. If domain partition is primary: constraint is rope or scaffold; suppression is low because coherence is structural. The choice of reading affects the entire classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pragmatic_incoherence_vs_sibling_readings, conceptual, 'Kernel reading selection: pragmatic incoherence vs. ontological fusion vs. domain partition').

omega_variable(
    false_summit_natural_law_candidate,
    'Does the analytical perspective''s classification as tangled_rope hide an alternative reading that treats simultaneous veneration as a natural law of syncretic religion — an immutable feature of how religious systems integrate rather than a contingent institutional constraint?',
    'Cross-cultural evidence: do all syncretic religious systems exhibit simultaneous veneration of incoherent deities, or only some? If universal: candidate mountain (natural law of syncretism). If contingent on institutional factors (lack of enforcement, doctrinal tolerance, political utility): tangled_rope (institutional constraint, not natural law). Historical evidence: did Japanese syncretism persist because it was structurally inevitable, or because Edo-period authorities lacked capacity or will to enforce separation?',
    'If mountain: reclassify analytical perspective to mountain; remove beneficiaries declaration (natural laws have no beneficiaries). If tangled_rope confirmed: beneficiaries (priestly elites, political authorities) correctly identified as sustaining incoherence through enforcement neglect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, empirical, 'Whether pragmatic incoherence is natural law or contingent institutional constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatic_incoherence_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_tr_t1600, pragmatic_incoherence_reading, theater_ratio, 1600, 0.52).
narrative_ontology:measurement(prag_tr_t1700, pragmatic_incoherence_reading, theater_ratio, 1700, 0.62).
narrative_ontology:measurement(prag_tr_t1800, pragmatic_incoherence_reading, theater_ratio, 1800, 0.68).
narrative_ontology:measurement(prag_tr_t1868, pragmatic_incoherence_reading, theater_ratio, 1868, 0.18).

% Extraction over time
narrative_ontology:measurement(prag_be_t1600, pragmatic_incoherence_reading, base_extractiveness, 1600, 0.35).
narrative_ontology:measurement(prag_be_t1700, pragmatic_incoherence_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement(prag_be_t1800, pragmatic_incoherence_reading, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(prag_be_t1868, pragmatic_incoherence_reading, base_extractiveness, 1868, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pragmatic_incoherence_reading, attachment_coordination).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(pragmatic_incoherence_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% Simultaneous veneration is one kernel with three structurally distinct readings. Each reading instantiates a different constraint: pragmatic_incoherence_reading (ε=0.58, suppression via unarticulation), ontological_fusion_reading (ε~0.35, coherence implicit in unified cosmology), domain_partition_reading (ε~0.30, coherence in natural domain separation). The three constraints are NOT alternatives or approximations — they are structurally incompatible claims about the same historical phenomenon, with different ε values, different beneficiary/victim structures, and different classification profiles. They are linked via network.affects_constraints to document the constraint family: choosing one reading does not eliminate the others as conceptual possibilities, but generates different empirical predictions about how practitioners experienced coherence/incoherence and how rapidly Meiji separation could dissolve the prior system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pragmatic_incoherence_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
