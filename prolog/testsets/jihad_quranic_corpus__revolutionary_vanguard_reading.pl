% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation (Fard 'Ayn) — Revolutionary Vanguard Reading
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   The revolutionary vanguard reading of jihad as fard 'ayn (immediate
 *   individual obligation) represents one contestable instantiation of
 *   Islamic jurisprudential texts. This reading claims that believers have a
 *   direct, unmediated duty to fight apostate rulers and occupying powers
 *   without requiring state authorization, scholarly consensus, or classical
 *   deliberative safeguards. The operative mechanism is twofold: (1) takfir
 *   doctrine, which expands the category of apostasy to include nominal
 *   Muslims who fail to support the vanguard's cause or who live under
 *   occupying powers; (2) emergency jurisprudence (darurah), which suspends
 *   classical safeguards (consultation, scholarly qualification,
 *   harm-limiting prohibitions) in the name of necessity. This reading
 *   concentrates authority in the vanguard religious figure or cell, who
 *   interprets Quranic texts to claim direct access to binding obligation.
 *   Individual believers are trapped in a binary: obey the fard 'ayn claim or
 *   face accusations of apostasy, cowardice, or complicity. Civilians in
 *   occupied territories are classified as collectively guilty through takfir
 *   doctrine, becoming legitimate targets. The classical Islamic
 *   jurisprudential tradition (madhab-based, consensus-driven, protective of
 *   innocent life) is both the beneficiary (its prestige is borrowed to
 *   legitimize the vanguard reading) and the primary victim (its authority is
 *   displaced, its safeguards are overridden, its interpretive legitimacy is
 *   claimed but not granted by institutional consensus). This constraint
 *   story instantiates ONE reading of the contested jihad kernel. Other
 *   readings (defensive-spiritual, expansionist-legalist) are separate
 *   constraints with different ε values, beneficiaries, and victim
 *   structures. The revolutionary vanguard reading produces the highest
 *   extractiveness and the most severe suppression mechanism because it
 *   maximizes the individual's binding obligation and minimizes institutional
 *   buffers.
 *
 * KEY AGENTS:
 *   - Individual Believer: Primary victim (powerless/trapped/immediate) — bound by fard 'ayn obligation that claims their life and moral agency; exit is branded as apostasy.
 *   - Vanguard Religious Authority (Islamic State, Al-Qaeda, etc.): Primary beneficiary (institutional/arbitrage) — claims control over obligation interpretation, recruits individual believers, bypasses state/ulama authority.
 *   - Civilian Populations in Occupied Territories: Victims (powerless/identity-locked/immediate) — classified as collectively guilty through takfir doctrine; structurally mobile but identity-constituted as apostate; become legitimate targets.
 *   - Classical Islamic Jurisprudential Tradition (Madhabs, Ulama): Dual role (institutional/constrained) — benefits from prestige borrowed by vanguard claims; victimized by displacement of authority and overriding of safeguards.
 *   - State-Aligned Islamic Institutions (Al-Azhar, Official Muftis): Institutional actor (institutional/arbitrage) — formally reject takfir doctrine (piton perspective); maintain theatrical counter-fatwas with limited effect on recruitment dynamics.
 *   - Occupying Powers and Apostate Rulers: Nominal targets but actual beneficiaries of suppression mechanism — the vanguard reading drives state counterinsurgency, military escalation, and civilian displacement that reinforce the occupation and authoritarian rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.68).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.78).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation (Fard 'Ayn) — Revolutionary Vanguard Reading").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'efbee3e0-a821-4ec4-898b-f15ce208ec73').
narrative_ontology:cs_kernel_codification('efbee3e0-a821-4ec4-898b-f15ce208ec73', fixed_text).
narrative_ontology:cs_authority_grounding('efbee3e0-a821-4ec4-898b-f15ce208ec73', lineage).
narrative_ontology:cs_interpretation_layer_present('efbee3e0-a821-4ec4-898b-f15ce208ec73').
narrative_ontology:cs_reading_relation('efbee3e0-a821-4ec4-898b-f15ce208ec73', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('efbee3e0-a821-4ec4-898b-f15ce208ec73', jihad_quranic_corpus__expansionist_legalist_reading, influences).
narrative_ontology:cs_axiom('efbee3e0-a821-4ec4-898b-f15ce208ec73', foundational, takfir_without_deliberation).
narrative_ontology:cs_axiom_status(takfir_without_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('efbee3e0-a821-4ec4-898b-f15ce208ec73', takfir_without_deliberation, deontological).
narrative_ontology:cs_axiom('efbee3e0-a821-4ec4-898b-f15ce208ec73', foundational, emergency_override_boundless).
narrative_ontology:cs_axiom_status(emergency_override_boundless, holdable).
narrative_ontology:cs_axiom_grounding('efbee3e0-a821-4ec4-898b-f15ce208ec73', emergency_override_boundless, empirically_contingent).
narrative_ontology:cs_reference_frame('efbee3e0-a821-4ec4-898b-f15ce208ec73', classical_madhab_jihad_doctrine).
narrative_ontology:cs_drift_state('efbee3e0-a821-4ec4-898b-f15ce208ec73', contemporary_vanguard_emergentism, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('efbee3e0-a821-4ec4-898b-f15ce208ec73', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_religious_authority).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_powers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_within_occupied_territories).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jurisprudential_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL BELIEVER (SNARE) — The fard 'ayn obligation binds each believer individually and immediately. Exit from the obligation is branded as apostasy, kufr, or grave sin. No state mediation, no deliberative assembly, no escape clause. The individual is trapped between the internal duty claim and external suppression (state counterinsurgency, social ostracism, family rupture). Maximum extraction — the vanguard authority claims the believer's life, body, and moral agency through sacred obligation.
constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN POPULATION (IDENTITY-LOCKED SNARE) — Structurally mobile (could relocate, hide, comply) but identity-constituted through collective guilt: takfir doctrine renders civilians legitimate targets by declaring their society apostate. Exit would require abandoning religious/communal identity. The constraint's suppression mechanism is both structural (military occupation, surveillance, economic blockade) and internalized (collective responsibility doctrine that makes civilians feel they are guilty of apostasy through collective passivity). High extraction — civilians are both targets and implicated in the vanguard's claims.
constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: CLASSICAL JURISPRUDENTIAL ULAMA (TANGLED ROPE) — The revolutionary vanguard reading bypasses classical safeguards (shura, ijma', madhab-based consensus) that distributed authority across qualified scholars. The ulama experience both coordination loss (their interpretive authority is overridden) and extraction (their institutional legitimacy is used to legitimize vanguard claims while their actual verdicts are ignored). Some scholars benefit from association with the cause; others are constrained by either signing on or facing charges of complicity. Constrained exit — reaffirm classical jurisprudence and lose influence; support vanguard claims and sacrifice interpretive integrity.
constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: VANGUARD RELIGIOUS AUTHORITY (ROPE) — Experiences the fard 'ayn doctrine as a coordination mechanism: decentralized authority allows cells to act without central command, solving the organizational problem of distributed mobilization. The vanguard extracts recruits, legitimacy, and operational autonomy. Emergency jurisprudence (darurah) provides exit from classical constraints. Arbitrage options: can disavow connections, redefine doctrine, evolve interpretations. Net beneficiary — the constraint amplifies vanguard power by making individual believers direct agents of the cause.
constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE-ALIGNED ISLAMIC INSTITUTIONS (PITON) — Formal Islamic authority structures (Al-Azhar, state muftis, majority madhabs) have formally rejected takfir doctrine and the fard 'ayn reading as heretical (bid'ah) and contrary to consensus. Yet these institutions continue to perform legitimacy rituals (issuing counter-fatwas, delivering sermons against extremism) that are largely theatrical — they do not prevent recruitment, do not rebuild damaged institutional authority, and do not offer viable alternatives to individuals trapped in the vanguard's binary framing. The piton classification derives from theater_ratio (0.55 for the majority response) and the institutional inertia of formal Islamic structures that cannot effectively counter the vanguard's claim to revolutionary purity.
constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED EMERGENCY (MOUNTAIN) — From a civilizational analytical perspective, the fard 'ayn obligation emerges as an irrevocable feature of Islamic law: emergency (darurah) suspends normal jurisprudential constraints, rendering the binding of individual believers as immediate and immutable as a law of nature. The observer who naturalizes emergency doctrine sees the constraint as emerging necessarily from Quranic text and theological premises. However, the structural data contradicts this mountain classification — the vanguard reading depends on contentious selections from scriptural corpus, abandonment of classical safeguards, and institutional innovation disguised as recovery. The engine will compute this as a false summit, revealing that 'natural' Islamic law is actually a contested reading.
constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jihad_quranic_corpus__revolutionary_vanguard_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, TR),
    TR >= 0.70.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The fard 'ayn obligation claims each believer's immediate compliance through a binding religious duty. The vanguard authority extracts recruitment, operational capacity, moral legitimacy, and ideological commitment. Extractiveness rises over the interval (0.45→0.68) as takfir doctrine expands to include progressively broader populations (nominal Muslims → civilians → diaspora communities), and as emergency doctrine becomes normalized within the vanguard's operational framework. By t=30, the extracted obligation covers not just active combatants but passive supporters and populations unable to resist. Suppression (0.78): Very high. Multiple suppressive mechanisms operate in parallel: (1) structural — state counterinsurgency, military occupation, economic blockade; (2) juridical — takfir doctrine renders dissent apostasy, eliminating escape through reinterpretation; (3) internalized — collective guilt doctrine and identity-locked status make civilians feel implicated in the cause regardless of their actual participation. Suppression requirement increases over the interval (0.65→0.78) as the vanguard must deploy escalating coercion to maintain recruitment and prevent defection. The constraint has no institutional floor — suppression can increase indefinitely as the vanguard consolidates control. Theater ratio (0.55): Moderate. The vanguard reading performs substantial religious-juridical theater: textual appeals to Quranic authority, elaborate takfir argumentation, invocations of emergency doctrine. However, the constraint's primary mechanism (direct obligation binding on believers) is functionally transparent — individuals experience the obligation as binding regardless of the theological performance. Theater increases over the interval (0.38→0.55) as the vanguard must justify increasingly broad takfir verdicts and justify civilian casualties through scriptural exegesis. The theater masks but does not eliminate the underlying extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap. The individual believer sees an immediate, binding, inescapable obligation (snare from trapped perspective). The vanguard sees a coordination solution enabling decentralized mobilization (rope from institutional/arbitrage perspective). Classical scholars see loss of authority and interpretive integrity (tangled rope from constrained perspective). The state-aligned institutions see a defeated and inert ritualism (piton from theatrical institutional perspective). The analytical observer risks naturalizing the vanguard reading as necessary Islamic law (false-summit mountain from civilizational/analytical perspective). The gap reveals that perspectival classification captures structural reality: different agents experience the same Quranic text and jurisprudential tradition as supporting radically different constraints. The vanguard's claim to represent 'true Islam' is itself a false-summit argument — naturalizing one contestable reading as the inevitable implication of scripture.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the fard 'ayn obligation. Individual believers face the constraint as binding duty with no exit cost in the sense of classical contract-breaking — the cost is the burden of obedience itself, which is presented as redemptive. However, exit from the vanguard's interpretation (e.g., by adopting the defensive-spiritual reading) is branded as apostasy, raising d toward 1.0. Powerless/trapped individuals experience d ≈ 0.92 (near-total extraction). Civilians in occupied territories experience d ≈ 0.88 (high extraction through collective guilt, with identity-lock preventing reframing as innocence). Classical scholars experience d ≈ 0.50 (symmetric extraction and benefit — they are used to legitimize the vanguard but lose institutional authority). The vanguard beneficiary experiences d ≈ 0.15 (arbitrage exit and beneficiary status lower d substantially). State-aligned institutions experience d ≈ 0.30 (they maintain formal authority but lack effective exit from the vanguard's challenge to their legitimacy). The analytical observer at the civilizational context experiences d ≈ 0.72 (analytical position observes the constraint without being bound by it, but observes structural extraction dynamics that suggest high chi). The per-perspective chi values follow from the tuple (P,T,E,S), the d value, and the scope modifier σ(S). Powerless perspectives with trapped/identity-locked exit at immediate/regional scope produce χ ≈ 0.84–0.92 (snare classification). Institutional perspectives with arbitrage exit produce χ ≈ 0.20–0.35 (rope classification). Organized perspectives with constrained exit produce χ ≈ 0.50–0.65 (tangled rope classification).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through explicit omega variables documenting the conceptual and definitional choices that determine classification. Takfir scope (omega_1) directly determines victim set and extractiveness. Emergency doctrine scope (omega_2) determines suppression level and whether classical safeguards persist. Decentralized authority coherence (omega_3) determines whether suppression is uniform or fragmented. Quranic polysemy (omega_4) determines whether the reading is forced or chosen. Collective guilt mechanism (omega_5) determines civilian targeting and suppression depth. The false-summit ambiguity (omega_6) is the highest-order omega: whether the constraint is a natural law of Islamic jurisprudence or a contestable reading that claims natural-law status to suppress alternatives. The mandatrophy resolution lies in recognizing that each perspective's classification is correct FROM THAT PERSPECTIVE — the snare is real to the trapped believer, the rope is real to the vanguard beneficiary, the piton is real to the failed state institutions. The constraint cannot be resolved to a single type; it can only be transparently decomposed into its reading-dependent components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takfir_definitional_boundary,
    'What acts, beliefs, or states constitute apostasy (kufr) sufficient to trigger fard ''ayn obligation to fight?',
    'Systematic comparison of classical madhab definitions of apostasy (Hanafi, Maliki, Shafi''i, Hanbali) vs. vanguard expansive takfir; analysis of Quranic precedent for collective vs. individual guilt; historical examples of takfir rulings and their acceptance by contemporary Islamic scholarly consensus.',
    'If takfir scope is narrow (classical, individual-act-based): constraint affects only formally apostate rulers, not populations or occupiers. If takfir scope is broad (vanguard, identity-based): constraint expands victim set to include civilians, occupying populations, and those who refuse the vanguard reading itself. Directly determines extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(takfir_definitional_boundary, conceptual, 'Definitional boundaries for apostasy triggering fard ''ayn').

omega_variable(
    emergency_doctrine_scope,
    'Does darurah (necessity/emergency) override all classical jurisprudential safeguards, or are there immutable limits (e.g., harm to innocents, violation of Quranic explicit prohibitions)?',
    'Classical jurisprudential texts on darurah and exceptions (Quranic prohibitions on killing innocents, breaking oaths, harming family); comparative analysis with vanguard emergency doctrines; examination of whether vanguard texts claim unlimited or bounded scope for emergency suspension.',
    'If darurah has immutable limits: constraint is constrained by classical law and suppression is lower (classical limits prevent wholesale civilian targeting). If darurah is unlimited: constraint permits systematic civilian harm, extraction increases, and suppression mechanisms are unconstrained. Determines suppression value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_doctrine_scope, conceptual, 'Scope of emergency doctrine overrides on classical jurisprudential constraints').

omega_variable(
    decentralized_authority_legitimacy,
    'Can fard ''ayn obligation (binding on each believer individually) exist coherently alongside centralized jurisprudential authority? Or does individual obligation necessitate local interpretation?',
    'Analysis of vanguard texts claiming individual obligation bypasses consultation (shura) and group deliberation; examination of whether decentralized cells produce divergent takfir verdicts; comparison with classical madhab requirements for qualified mufti interpreting obligation.',
    'If individual obligation is compatible with centralized authority: the constraint is more tightly bound and suppression is more uniform (central authority can enforce consistency). If individual obligation necessitates decentralized interpretation: authority fractures, suppression becomes uneven, and the constraint''s extractiveness depends on which cell''s interpretation claims legitimacy. Determines whether this is a unitary snare or a network of competing snares.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralized_authority_legitimacy, conceptual, 'Coherence of individual fard ''ayn obligation with centralized vs. decentralized authority').

omega_variable(
    quranic_corpus_polysemy,
    'Can the Quranic corpus be read as supporting BOTH the defensive-spiritual reading and the revolutionary-vanguard reading, or are the two readings mutually exclusive in their scriptural foundation?',
    'Verse-by-verse analysis of Quranic jihad passages (Surah 2:190-194, 22:39-40, 4:74-77, 9:29, etc.) for compatibility with both readings; examination of whether exegetical (tafsir) traditions have accommodated both; analysis of what hermeneutical rules permit or foreclose multiple readings.',
    'If readings coexist in corpus: constraint is a reading choice, not a necessary interpretation, and the revolutionary vanguard reading is one legitimate option among others. If readings foreclose each other: the constraint is a hermeneutical claim about which reading correctly instantiates Quranic intent. Determines the `forecloses` vs `coexists_with` relationship between this reading and the defensive-spiritual sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_corpus_polysemy, conceptual, 'Whether Quranic corpus supports multiple jihad readings or forecloses alternatives').

omega_variable(
    collective_guilt_mechanism,
    'Does takfir doctrine permit assignment of collective guilt to civilian populations, or does Islamic law require individual determination of culpability?',
    'Examination of vanguard texts justifying civilian targeting through collective identity (nation-state, occupation authority, religious majority); comparison with classical prohibitions on harming innocents (Quranic 17:15); analysis of whether collective guilt is a necessary implication of vanguard emergency doctrine or an additional choice made by specific operatives.',
    'If collective guilt is logically entailed: civilians become legitimate targets by identity alone, suppression is total, and extractiveness is maximized (all subjects trapped). If collective guilt is a separate choice: the constraint targets only identified apostate rulers and occupiers, and civilians may exit through neutrality or explicit dissent. Determines scope of victim set and suppression level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_guilt_mechanism, conceptual, 'Whether collective guilt doctrine is necessarily entailed or separately chosen').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is the fard ''ayn revolutionary vanguard reading a necessary interpretation of Islamic jurisprudence rooted in immutable scriptural principles, or is it a contestable institutional innovation that claims natural-law status to suppress alternatives?',
    'Historical analysis of when takfir and emergency doctrine expansions emerged (8th-20th centuries), whether they appear in classical madhab consensus, whether current Islamic scholarly institutions (Al-Azhar, majority ulama, official muftis) recognize them as legitimate readings or departures. Test whether constraint meets mountain metric gates independent of the reading chosen.',
    'If natural law: constraint cannot be changed without changing Islam itself. If contestable reading: constraint is subject to jurisprudential debate, and alternatives remain live. Determines whether the mountain perspective should classify as false summit (naturalization of contingent reading) or genuine mountain (immutable feature). High confidence that the engine''s false summit detector will flag this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, conceptual, 'Whether fard ''ayn reading is natural law or contestable innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_rv_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(jihad_rv_tr_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(jihad_rv_tr_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(jihad_rv_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jihad_rv_be_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(jihad_rv_be_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jihad_rv_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(jihad_rv_su_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(jihad_rv_su_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_doctrine_scope).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, state_monopoly_violence).

% DUAL FORMULATION NOTE:
% The jihad kernel generates three structurally distinct constraints corresponding to the three major readings. The revolutionary vanguard reading (this story) has ε=0.68 (high extraction, snare primary). The defensive-spiritual reading has lower ε (constraint as legitimate self-defense coordination, rope primary) and stricter suppression limits (classical madhab safeguards remain binding). The expansionist-legalist reading sits between them. These are not perspectives on a single constraint; they are distinct constraints generated by selecting different scriptural interpretations. The vanguard reading is downstream of (and influences) both siblings because it claims to restore classical doctrine while actually exceeding it, creating a false-summit dynamic where it appears to be the most legitimate reading while actually being the most innovative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
