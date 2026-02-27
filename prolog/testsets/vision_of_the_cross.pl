% ============================================================================
% CONSTRAINT STORY: vision_of_the_cross
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vision_of_the_cross, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vision_of_the_cross
 *   human_readable: In Hoc Signo Vinces Mandate
 *   domain: religious/political
 *
 * SUMMARY:
 *   The 'In Hoc Signo Vinces' mandate represents a foundational hybrid
 *   constraint that fused religious authority, state legitimacy, and social
 *   coercion into a single institutional apparatus. Constantine's reported
 *   vision of the Christian symbol in the sky became the narrative
 *   justification for state-enforced Christian identity across the Roman
 *   empire. The constraint exhibits Tangled Rope structure at base: the
 *   mandate genuinely coordinates imperial identity and consolidates state
 *   authority (rope function), while simultaneously extracting compliance
 *   from non-Christian populations and suppressing doctrinal dissent (snare
 *   function). The theater ratio (0.85) reflects the increasing gap between
 *   the elaborated theological and ceremonial apparatus and actual functional
 *   necessity — by the 5th century, invocations of Constantine's vision are
 *   largely performative ritual maintaining institutional inertia. The
 *   extractiveness value (0.58) captures the moderate but sustained
 *   asymmetry: beneficiaries (imperial state, institutional clergy)
 *   experience genuine organizational benefit, while victims (pagans,
 *   doctrinal minorities) bear costs of restricted religious practice and
 *   forced assimilation.
 *
 * KEY AGENTS:
 *   - Constantine: Original beneficiary (institutional/arbitrage) — vision narrative provides legitimacy for political consolidation and religious centralization
 *   - Imperial State Apparatus: Primary beneficiary (institutional/arbitrage) — Christian mandate solves fragmentation problem through sacralized authority
 *   - Institutional Christian Clergy: Primary beneficiary (organized/arbitrage) — state enforcement of orthodoxy and property protection provide massive organizational consolidation
 *   - Pagan Subject Populations: Primary victim (powerless/trapped) — religious practice restrictions, property seizures, social degradation accumulate over generations
 *   - Doctrinal Minority Christians: Secondary victim (powerless/trapped) — Arian, Nestorian, and heterodox communities targeted by orthodoxy enforcement
 *   - Non-Christian Merchant Class: Secondary victim (moderate/constrained) — religious tax differentials and property restrictions constrain economic activity but some arbitrage through conversion or migration
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable religious-political law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vision_of_the_cross, 0.58).
domain_priors:suppression_score(vision_of_the_cross, 0.72).
domain_priors:theater_ratio(vision_of_the_cross, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vision_of_the_cross, extractiveness, 0.58).
narrative_ontology:constraint_metric(vision_of_the_cross, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vision_of_the_cross, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vision_of_the_cross, tangled_rope).
narrative_ontology:human_readable(vision_of_the_cross, "In Hoc Signo Vinces Mandate").
narrative_ontology:topic_domain(vision_of_the_cross, "religious/political").

domain_priors:requires_active_enforcement(vision_of_the_cross).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vision_of_the_cross, constantinopolitan_state).
narrative_ontology:constraint_beneficiary(vision_of_the_cross, christian_institutional_clergy).
narrative_ontology:constraint_victim(vision_of_the_cross, non_christian_populations).
narrative_ontology:constraint_victim(vision_of_the_cross, doctrinal_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PAGAN SUBJECT POPULATION (SNARE) — Powerless populations with no exit from the expanding Christian state apparatus. Religious practice restrictions, property seizures, and social degradation accumulate over generations. Cannot organize resistance or escape imperial jurisdiction. Experiences maximum extraction under increasingly coercive enforcement.
constraint_indexing:constraint_classification(vision_of_the_cross, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: DOCTRINAL MINORITY CHRISTIANS (SNARE) — Arian, Nestorian, and other heterodox Christian communities experience the state's Christian mandate as extractive coercion, not coordination. Trapped within Christian identity but targeted by orthodox enforcement apparatus. Suppressed alternatives and heresy restrictions create maximum extraction for theological dissenters.
constraint_indexing:constraint_classification(vision_of_the_cross, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: IMPERIAL STATE APPARATUS (ROPE) — Constantine and successor rulers see the Christian mandate as pure coordination: unifying religious identity under state authority solves the problem of social cohesion in a fragmenting empire. The vision narrative provides legitimacy and voluntary compliance through sacralization of political authority. Net beneficiary experiencing low extraction through arbitrage (can maintain or shift religious policy if political calculus changes).
constraint_indexing:constraint_classification(vision_of_the_cross, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL CHRISTIAN CLERGY (ROPE) — Orthodox clergy experience the mandate as coordination enabling institutional power consolidation. State protection of church property, enforcement of doctrinal orthodoxy, and tithing guarantees provide massive organizational benefit. Organized actors with arbitrage options (can align with or resist particular emperors) see this as coordination solving the problem of maintaining religious authority across the empire.
constraint_indexing:constraint_classification(vision_of_the_cross, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: NON-CHRISTIAN MERCHANT CLASS (TANGLED ROPE) — Constrained exit (cannot fully abandon economic participation in Christian-majority cities and trade networks) but also genuine coordination benefits from stable imperial administration and standardized law. Extraction occurs through property restrictions and religious tax differentials, but merchants retain some arbitrage through conversion, strategic religious performance, or migration to outer provinces. Mixed experience: some extraction, some genuine benefit from order.
constraint_indexing:constraint_classification(vision_of_the_cross, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: IMPERIAL RELIGIOUS PROPAGANDA SYSTEM (PITON) — The vision narrative itself becomes the degraded mechanism. By the 5th-6th centuries, the vision is invoked ceremonially and theatrically but its legitimating power has atrophied — emperors maintain the Christian mandate through habit and institutional momentum rather than genuine belief in the narrative's truth. Theater ratio (0.85) reflects that the elaborate ceremony and theological elaboration vastly exceed the actual verification or functional necessity of Constantine's original vision claim.
constraint_indexing:constraint_classification(vision_of_the_cross, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT ALERT) — From a universal analytical view, the mandate appears to be an immutable natural law: religious identification and state power are inherently entangled, and any state must sacralize its authority. However, the structural data contradicts this. The vision narrative is contingent, the enforcement is coercive, and alternative organizational forms exist. The analytical 'mountain' perspective risks naturalizing what is actually a hybrid coordination-extraction mechanism deployed by institutional beneficiaries.
constraint_indexing:constraint_classification(vision_of_the_cross, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vision_of_the_cross_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vision_of_the_cross, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vision_of_the_cross, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vision_of_the_cross, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vision_of_the_cross, TR),
    TR >= 0.70.

:- end_tests(vision_of_the_cross_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Christian mandate transfers authority over religious practice from individual choice to state control, generating clear asymmetric benefit: the imperial state and orthodox clergy gain institutional power consolidation and enforcement mechanism for compliance; non-Christian populations lose autonomy over religious practice and face property seizures/restrictions. However, extractiveness is not extreme (not 0.75+) because: (1) the coordination function is genuine — the mandate does solve a real imperial fragmentation problem; (2) voluntary conversion operates as a genuine exit option reducing effective extraction for some populations; (3) enforceability depends on continuing state investment rather than occurring passively. The measurement trajectory shows increasing extractiveness as coercive capacity matures (0.35→0.50→0.58 over 100 years), indicating that initial coordination benefits become layered with increasing extraction as enforcement apparatus hardens. Suppression (0.72): High. Severe barriers to exit include: religious identity restrictions (cannot freely practice non-Christian religions in public), property seizures targeting non-Christian institutions, legal penalties for doctrinal dissent, social degradation of apostate status. Suppression is high but not maximal (0.90+) because some populations can exit through conversion or migration to less-enforced peripheries. Theater ratio (0.85): Very high and increasing over the interval. Constantine's original vision claim is relatively straightforward — a narrative legitimating religious-political consolidation. By the 5th-6th centuries, the apparatus has evolved into elaborate theological structures (Nicene Councils, Christological disputes, liturgical hierarchy) where the performative and ceremonial content vastly exceeds the functional necessity. The vision narrative itself is invoked ceremonially but its legitimating power has atrophied — emperors maintain the Christian mandate through habit and institutional momentum.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how structural position determines classification entirely independently of the underlying constraint mechanics. The same extraction of religious autonomy is experienced as beneficial coordination by the state (who sees itself consolidating fragmented authority), as pure predation by powerless pagan populations (who have no exit), as mixed coordination and extraction by merchants (who retain some arbitrage), and as degraded ritual by the institutional apparatus itself (which maintains it through inertia). The mandatrophy is resolved not by asking 'what is the constraint really?' but by recognizing that all six types are structural truths from different positions. The false summit (analytical mountain) reveals that the constraint is contingent, not immutable — alternative imperial structures (pluralistic, non-Christian, decentralized) could have achieved stability without extractive religious coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. Institutional beneficiaries (imperial state, orthodox clergy) with arbitrage options (can shift policy, maintain or reduce religious enforcement) derive low d values (0.10-0.20), producing negative or minimal χ. They experience the constraint as coordination. Powerless trapped agents (pagan populations, doctrinal minorities) with no exit options derive high d values (0.90-0.95), producing maximum χ. They experience the constraint as pure extraction. Moderate constrained agents (merchant class) with some but limited exit (conversion, migration at cost) derive mid-range d values (0.50-0.65), producing moderate χ. They experience mixed coordination and extraction. The imperial state's ability to unilaterally set the mandate's terms and exempt itself from restrictions places it at d≈0.05 (full beneficiary); pagan populations' inability to escape religious restrictions places them at d≈0.95 (full target). The piton and false-summit mountain perspectives reflect that the constraint's functional necessity has atrophied even as its performance intensifies — this is captured in the theater ratio (0.85) indicating heavy performative content relative to actual verification or functional requirement.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY: Vision of the Cross is a decomposable phenomenon. The historical event (Constantine's vision) is separable from the institutional mandate that used it as justification. The mandate itself exhibits Tangled Rope structure (genuine coordination for state power consolidation + asymmetric extraction from non-Christian populations). The vision narrative becomes increasingly theatrical over time (theater_ratio 0.40→0.85), indicating that by the late imperial period, invocations of Constantine's vision are performative justification for institutional arrangements that no longer depend on the narrative's truth. The mandatrophy is resolved by recognizing that beneficiaries experience the constraint as coordination (rope) while victims experience it as pure extraction (snare), and this gap is not a classification error but a structural feature of the constraint's hybrid nature. The false summit perspective (mountain) attempts to naturalize contingent institutional arrangements as immutable religious-political law, but the structural data reveals that alternative coordination mechanisms were available. The vision mandate is neither pure extraction (snare) nor pure coordination (rope), but a hybrid where the coordination benefit accrues primarily to the state and clergy while the extraction costs fall primarily on non-Christian populations and doctrinal minorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constantine_vision_historicity,
    'Did Constantine actually experience a vision, or was the narrative constructed post-hoc for legitimacy?',
    'Textual analysis of Eusebius vs Constantine''s Oratio; archaeological investigation of Constantine''s documented religiosity in pre-312 period; comparison with other reported visions by imperial figures',
    'If genuine: vision is contingent psychological event that shaped policy (constraint remains contingent). If constructed: narrative is pure theater (theater_ratio moves toward 1.0, piton classification strengthens). Either way, not a natural law justifying extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constantine_vision_historicity, empirical, 'Whether Constantine''s vision was historical or constructed post-hoc').

omega_variable(
    extraction_necessity_thesis,
    'Was Christian institutional coercion necessary for imperial stability, or could non-Christian or pluralistic authority structures have achieved equivalent coordination?',
    'Comparative analysis with other empires managing religious diversity (Sassanid, Abbasid, Ottoman); counterfactual historical modeling of non-Christian centralization scenarios; examination of why pagan emperors (Maxentius, Galerius) failed in competition with Constantine',
    'If necessary: some extraction is legitimate coordination cost (tangled_rope classification holds). If contingent: extraction could have been avoided through alternative structures (snare classification strengthens for victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_necessity_thesis, conceptual, 'Whether Christian coercion was structurally necessary for imperial stability').

omega_variable(
    doctrinal_enforcement_mechanism,
    'Why did Constantine and successors enforce doctrinal orthodoxy against Arian and other heterodox Christian communities, rather than permitting internal Christian pluralism?',
    'Analysis of the Council of Nicaea and successor councils; comparison of enforcement patterns under Constantine vs later emperors; examination of whether doctrinal uniformity strengthened or weakened imperial authority',
    'If doctrinal uniformity strengthened state control: pure extraction against minorities (snare). If theological disagreement threatened imperial unity: tangled rope (coordination + extraction). Mechanism determines whether doctrinal victims are seen as targets of pure coercion or as obstacles to legitimate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_enforcement_mechanism, conceptual, 'Mechanism underlying doctrinal orthodoxy enforcement').

omega_variable(
    voluntary_compliance_vs_coercion_ratio,
    'What fraction of the Christian mandate''s effectiveness derives from genuine voluntary adoption by populations versus coercive enforcement?',
    'Comparative analysis of conversion rates in high-enforcement zones (Roman heartland) vs low-enforcement peripheries; study of apostate rates during persecution reversals; examination of whether Christian populations maintained faith in isolation from state enforcement (monastic communities, diaspora communities)',
    'If predominantly voluntary: coordination (rope) classification strengthens across populations. If predominantly coercive: extraction (snare) strengthens. Ratio determines whether extractiveness value of 0.58 is too low or too high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_vs_coercion_ratio, empirical, 'Ratio of voluntary adoption to coercive enforcement in Christian mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vision_of_the_cross, 312, 412).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(voc_tr_t0, vision_of_the_cross, theater_ratio, 0, 0.4).
narrative_ontology:measurement(voc_tr_t50, vision_of_the_cross, theater_ratio, 50, 0.65).
narrative_ontology:measurement(voc_tr_t100, vision_of_the_cross, theater_ratio, 100, 0.85).

% Extraction over time
narrative_ontology:measurement(voc_be_t0, vision_of_the_cross, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(voc_be_t50, vision_of_the_cross, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(voc_be_t100, vision_of_the_cross, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vision_of_the_cross, enforcement_mechanism).
narrative_ontology:affects_constraint(vision_of_the_cross, nicene_orthodoxy_enforcement).
narrative_ontology:affects_constraint(vision_of_the_cross, imperial_religious_property_seizure).

% DUAL FORMULATION NOTE:
% The vision narrative is separable from the mandate apparatus. The vision claim (historical event or constructed narrative) has ε≈0.15 (mountain or piton depending on authenticity). The mandate apparatus (institutional coercion + religious consolidation) has ε≈0.58 (tangled rope). Both use the vision as legitimating reference, but they are structurally distinct constraints with different extractiveness profiles. This story tracks the mandate apparatus; a separate story would track the vision claim's historical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vision_of_the_cross, institutional, 0.08).
constraint_indexing:directionality_override(vision_of_the_cross, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
