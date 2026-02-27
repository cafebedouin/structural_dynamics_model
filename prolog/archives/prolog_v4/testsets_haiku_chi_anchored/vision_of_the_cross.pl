% ============================================================================
% CONSTRAINT STORY: vision_of_the_cross
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The vision of the cross as reported by Eusebius of Caesarea represents a
 *   foundational legitimacy myth in Western religious and political history.
 *   Constantine, confronted with civil war fragmentation and military
 *   uncertainty before the Battle of the Milvian Bridge (312 CE), allegedly
 *   received a divine vision promising victory through the sign of the cross.
 *   Whether authentic experience, strategic narrative, or retrospective
 *   fabrication, this constraint functions as a coordination mechanism
 *   unifying Christian theological factions and the Roman imperial military
 *   under a single religious framework. Simultaneously, it operates as an
 *   extraction mechanism suppressing pagan worship, eliminating competing
 *   Christian doctrines, and centralizing religious authority under imperial
 *   control. The constraint exhibits high theater — the vision testimony
 *   itself has no historical corroboration and relies entirely on
 *   institutional repetition (Eusebius's account is our only source). Over
 *   the 50-year interval from Constantine through his successors (312-362
 *   CE), the vision mandate shifted from persuasive incentive to coercive
 *   enforcement, with suppression and extractiveness increasing
 *   monotonically. Theater increased as the original claim's falsehood
 *   accumulated and the constraint came to rest on pure institutional
 *   authority rather than persuasive power.
 *
 * KEY AGENTS:
 *   - Constantine I: Primary beneficiary (institutional/arbitrage) — captures military unification, religious legitimacy, succession of power through Christian institutional backing
 *   - Christian Clergy (Nicene Orthodox faction): Primary beneficiary (organized/constrained) — secure institutional consolidation, state patronage, doctrinal victory over rivals, organizational hierarchy
 *   - Pagan Religious Institutions: Primary victim (powerless/trapped) — forced suppression of worship, loss of civic authority, confiscation of temples
 *   - Arian and Heterodox Christian Factions: Secondary victim (powerful/mobile) — doctrinal suppression despite significant theological sophistication; some factions can migrate (Goths, Sasanian territories)
 *   - Syncretist Populations: Secondary victim (moderate/constrained) — coerced religious conformity, suppression of hybrid worship practices, forced institutional integration
 *   - Eusebius of Caesarea: Institutional narrator (institutional/arbitrage) — benefits from positioning as legitimacy-myth author; carries forward the vision testimony as institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vision_of_the_cross, 0.52).
domain_priors:suppression_score(vision_of_the_cross, 0.68).
domain_priors:theater_ratio(vision_of_the_cross, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vision_of_the_cross, extractiveness, 0.52).
narrative_ontology:constraint_metric(vision_of_the_cross, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vision_of_the_cross, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vision_of_the_cross, tangled_rope).
narrative_ontology:human_readable(vision_of_the_cross, "In Hoc Signo Vinces Mandate").
narrative_ontology:topic_domain(vision_of_the_cross, "religious/political").

domain_priors:requires_active_enforcement(vision_of_the_cross).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vision_of_the_cross, constantine_imperial_authority).
narrative_ontology:constraint_beneficiary(vision_of_the_cross, christian_clergy_institutional).
narrative_ontology:constraint_beneficiary(vision_of_the_cross, roman_military_command).
narrative_ontology:constraint_victim(vision_of_the_cross, pagan_religious_institutions).
narrative_ontology:constraint_victim(vision_of_the_cross, syncretist_populations).
narrative_ontology:constraint_victim(vision_of_the_cross, doctrinal_losers_in_nicene_disputes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PAGAN SUBJECTS (SNARE) — Cannot exit religious conversion mandate without penalty. Bears full extraction cost: loss of ancestral worship, forced reinterpretation of identity, coercion into Christian institutional structures. d≈0.92, f(d)≈1.39, σ=0.9 → χ≈0.52.
constraint_indexing:constraint_classification(vision_of_the_cross, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SYNCRETIST POPULATIONS (TANGLED ROPE) — Benefit from some coordination (Christian institutional access to services, unified moral framework), but extraction is severe: suppression of hybrid worship, forced doctrinal conformity. Constrained by social pressure and institutional integration. d≈0.65, f(d)≈0.95, σ=1.1 → χ≈0.48.
constraint_indexing:constraint_classification(vision_of_the_cross, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CHRISTIAN CLERGY (ROPE) — Experiences the constraint as pure coordination: unified doctrine, institutional hierarchy, state support. The vision narrative solves a collective action problem (which interpretation of Christ wins?), and Nicene orthodoxy emerges as the coordination solution. Clergy benefit from state enforcement and institutional stability. d≈0.35, f(d)≈0.25, σ=1.1 → χ≈0.14.
constraint_indexing:constraint_classification(vision_of_the_cross, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: CONSTANTINE AND IMPERIAL MILITARY (ROPE) — The vision narrative solves the imperial coordination problem: unifying military loyalty, centralizing religious authority to consolidate power, establishing legitimacy myth. Constantine benefits from the strategic value of Christian institutional organization. d≈0.15, f(d)≈0.00, σ=1.1 → χ≈0.00. Functionally a beneficiary with full arbitrage exit (could have chosen another religion or system).
constraint_indexing:constraint_classification(vision_of_the_cross, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE VISION TESTIMONY (PITON) — The core claim (Constantine saw a divine sign) is increasingly theatrical/inert. No historical evidence supports the vision; Eusebius likely invented it as institutional justification for orthodoxy; modern scholarship treats it as a retrospective mythmaking. The legitimacy mechanism has atrophied — the vision is maintained through centuries of repetition and institutional authority, not because anyone believes it happened. theater_ratio=0.81 satisfies piton gate. The claim persists due to theological inertia.
constraint_indexing:constraint_classification(vision_of_the_cross, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETING DOCTRINAL FACTIONS (TANGLED ROPE) — Arians and Nicenes both benefit from coordination under imperial patronage but suffer asymmetric extraction: the faction backed by Constantine's legitimacy myth (Nicene orthodoxy) extracts from rival factions (Arians, other heterodox groups). Even winners face constraints on doctrinal freedom. Mobile exit options (some factions can migrate to Sasanian empire or desert monasticism) limit maximum extraction. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.37.
constraint_indexing:constraint_classification(vision_of_the_cross, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN? — FALSE SUMMIT RISK) — From deep historical analysis, one might argue that religious consolidation is a structural necessity of empire (some unified religious framework enables administrative coherence), making Constantine's conversion appear as a natural law. However: (a) extractiveness=0.52 exceeds mountain ceiling of 0.25; (b) suppression=0.68 exceeds mountain ceiling of 0.05; (c) theater=0.81 suggests performative rather than natural mechanism. The mountain framing naturalizes what is a contingent institutional choice (Constantine could have unified without Christianity, or could have done it differently). Engine marks as false summit.
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
    constraint_indexing:constraint_classification(vision_of_the_cross, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. Constantine extracts significant costs from pagan populations and doctrinal losers (suppression of worship, elimination of rival institutional structures, forced conversion). The extraction is not total because (a) some populations adopt Christianity voluntarily through persuasion/incentives rather than pure coercion, (b) mobile factions can migrate, and (c) Constantine himself frames this as a positive unification, not rapine. Suppression (0.68): High. Multifaceted suppression: legal penalties for pagan worship, institutional defunding of pagan temples and priests, social shame for non-conversion, military integration of Christianity into the loyalty structure, doctrinal violence against Arian competitors. Theater ratio (0.81): High and increasing. The vision claim itself is theater — Eusebius is the sole source, Constantine's own writings don't mention it, and no contemporary corroboration exists. As extractive enforcement increases, the theatrical maintenance of the legitimacy myth becomes more obvious; by the late 4th century, the vision is sustained by institutional repetition, not persuasive power. The trajectory shows classic degradation: theater_ratio rises from 0.35 to 0.81 as function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   Extreme perspectival divergence. Constantine and his military perceive the vision mandate as pure coordination (unifying fragmented factions, creating shared moral identity, solving the problem of which Christian interpretation wins). Christian clergy perceive coordination benefits (institutional patronage, doctrinal resolution) but also benefit from asymmetric extraction (their faction wins, others lose). Arian and heterodox factions perceive tangled extraction — they gain some institutional resources but face doctrinal suppression despite theological sophistication. Pagan populations and conquered doctrinal losers perceive pure snare — mandatory conversion with no exit, loss of religious identity, institutional coercion. Eusebius perceives (or performs) the vision as foundational myth, treating theater as function. The analytical observer risks treating religious consolidation as inevitable (mountain), but the high extractiveness, suppression, and theater values reveal this as contingent institutional imposition. The perspectival gap is maximal because the beneficiary (Constantine) experiences rope-level coordination while the victim (pagan subjects) experiences snare-level extraction from identical structural phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Constantine and Imperial Military: Beneficiary + arbitrage → d≈0.15, f(d)≈0.00. Net beneficiary. Full exit options (could have chosen different unification strategy); benefits without extraction cost. Christian Clergy (Nicene): Beneficiary + constrained → d≈0.35, f(d)≈0.25. Net beneficiary but constrained by doctrinal orthodoxy. Benefits from state patronage; constrained by need to maintain ideological purity to retain legitimacy. Arian/Heterodox Factions: Victim + mobile → d≈0.50, f(d)≈0.65. Some extraction (doctrinal suppression, institutional disadvantage) but mobile exit (Sasanian empire, desert monasticism, migration with barbarian tribes). Pagan Subjects: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. No exit option; mandatory conversion; loss of ancestral worship. Syncretist Populations: Victim + constrained → d≈0.75, f(d)≈1.10. Significant extraction (forced conformity, suppression of hybrid practices); some constrained exit (geographic migration possible but socially costly). Eusebius: Institutional narrator + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through positioning as legitimacy-myth author; has exit options (could have written differently or not written).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that Constantine's vision mandate is a genuine hybrid—both coordination mechanism AND extraction device, simultaneously. The coordination function is real but asymmetric: it unifies Christian factions (solving a genuine doctrinal chaos problem) AND it enables imperial consolidation AND it suppresses non-winning factions and pagans. The extraction is not a secondary effect but a primary structural feature — the constraint's stability depends on sustained suppression of alternatives (pagan worship, Arian Christianity, syncretism). The tangled_rope classification avoids the false choice between 'this is just coordination' (beneficiary's illusion) and 'this is just extraction' (victim's reality). It is both, with the asymmetry measured by directionality: Constantine experiences χ≈0.0 (net beneficiary), while pagan subjects experience χ≈0.52 (net victim). The increasing theater ratio (0.35→0.81) shows the hallmark of a degraded constraint: as the original coordination function becomes less persuasive, maintenance relies increasingly on institutional repetition of the vision myth. By 362 CE (50 years in), the vision's factual status is irrelevant — belief in the claim is enforced by institutional authority, not by evidence or argument. This is piton-level theater, indicating that the mandate's underlying function has atrophied: what was once a persuasive coordination mechanism has become pure institutional inertia maintained by law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vision_authenticity,
    'Did Constantine actually experience a visionary experience, or did Eusebius fabricate the account for institutional legitimacy?',
    'Cross-examination of manuscript traditions, analysis of Eusebius''s redaction history, comparison with Constantine''s coins/edicts that predate the ''vision'' narrative',
    'If authentic: vision is exogenous shock revealing genuine religious experience. If fabricated: vision is pure institutional theater — piton classification strengthens. Classification shifts from tangled_rope to snare if theater completely decouples from function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_vision_authenticity, empirical, 'Whether Constantine''s vision was authentic or retrospectively constructed').

omega_variable(
    religious_consolidation_necessity,
    'Was religious consolidation a structural necessity for maintaining imperial coherence, or a contingent choice among viable alternatives?',
    'Comparative historical analysis: did later empires (Sasanian, Islamic Caliphate, Byzantine) achieve administrative coherence only through religious unification? Did they face equivalent costs when attempting pluralism?',
    'If necessity: some perspectives shift toward mountain (religious consolidation is a natural law of empires). If contingent: the tangled_rope/snare classifications hold; Constantine''s choice reflects institutional preference, not structural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_consolidation_necessity, conceptual, 'Whether religious consolidation was structurally necessary or contingent').

omega_variable(
    doctrine_winner_determination,
    'What structural property of Nicene Christianity (vs Arian, Donatist, other heterodox forms) made it the winning doctrine that Constantine''s legitimacy could best support?',
    'Analysis of theological simplicity, institutional scalability, appeal to educated elites, compatibility with Neoplatonic philosophy, organizational hierarchy vs flat structure',
    'If Nicene was objectively superior for coordination: legitimacy of Constantine''s choice increases, snare classification for losers becomes harder to defend. If selection was arbitrary: Constantine''s backing was pure institutional power play — snare extraction for doctrinal losers becomes unambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_winner_determination, conceptual, 'What made Nicene Christianity the stabilizing doctrinal choice').

omega_variable(
    conversion_coercion_timeline,
    'Over what historical arc did Constantine''s vision mandate transition from persuasion to coercion, and what triggers marked the shift?',
    'Timeline analysis: Constantine''s edicts (312-337), comparison with successors'' enforcement (Theodosius I, Justinian), measurement of legal penalty severity over decades',
    'If coercion increased monotonically: snare classification grows stronger. If it remained voluntary-but-incentivized for centuries: rope classification persists. The transition point marks when the constraint moves from coordination to pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conversion_coercion_timeline, empirical, 'Timeline of transition from voluntary to coercive religious consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vision_of_the_cross, 312, 362).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visioncross_tr_t0, vision_of_the_cross, theater_ratio, 0, 0.35).
narrative_ontology:measurement(visioncross_tr_t25, vision_of_the_cross, theater_ratio, 25, 0.58).
narrative_ontology:measurement(visioncross_tr_t50, vision_of_the_cross, theater_ratio, 50, 0.81).

% Extraction over time
narrative_ontology:measurement(visioncross_be_t0, vision_of_the_cross, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(visioncross_be_t25, vision_of_the_cross, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(visioncross_be_t50, vision_of_the_cross, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vision_of_the_cross, enforcement_mechanism).
narrative_ontology:affects_constraint(vision_of_the_cross, nicene_orthodoxy_enforcement).
narrative_ontology:affects_constraint(vision_of_the_cross, pagan_temple_suppression).
narrative_ontology:affects_constraint(vision_of_the_cross, arian_christology_persecution).

% DUAL FORMULATION NOTE:
% The vision mandate is downstream of Constantine's broader imperial consolidation strategy but represents a distinct constraint operating through religious symbolism. Related constraints (Nicene orthodoxy enforcement, pagan suppression, Arian persecution) are entangled with the vision narrative but have independent structural properties. The vision myth is the legitimacy mechanism enabling all three downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vision_of_the_cross, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
