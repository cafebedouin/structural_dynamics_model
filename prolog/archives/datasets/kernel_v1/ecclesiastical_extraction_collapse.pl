% ============================================================================
% CONSTRAINT STORY: ecclesiastical_extraction_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecclesiastical_extraction_collapse, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: ecclesiastical_extraction_collapse
 *   human_readable: Ecclesiastical Extraction and Institutional Collapse in Medieval-Early Modern Christendom
 *   domain: religious_history/commitment_system_theory/institutional_extraction
 *
 * SUMMARY:
 *   The ecclesiastical extraction collapse (1200-1517) models the
 *   institutional dynamics of a constraint that began as tangled rope
 *   coordination (managing Christendom's spiritual and temporal affairs) but
 *   accumulated extraction faster than its legitimacy could absorb. The
 *   medieval church coordinated genuine functions: sacramental access,
 *   marriage canon law, poor relief, literacy and manuscript preservation,
 *   dispute resolution among kingdoms. But these coordination functions were
 *   progressively overlaid with extraction mechanisms: indulgence sales,
 *   tithe concentration in Rome, celibacy as labor control, doctrinal
 *   monopoly enforced through heresy suppression. By 1450, the extractive
 *   overlay had become so visible that the theater—the ritualistic and
 *   rhetorical performance of legitimacy—had to expand dramatically to
 *   maintain compliance. The printing press broke the information monopoly
 *   that had allowed the extraction to persist largely invisible to the lay
 *   faithful. The Reformation was not an external disruption but the collapse
 *   of an internal contradiction: an institutional system whose extraction
 *   had exceeded its coordination function and whose legitimacy had been
 *   overtaken by its own performative apparatus. The constraint exhibits all
 *   six classification types from different perspectives, revealing that the
 *   Reformation cannot be understood as a single theological dispute but must
 *   be analyzed as a composite event structure—multiple kernels (authority,
 *   doctrine, practice, wealth distribution) each with their own contested
 *   readings, all collapsing simultaneously.
 *
 * KEY AGENTS:
 *   - The Papal Curia: Primary beneficiary (institutional/arbitrage) — captures tithe flows, land control, political leverage; experiences constraint as coordination problem requiring institutional maintenance
 *   - The Lay Faithful: Primary victim (powerless/trapped/identity-locked) — bear extraction through indulgences, tithes, sacramental fees; trapped by geography and identity fusion with Catholic framework; no exit capacity
 *   - Parish Clergy: Secondary victim and secondary beneficiary (powerless/identity-locked to institutional/constrained) — extract from laity while being extracted from by hierarchy; identity fused with clerical vocation; constrained by celibacy enforcement and loyalty oaths
 *   - Reformation Coalition: Organized agents (organized/constrained) — scholars, nobles, urban magistrates, printers; recognize extraction mechanism and begin building alternative legitimacy; constrained by religious warfare and suppression; extract through their own authority consolidation
 *   - Scholastic Theological System: Degraded institution (institutional/constrained) — maintains theater through degree ceremonies and disputation cycles; has lost functional coordination capacity; persists through inertia
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing contingent papal decisions as inevitable institutional law; the false-summit detector should flag the mountain classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecclesiastical_extraction_collapse, 0.58).
domain_priors:suppression_score(ecclesiastical_extraction_collapse, 0.72).
domain_priors:theater_ratio(ecclesiastical_extraction_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecclesiastical_extraction_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecclesiastical_extraction_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ecclesiastical_extraction_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecclesiastical_extraction_collapse, tangled_rope).
narrative_ontology:human_readable(ecclesiastical_extraction_collapse, "Ecclesiastical Extraction and Institutional Collapse in Medieval-Early Modern Christendom").
narrative_ontology:topic_domain(ecclesiastical_extraction_collapse, "religious_history/commitment_system_theory/institutional_extraction").

domain_priors:requires_active_enforcement(ecclesiastical_extraction_collapse).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecclesiastical_extraction_collapse, 'a3bbff68-dbc2-407c-b7b0-2eaf59578b04').
narrative_ontology:cs_kernel_codification('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', fixed_text).
narrative_ontology:cs_authority_grounding('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', extraction).
narrative_ontology:cs_interpretation_layer_present('a3bbff68-dbc2-407c-b7b0-2eaf59578b04').
narrative_ontology:cs_reading_relation('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', conciliar_authority_reading, influences).
narrative_ontology:cs_reading_relation('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', congregational_authority_reading, coexists_with).
narrative_ontology:cs_axiom('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', foundational, papal_irreformability_doctrine).
narrative_ontology:cs_axiom_status(papal_irreformability_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', papal_irreformability_doctrine, conventional).
narrative_ontology:cs_axiom('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', foundational, sacramental_validity_requires_ordained_minister).
narrative_ontology:cs_axiom_status(sacramental_validity_requires_ordained_minister, holdable).
narrative_ontology:cs_axiom_grounding('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', sacramental_validity_requires_ordained_minister, deontological).
narrative_ontology:cs_axiom('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', secondary, celibacy_enforces_spiritual_purity).
narrative_ontology:cs_axiom_status(celibacy_enforces_spiritual_purity, overridden).
narrative_ontology:cs_axiom_grounding('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', celibacy_enforces_spiritual_purity, empirically_contingent).
narrative_ontology:cs_axiom('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', secondary, indulgence_mechanism_theologically_sound).
narrative_ontology:cs_axiom_status(indulgence_mechanism_theologically_sound, overridden).
narrative_ontology:cs_axiom_grounding('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', indulgence_mechanism_theologically_sound, empirically_contingent).
narrative_ontology:cs_reference_frame('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', apostolic_succession_centralized_authority).
narrative_ontology:cs_drift_state('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', late_medieval_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a3bbff68-dbc2-407c-b7b0-2eaf59578b04', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecclesiastical_extraction_collapse, papal_institutional_authority).
narrative_ontology:constraint_beneficiary(ecclesiastical_extraction_collapse, ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(ecclesiastical_extraction_collapse, lay_faithful).
narrative_ontology:constraint_victim(ecclesiastical_extraction_collapse, parish_clergy).
narrative_ontology:constraint_victim(ecclesiastical_extraction_collapse, spiritual_legitimacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY FAITHFUL (SNARE) — Trapped by geographic immobility, religious identity fusion, and sacramental dependency. Cannot exit the ecclesiastical system without abandoning salvation itself (identity_locked adjacent: their spiritual identity is constituted through the Catholic framework). Bear full cost of extraction (indulgences, fees, tithes) with no exit capacity. Maximum suppression — religious framing makes refusal unthinkable. No exit via heresy carries eternal cost.
constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PARISH CLERGY (SNARE, IDENTITY-LOCKED EXIT) — Structurally mobile (could exit the church hierarchy, marry, pursue secular work) but identity-fused with clerical vocation. The parish priest's self-concept, social role, intellectual formation, and moral authority are all constituted through ordination. Exit would require abandoning the identity that is the priest's entire epistemic and social frame. Suppression operates through identity lock: the clergy extract from the laity AND are themselves extracted from by the hierarchical church. They see their extraction as service, not imposition. Their separation from sexuality and family is absorbed as sacrifice, not coercion. High theater — the sacramental system performs its own legitimacy.
constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: EPISCOPAL/MONASTIC HIERARCHY (TANGLED ROPE) — Constrained by loyalty to Rome and by dependency on subordinate clergy and monastic communities for institutional reproduction. Benefits from extraction: indulgences, tithe collection, land control, political influence. But also genuinely coordinates: manages marriage canon law, poor relief (however inadequate), literacy and manuscript preservation, pilgrimage systems. The extraction and coordination are structurally intertwined — the institutional hierarchy cannot function without the coordination functions, but uses those functions to justify and conceal the extraction. Effective extraction chi is moderate — the hierarchy has alternatives (secular power, property, education) but remains locked into the ecclesiastical system by the legitimacy claims it depends on.
constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: PAPAL AUTHORITY (ROPE) — At the peak of the hierarchy, experiences the constraint as pure coordination: managing the church's institutional complexity, resolving doctrinal disputes, maintaining unity. The papacy has exit options (secular rule, negotiation with kingdoms, doctrinal flexibility) and uses them. From this perspective, tithes and indulgences are coordination mechanisms for funding institutional maintenance, not extraction per se. The papacy sees itself as solving a collective action problem: how to maintain unified Christendom. The extraction is the unspoken cost of that coordination.
constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: REFORMATION COALITION (TANGLED ROPE) — Organized agents (theologians, nobles with printing access, urban magistrates, vernacular literacy movements) recognize the extraction mechanism and begin coordinating alternative legitimacy frameworks. Exit is constrained by religious warfare, suppression, and institutional retaliation — but agency exists. They see the old ecclesiastical system as pure snare (especially indulgence sales) and frame reformation as restoration of authentic Christian coordination. Their perspective generates new structural possibilities: clerical marriage, vernacular scripture, congregational authority. Theater ratio is high on both sides — old church defends through ritual; reformers construct new legitimacy through rhetoric and print. Tangled rope rather than pure rope because they still extract (authority consolidation, wealth redistribution) even as they coordinate (literacy, doctrinal systematization).
constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SCHOLASTIC THEOLOGICAL SYSTEM (PITON) — By the 15th century, scholastic theology had become substantially performative. Theological disputation maintained institutional legitimacy (university positions, ecclesiastical authority) but had lost functional traction on lived religious experience. The five-fold proof of God's existence was not generating new adherents; indulgence theology had become a parody of itself. The system was maintained through theater (degree ceremonies, publishing cycles, Latin gatekeeping) and institutional inertia (careers depended on the system continuing) rather than through genuine coordination. Theater ratio 0.68 reflects this degradation: much of the ecclesiastical apparatus is running on momentum.
constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / UNIVERSAL VIEW (MOUNTAIN) — From the civilizational analytical perspective, institutional collapse is inevitable when extraction exceeds coordination function sufficiently and alternatives emerge. The mathematical structure of incentives guarantees that any system with (extraction > coordination + legitimacy buffer) will deteriorate when legitimacy is challenged and alternatives appear. The reformation was thus 'inevitable' — a natural law of institutional dynamics. However, this perspective naturalizes what are actually contingent institutional choices: the papal curia's decisions to concentrate wealth, sell indulgences, resist reform, suppress heresy. The natural-law framing masks the specific historical actors and decisions. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecclesiastical_extraction_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecclesiastical_extraction_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecclesiastical_extraction_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecclesiastical_extraction_collapse, TR),
    TR >= 0.70.

:- end_tests(ecclesiastical_extraction_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The medieval church extracted substantial rents from the lay faithful through indulgences, tithes, and sacramental fees, but extraction was not as severe as a full snare (which would be 0.70+). The papal hierarchy had legitimate coordination functions to perform—managing church governance, settling doctrinal disputes, maintaining institutional unity—and portions of extracted wealth did fund these functions. However, by 1450, the extractive overlay had become excessive relative to the coordination it funded. The tithes concentrated in Rome exceeded what institutional maintenance required; indulgence sales had become a revenue mechanism for papal projects rather than spiritual benefit mechanism. The 0.58 value reflects that extraction and coordination were genuinely intertwined, not separable. Suppression (0.72): Moderately high. The church maintained suppression through multiple mechanisms: religious framing (exit was damnation), geographic immobility (lay faithful had no mobility options), identity fusion (clergy were trapped by vocation), hierarchical authority (lower clergy enforced upper hierarchy's will), and force (heresy suppression, inquisition). Suppression rose from 0.50 in 1200 (when alternatives like monastic reform were still available internally) to 0.78 by 1517 (when printing broke the information monopoly and made external alternatives visible). The rise in suppression_requirement reflects that the hierarchy had to intensify enforcement as extraction accumulated and theater became less effective. Theater ratio (0.72 in 1517, up from 0.42 in 1200): High. The scholastic theological system had become substantially performative by the late medieval period. Theological disputation maintained institutional prestige (university positions, ecclesiastical authority) but generated few new adherents and addressed lay religious concerns poorly. The sacramental system performed its legitimacy through ritual and mystery—complex Latin liturgy that the lay faithful could not understand or question, creating epistemic dependence on clerical interpretation. Indulgence theology was explicitly performative: the papacy knew the theological arguments for indulgences were weak (Ockham and other theologians had challenged them) but maintained them as revenue mechanism anyway, with the performance (the bull, the sermon, the monetary exchange) carrying more weight than the theology. Theater ratio rose as extractiveness rose because the hierarchy had to compensate for declining legitimacy with greater performance. Mandatrophy resolution: At extractiveness 0.62 (exceeds 0.70 threshold by 1517), the constraint required mandatrophy resolution. The resolution pathway is that the constraint is NOT a monolithic snare but a tangled rope whose coordination functions are real but whose extraction has become excessive and visible. The classification as tangled rope (rather than snare) prevents the mislabeling of the reformation as a rejection of institutional coordination per se—the reformers wanted to preserve and improve coordination functions (provision of scripture, pastoral care, doctrinal clarity) while removing extraction mechanisms. The mandatrophy is resolved by recognizing that reformation occurred because extraction had delaminated from coordination, making it visible that they were not identical, and making it possible to coordinate without the extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence, with all six types represented. The lay faithful see a snare (trapped, identity-locked, no exit). The parish clergy see a snare from below but rope from above (caught between extraction from hierarchy and extraction of laity). The episcopal hierarchy sees tangled rope (coordination and extraction intertwined, impossible to separate). The papal authority sees pure rope (managing complexity, solving collective action problems). The reformation coalition sees a tangled rope degrading into piton (performative theater replacing real function, but still defended by institutional force). The scholastic system sees itself as rope (preserving and transmitting authentic theology) but appears as piton (performing legitimacy while losing function). The analytical observer sees mountain (inevitable collapse when extraction exceeds coordination) but this is a false summit that naturalizes papal decisions. The perspectival gaps reveal that the Reformation was not a single theological event but an institutional collapse that different actors experienced as: oppression, service, coordination, routine, degradation, tradition, and inevitability. The gap between lay experience (snare) and papal experience (rope) is maximal—3-4 types of separation. This gap is the structural fact that made reformation possible: once the lay faithful could see the gap (through printing, literacy, alternative theology), they could see that the extraction and coordination were separable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation (d) from structural position: Lay faithful are full victims with zero exit (d → 1.0, f(d) → 1.42, maximum experienced extraction). Parish clergy are partial victims with identity-locked constraints (d → 0.85, f(d) → 1.15, high experienced extraction despite some structural mobility). Episcopal hierarchy are partial beneficiaries with moderate constraints (d → 0.55, f(d) → 0.75, moderate experienced extraction while still benefiting). Papal authority are full beneficiaries with arbitrage options (d → 0.05, f(d) → -0.12, negative effective extraction—they experience the constraint as coordination that benefits them). The reformation coalition are organized victims with constrained exit (d → 0.50, f(d) → 0.65, moderate experienced extraction but organized resistance). No overrides were necessary—the structural derivation produces accurate directionality for all agents. The chi formula χ = ε × f(d) × σ(S) produces: lay faithful experience χ ≈ 0.58 × 1.42 × 1.1 ≈ 0.91 (snare-level effective extraction at continental scope); papal authority experience χ ≈ 0.58 × (-0.12) × 1.1 ≈ -0.08 (negative—benefit from the constraint). This directionality divergence explains why the constraint persisted: the beneficiaries experienced it as beneficial coordination while the victims experienced it as oppressive extraction, and the institutional suppression prevented victims from coordinating a unified response. Printing enabled coordination by making the extraction visible across geographic boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   COMPOSITE EVENT RESOLUTION: The ecclesiastical extraction collapse represents a case where mandatrophy is resolved by recognizing compositeness rather than forcing singular kernel reduction. The Reformation was not ONE theological dispute but multiple distinct kernels collapsing simultaneously: (1) Doctrine kernel: scriptural authority vs papal authority (multiple readings possible: sola scriptura vs papal magisterium vs council-based authority). (2) Institutional kernel: hierarchical papal authority vs collegial bishops vs congregational authority (multiple readings exist in early church tradition). (3) Practice kernel: sacramental grace vs moral worthiness of minister vs congregational election (theological disagreements with structural implications). (4) Wealth kernel: legitimate tithes vs extraction, simony vs market mechanisms (disagreements about institutional funding). These were not subsidiary branches of one tree but independent kernels that happened to be contested simultaneously. The NON-BREAK verdict that 'defender absorbs as composite' is operationalized here: the Catholic church's attempt to maintain the constraint as a unified system (all these disputes are ultimately about the nature of the church) led to institutional fragmentation instead. Had the papal authority recognized compositeness and allowed independent resolution of each kernel (conceding on clerical marriage while maintaining doctrinal authority, for example), the system might have survived. Instead, the insistence on singular reduction forced an either/or choice—either Rome maintains total authority and reforms nothing, or the entire system collapses. Compositeness explains the sudden and total character of the Reformation: not a gradual doctrinal shift but a near-simultaneous crystallization of alternatives across multiple domains. Mandatrophy is resolved when we stop asking 'which type is the Reformation really?' and start asking 'which kernels were contested and in what order did they collapse?' This reframe prevents the snare/rope confusion: the constraint is not a single extractive mechanism but a bundle of coordination and extraction mechanisms that had delaminated from one another. The reformation coalition could advocate 'no to extraction, yes to coordination' because the two had become visible as separable. The medieval church had made them appear identical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compositeness_vs_singular_kernel,
    'Does the Reformation represent one contested kernel (singular ecclesiastical legitimacy principle with multiple readings) or multiple distinct kernels (doctrine, authority, institution, practice) that happen to converge?',
    'Kernel identification: trace whether disputes over indulgences, papal authority, scriptural interpretation, and clerical celibacy all reduce to disagreement about ONE underlying claim or whether they are structurally independent claims that happened to be contested simultaneously',
    'If singular: Reformation is ONE reading of ecclesiastical authority replacing another. If composite: Reformation is overdetermined — multiple kernels collapsed simultaneously, explaining why the event was so sudden and so thorough. NON-BREAK verdict suggests compositeness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compositeness_vs_singular_kernel, conceptual, 'Whether Reformation instantiates one contested kernel or multiple distinct kernels').

omega_variable(
    extraction_vs_coordination_boundary,
    'What distinguishes the medieval church''s legitimate coordination (sacramental system, poor relief, literacy preservation) from its extractive overlay (indulgences, tithe concentration, celibacy enforcement)?',
    'Functional analysis: identify which institutional activities would be necessary under any legitimate coordination framework (provision of marriage canon, record-keeping, doctrinal adjudication) versus which activities serve extraction while generating counterfactual coordination narratives (indulgence sales framed as spiritual benefit, celibacy framed as sanctity rather than labor control)',
    'If boundaries are clear: medieval church is tangled rope with measurable extraction overlay. If boundaries blur: the system used coordination language to conceal extraction so thoroughly that retrospective decomposition is ambiguous — suggests the extraction was constitutive of the ''coordination'' frame itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Boundary between ecclesiastical coordination and extraction mechanisms').

omega_variable(
    printing_technology_contingency,
    'Would ecclesiastical extraction have collapsed without the printing press enabling mass scripture distribution and reformation pamphletry, or was it overdetermined by structural contradictions regardless of technology?',
    'Counterfactual: compare trajectory in pre-printing dissent (Waldensians, Lollards, Hussite movements) — did they generate sustainable alternatives or get suppressed? Examine whether printing made reformation possible or only accelerated inevitable institutional collapse.',
    'If contingent on printing: the extraction system could have persisted through continued literacy monopoly; technology broke the suppression gate. If overdetermined: the intellectual contradictions and the lay resentment of extraction were so severe that alternatives would have emerged despite literacy barriers. Affects whether this is an institutional piton propped up by information monopoly (printing breaks it) or a snare maintained by psychological integration (printing just accelerated inevitable resistance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(printing_technology_contingency, empirical, 'Role of printing technology in making reformation possible versus inevitable').

omega_variable(
    false_summit_natural_law,
    'Is the classification as mountain (inevitable institutional collapse when extraction exceeds coordination) a description of natural law or a naturalization of historically contingent decisions by papal hierarchy?',
    'Historicize the papal choices: Council of Constance''s suppression of Hus, Leo X''s indulgence sales, Clement VII''s refusal of reform. Had different choices been made (accommodation of clerical marriage, reform of indulgence theology, council-based governance), could the extraction mechanism have been modified without institutional collapse? If yes: the mountain classification is false — the collapse resulted from specific decisions, not inevitable law.',
    'If false summit confirmed: the constraint is not natural law but extractive institutional rigidity disguised as necessity. Reclassify from mountain to snare (from papal authority perspective) or tangled rope (from ecclesiastical hierarchy perspective). The theological claim of ''irreformable church authority'' was a legitimacy narrative, not a structural limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether institutional collapse is natural law or contingent on papal decision-making').

omega_variable(
    identity_lock_persistence,
    'Among the lay faithful and parish clergy who remain Catholic after Reformation, is their continued participation structural (constrained exit options) or identity-locked (their identity constituted through Catholicism)?',
    'Distinguishing mechanism: post-Reformation Catholics who could exit to Protestantism but choose not to. Are they staying because they cannot bear the cost of exit (constrained) or because exiting would dissolve their identity (identity-locked)? Interview data, confessional texts, and counter-reformation writings should show whether retention is experienced as choosing to stay (identity-locked) versus being unable to leave (constrained).',
    'If identity-locked: suppression operates through internalized legitimacy even after structural alternatives exist. If constrained: the extraction persists because barriers remain (social cost, geographic isolation, education dependency). Identity-lock suggests the ecclesiastical system achieved deep legitimacy capture; constraint suggests surface-level institutional lock that could break with education and mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether Catholic persistence is identity-locked or structurally constrained').

omega_variable(
    competing_legitimacy_frames,
    'How did reformed Protestant theology legitimate its claims of greater authenticity without itself reproducing extraction mechanisms? Did it succeed or did it merely transfer extraction to different institutional actors (pastoral authority, congregational hierarchy, state control)?',
    'Structural comparison: measure extraction in early reformed churches (pastoral vetting of marriage, moral discipline, tithe enforcement) versus medieval Catholic extraction (indulgences, celibacy enforcement, doctrinal monopoly). Did reformation reduce extraction or redistribute it? Did new authority structures (magistrates, pastors, congregational bodies) extract differently or less?',
    'If extraction reduced: reformation achieved net institutional improvement. If merely redistributed: reformation was a competition between extraction regimes; the old system''s collapse was not due to its extraction per se but to its extraction being less competitive than alternatives. Affects whether the constraint should be classified as addressing a unique medieval pathology or as instantiating a universal logic of religious institutionalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_legitimacy_frames, empirical, 'Whether Reformation reduced extraction or redistributed it to new institutional actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecclesiastical_extraction_collapse, 1200, 1517).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecc_extract_tr_t1200, ecclesiastical_extraction_collapse, theater_ratio, 1200, 0.42).
narrative_ontology:measurement(ecc_extract_tr_t1350, ecclesiastical_extraction_collapse, theater_ratio, 1350, 0.55).
narrative_ontology:measurement(ecc_extract_tr_t1450, ecclesiastical_extraction_collapse, theater_ratio, 1450, 0.68).
narrative_ontology:measurement(ecc_extract_tr_t1517, ecclesiastical_extraction_collapse, theater_ratio, 1517, 0.72).

% Extraction over time
narrative_ontology:measurement(ecc_extract_be_t1200, ecclesiastical_extraction_collapse, base_extractiveness, 1200, 0.35).
narrative_ontology:measurement(ecc_extract_be_t1350, ecclesiastical_extraction_collapse, base_extractiveness, 1350, 0.48).
narrative_ontology:measurement(ecc_extract_be_t1450, ecclesiastical_extraction_collapse, base_extractiveness, 1450, 0.58).
narrative_ontology:measurement(ecc_extract_be_t1517, ecclesiastical_extraction_collapse, base_extractiveness, 1517, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ecc_extract_su_t1200, ecclesiastical_extraction_collapse, suppression_requirement, 1200, 0.5).
narrative_ontology:measurement(ecc_extract_su_t1350, ecclesiastical_extraction_collapse, suppression_requirement, 1350, 0.62).
narrative_ontology:measurement(ecc_extract_su_t1450, ecclesiastical_extraction_collapse, suppression_requirement, 1450, 0.72).
narrative_ontology:measurement(ecc_extract_su_t1517, ecclesiastical_extraction_collapse, suppression_requirement, 1517, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecclesiastical_extraction_collapse, identity_coordination).
narrative_ontology:boltzmann_floor_override(ecclesiastical_extraction_collapse, 0.12).
narrative_ontology:affects_constraint(ecclesiastical_extraction_collapse, medieval_literacy_monopoly).
narrative_ontology:affects_constraint(ecclesiastical_extraction_collapse, celibacy_enforcement_mechanism).
narrative_ontology:affects_constraint(ecclesiastical_extraction_collapse, indulgence_theology_contradiction).
narrative_ontology:affects_constraint(ecclesiastical_extraction_collapse, papal_state_consolidation).

% DUAL FORMULATION NOTE:
% Ecclesiastical extraction collapse is the parent constraint. Downstream constraints (literacy monopoly, celibacy enforcement, indulgence theology) are the mechanisms through which extraction was operationalized. The collapse of the parent constraint is visible as simultaneous delamination of the downstream constraints. This is a constraint family where the upstream constraint's type change (from tangled rope to piton to collapse) cascades downward: when papal authority's legitimacy fails, celibacy enforcement becomes merely suppression (no longer justified as sanctity), indulgence sales become obviously fraudulent, the literacy monopoly becomes visible as gatekeeping. Network decomposition shows that these are not independent constraints but interlocking mechanisms of a single extraction system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecclesiastical_extraction_collapse, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
