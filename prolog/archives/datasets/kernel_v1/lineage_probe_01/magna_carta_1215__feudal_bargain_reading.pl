% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__feudal_bargain_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__feudal_bargain_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_1215__feudal_bargain_reading
 *   human_readable: Magna Carta 1215: Feudal Bargain Reading
 *   domain: legal/doctrinal/feudal_political_economy
 *
 * SUMMARY:
 *   Magna Carta (1215) was a feudal peace treaty negotiated between King John
 *   and a coalition of rebel barons following military defeat at the Battle
 *   of Runnymede. The charter codified specific grievances about feudal
 *   incidents — reliefs (payments due upon inheritance of fiefs), wardships
 *   (crown exploitation of underage heirs' lands during minority), and
 *   scutage (military service substitution fees). From the feudal bargain
 *   reading, Magna Carta is structurally a tangled_rope: it coordinates
 *   feudal obligations (legitimate coordination function) while constraining
 *   the crown's fiscal extraction from these incidents (asymmetric benefit to
 *   baronage). The charter was annulled by Pope Innocent III within months,
 *   reissued under pressure repeatedly (1217, 1225, 1297), and functioned as
 *   a feudal peace treaty enforced by periodic baronial threat rather than as
 *   a permanent constitutional foundation. This reading contests the
 *   common_law_foundation reading, which claims the charter seeded
 *   constitutional due process and legality doctrine; it also contests the
 *   symbolic_myth reading, which claims the charter's authority is
 *   invocational rather than structural. The feudal bargain reading grounds
 *   Magna Carta's legitimacy in the concrete power balance between defeated
 *   king and armed magnates, with extractiveness capped to feudal incidents
 *   affecting the baronage class specifically.
 *
 * KEY AGENTS:
 *   - Rebel Baronage Coalition: Primary beneficiary (organized/arbitrage) — achieves reduced reliefs, wardship restrictions, scutage caps; enforces via threat of renewed conflict
 *   - Royal Fiscal Officers: Primary victim (powerless/trapped) — enforcement agents for feudal incidents now unilaterally constrained by treaty they did not negotiate
 *   - Crown Revenue Apparatus: Secondary victim (institutional/constrained) — experiences total revenue reduction during active enforcement periods; gradually adapts via alternative revenue sources
 *   - Shire Knights and Mid-Rank Feudal Holders: Secondary actors (moderate/constrained) — modest beneficiaries from capped incidents; constrained by persistent feudal service obligations
 *   - Pope Innocent III: Institutional actor with annulment authority (institutional/arbitrage) — exercises authority to void the charter within months, returning maximum extraction capacity to crown
 *   - Crown (Reissuing Perspective): Institutional perspective (institutional/constrained) — later reissues as crown recognizes charter enforceability and coordination value despite initial extraction costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__feudal_bargain_reading, 0.38).
domain_priors:suppression_score(magna_carta_1215__feudal_bargain_reading, 0.42).
domain_priors:theater_ratio(magna_carta_1215__feudal_bargain_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__feudal_bargain_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__feudal_bargain_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__feudal_bargain_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__feudal_bargain_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__feudal_bargain_reading, "Magna Carta 1215: Feudal Bargain Reading").
narrative_ontology:topic_domain(magna_carta_1215__feudal_bargain_reading, "legal/doctrinal/feudal_political_economy").

domain_priors:requires_active_enforcement(magna_carta_1215__feudal_bargain_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__feudal_bargain_reading, 'b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744').
narrative_ontology:cs_kernel_codification('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', formalized).
narrative_ontology:cs_authority_grounding('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', lineage).
narrative_ontology:cs_interpretation_layer_present('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744').
narrative_ontology:cs_reading_relation('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', magna_carta_1215__common_law_foundation_reading, influences).
narrative_ontology:cs_reading_relation('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', magna_carta_1215__symbolic_myth_reading, coexists_with).
narrative_ontology:cs_axiom('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', foundational, feudal_incidents_materially_constrainable).
narrative_ontology:cs_axiom_status(feudal_incidents_materially_constrainable, holdable).
narrative_ontology:cs_axiom_grounding('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', feudal_incidents_materially_constrainable, empirically_contingent).
narrative_ontology:cs_axiom('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', foundational, charter_enforced_by_periodic_threat).
narrative_ontology:cs_axiom_status(charter_enforced_by_periodic_threat, holdable).
narrative_ontology:cs_axiom_grounding('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', charter_enforced_by_periodic_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', feudal_incident_bargain).
narrative_ontology:cs_created_at('b9e2ffe3-88e3-4ccf-9c15-dd3211cd3744', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__feudal_bargain_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__feudal_bargain_reading, rebel_baronage).
narrative_ontology:constraint_victim(magna_carta_1215__feudal_bargain_reading, royal_fiscal_officers).
narrative_ontology:constraint_victim(magna_carta_1215__feudal_bargain_reading, crown_revenue_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ROYAL FISCAL OFFICER (SNARE) — Trapped between the king's fiscal demands and baronial resistance. The feudal incidents (reliefs, wardships, scutage) that comprise their enforcement domain have been unilaterally constrained by a peace treaty they did not negotiate. No exit from the enforcement apparatus; maximum experienced extraction as the target of baronial grievances.
constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SHIRE KNIGHT (TANGLED ROPE) — Mid-level feudal actor constrained by both royal demands and baronial pressure. The charter benefits them modestly (capped reliefs, wardship restrictions) while still extracting feudal service obligations. Experiences mixed coordination (the charter settles which obligations are owed) and extraction (those obligations persist). Constrained by social status and landholding dependency.
constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REBEL BARONAGE COALITION (ROPE) — Primary beneficiary. The charter is a pure coordination mechanism from their perspective: it translates military victory into codified feudal obligations, enabling them to organize subsequent enforcement and exit the state of war with retained spoils. Arbitrage position: they can enforce the charter via threat of renewed conflict. The charter coordinates their grievances into a stable agreement.
constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CROWN REISSUING PERSPECTIVE (TANGLED ROPE) — The crown reissues the charter repeatedly (1217, 1225, 1297) despite its initial annulment. From this institutional perspective, the charter becomes a coordination mechanism: it codifies feudal obligations in a way that prevents civil war and enables revenue collection via reliable channels rather than arbitrary exaction. The crown experiences both benefit (stable revenue, legitimacy from observing law) and extraction (capped incidents reduce total take).
constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEUDAL CEREMONY (PITON) — Viewed from the civilizational distance, Magna Carta's feudal clauses are increasingly performative: as crown revenue sources shift from feudal incidents to direct taxation (13th-14th centuries), the charter's careful specification of reliefs, wardships, and scutage becomes theater — the constraints persist through repetition and institutional inertia long after their functional role has atrophied. By the 15th century, the feudal incidents are largely ceremonial.
constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FEUDAL INEVITABILITY VIEW (MOUNTAIN) — From maximum distance, Magna Carta might appear as an immutable natural law of feudal political economy: a defeated sovereign must concede to victorious magnates; this is the structural logic of feudalism. However, the structural data contradicts mountain classification: identifiable beneficiaries (baronage), victims (fiscal officers), and time-bounded enforcement (annulled within months, requiring repeated reissue) reveal that what appears 'inevitable to feudalism' is actually a contingent bargain between power holders. The engine's false summit detection will flag this.
constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__feudal_bargain_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_1215__feudal_bargain_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_1215__feudal_bargain_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_1215__feudal_bargain_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The charter constrains feudal incidents (reliefs, wardships, scutage) that benefitted the crown and harmed baronage. The extractiveness value reflects that the constraint applies narrowly to one class of feudal obligations affecting a specific beneficiary group (the rebel barons), not to royal fiscal power broadly. Non-feudal revenues (direct taxes, customs, demesnes) remain unconstrained. The measurements show high extractiveness at t=0 (pre-charter arbitrary exaction), collapse at t=3 (active baronial enforcement of constraints), and recovery at t=9 as crown develops alternative revenue and baronial enforcement weakens. Suppression (0.42): Moderate. Suppression is confined to baronial grievances about feudal incidents — the charter does not suppress alternatives broadly, but rather narrows the king's options in handling reliefs, wardships, and scutage. The pope's annulment power represents major suppression (t=0 extraction surge on annulment), which then decays as reissues establish enforcement expectation. Theater ratio (0.35): Low-moderate. The feudal bargain reading emphasizes substantive constraint on specific fiscal mechanisms rather than performance. Unlike the symbolic_myth reading (which would score high theater), the feudal bargain reading sees the charter as material coordination of concrete obligations, with theater increasing over time (t=0 to t=9) as the charter's functional role shifts from enforcement to ceremonial reconfirmation.
 *
 * PERSPECTIVAL GAP:
 *   The feudal bargain reading produces a substantial perspectival gap between beneficiaries and victims. Rebel barons see coordination that secured their grievances (Rope perspective, or rope-phase of tangled_rope when recognizing persistent extraction elsewhere). Fiscal officers see pure extraction (Snare perspective). The crown sees mixed benefit/harm (Tangled Rope — the charter coordinates feudal obligations reliably, enabling revenue collection through predictable channels, while simultaneously capping the take). The piton perspective emerges at civilizational distance: by the 13th-14th centuries, as crown revenue sources diversify away from feudal incidents, the careful specification of reliefs and wardships becomes increasingly ceremonial — the charter persists through inertia and symbolic authority rather than functional constraint. The analytical observer at universal scope risks collapsing this variance into 'inevitable feudal structure,' which the false summit detector identifies as naturalization of contingent bargaining.
 *
 * DIRECTIONALITY LOGIC:
 *   Baronage beneficiaries with arbitrage power (ability to enforce via threat) experience low directed extraction — they capture the benefit. Fiscal officers trapped in enforcement apparatus experience high directed extraction — they are the targets of baronial constraint. The crown's institutional perspective is constrained but not trapped: the crown can adapt by shifting revenue sources or consolidating power, but faces material constraint in handling feudal incidents. The analytical observer risks seeing feudal structure as inevitable natural law ('this is how feudal kings and barons relate') but the structural data reveals contingent bargaining power: absent military defeat at Runnymede, no charter emerges. The false summit detection flags this naturalization.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_functional_duration,
    'Is Magna Carta''s function purely extractive constraint on royal fiscal officers, or does it establish a coordination mechanism that persists beyond the initial 1215 settlement?',
    'Historical analysis of enforcement gaps and reissue patterns: if each reissue requires military threat renewal (extractive model), vs if reissues become routine confirmations (coordination model emerging from extraction)',
    'If purely extractive: classify as snare from crown perspective. If coordination emerges: tangled_rope classification validated. If coordination persists across centuries: the reading foreclosed by common_law_foundation_reading''s claim that the charter seeded permanent constitutional constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_functional_duration, empirical, 'Whether charter establishes persistent coordination or dies with 1215 settlement').

omega_variable(
    feudal_incidents_material_magnitude,
    'How materially significant were reliefs, wardships, and scutage to royal fiscal capacity in 1215? Did their constraint meaningfully reduce crown extraction, or was the constraint primarily symbolic of baronial victory?',
    'Comparative quantification: crown revenue from feudal incidents pre-1215 vs post-charter enforcement periods; correlation with non-feudal revenue sources and their growth trajectory',
    'If high material impact: extraction flows powerfully to beneficiaries, classifying as snare from fiscal officer perspective. If low material impact: charter is performative constraint (piton), defeating tangled_rope classification. If impact decays over time: measurement trajectory shows extractiveness declining as revenue sources diversify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_incidents_material_magnitude, empirical, 'Material magnitude of feudal incidents in crown revenue').

omega_variable(
    reading_kernel_identity_contest,
    'Is this the same kernel (Magna Carta''s legitimacy foundation) as the common_law_foundation_reading, or do the readings refer to incommensurable kernels that happen to share a document label?',
    'Authority genealogy: trace which reissues and confirmations cite which clauses, and which interpretive traditions (feudal lawyers vs common-law judges) treat the charter as authorizing what commitment structure. If feudal tradition treats 1215 as feudal bargain and common-law tradition treats it as constitutional seed, the readings are siloed — they coexist by occupying different interpretive lineages, not because both readings describe the same kernel.',
    'If same kernel: the readings coexist via different framing of identical commitment structure. If different kernels: the contest is one of label-collision rather than true doctrinal conflict — the common_law reading is reading a different charter (the reissued, reinterpreted 13th-14th century artifact) than the feudal bargain reading (the 1215 peace treaty proper).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity_contest, conceptual, 'Whether this reading and common_law_foundation_reading contest the same kernel or inhabit different interpretive lineages').

omega_variable(
    baronial_extraction_reciprocal,
    'Does the charter''s constraint on royal fiscal officers constitute extraction FROM the king BY the baronage, or does it represent a legitimate boundary-setting on feudal incidents that the king had previously exceeded?',
    'Historical analysis of pre-1215 royal exaction practices: were reliefs, wardships, and scutage applied arbitrarily or in violation of feudal custom? If arbitrary exaction: charter is restoration of customary limits, not new extraction. If customary practice: charter imposes new constraint, constituting extraction toward beneficiaries.',
    'If restoration: beneficiary classification weakens; charter becomes coordination mechanism restoring mutual obligation, not extraction mechanism. Classifies as rope, not tangled_rope. If new constraint: extraction classification holds; tangled_rope maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baronial_extraction_reciprocal, empirical, 'Whether charter constrains arbitrary exaction or imposes new obligations on crown').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__feudal_bargain_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc1215fb_tr_t0, magna_carta_1215__feudal_bargain_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mc1215fb_tr_t3, magna_carta_1215__feudal_bargain_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(mc1215fb_tr_t6, magna_carta_1215__feudal_bargain_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(mc1215fb_tr_t9, magna_carta_1215__feudal_bargain_reading, theater_ratio, 9, 0.4).

% Extraction over time
narrative_ontology:measurement(mc1215fb_be_t0, magna_carta_1215__feudal_bargain_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mc1215fb_be_t3, magna_carta_1215__feudal_bargain_reading, base_extractiveness, 3, 0.18).
narrative_ontology:measurement(mc1215fb_be_t6, magna_carta_1215__feudal_bargain_reading, base_extractiveness, 6, 0.08).
narrative_ontology:measurement(mc1215fb_be_t9, magna_carta_1215__feudal_bargain_reading, base_extractiveness, 9, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mc1215fb_su_t0, magna_carta_1215__feudal_bargain_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(mc1215fb_su_t3, magna_carta_1215__feudal_bargain_reading, suppression_requirement, 3, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__feudal_bargain_reading, resource_allocation).
narrative_ontology:affects_constraint(magna_carta_1215__feudal_bargain_reading, magna_carta_1215__common_law_foundation_reading).
narrative_ontology:affects_constraint(magna_carta_1215__feudal_bargain_reading, magna_carta_1215__symbolic_myth_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes Magna Carta into three structurally distinct readings of the same kernel, each with different epsilon values, beneficiary/victim structures, and classification types. The feudal_bargain_reading (ε=0.38, tangled_rope) emphasizes material constraint on feudal fiscal mechanisms. The common_law_foundation_reading emphasizes doctrinal seeding of due process across generations (higher extractiveness, different victim set). The symbolic_myth_reading emphasizes invocation and narrative authority (different epsilon structure reflecting the constraint as performative from inception). All three readings refer to the same historical document; they differ in which commitment structure the document is read as grounding, and therefore differ in what extractiveness and suppression mean in each framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__feudal_bargain_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
