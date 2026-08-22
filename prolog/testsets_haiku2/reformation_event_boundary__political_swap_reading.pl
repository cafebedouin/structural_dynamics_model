% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Authority Swap and Asset Seizure
 *   domain: historical/political/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   Reformation as a political realignment event (the political-swap
 *   reading). The reading presents the Reformation primarily as a strategic
 *   realignment in which secular territorial rulers exploited theological
 *   disputes to break papal authority, consolidate territorial sovereignty,
 *   and seize ecclesiastical assets. Theology — justification by faith alone,
 *   sola scriptura, the priesthood of all believers — functioned as post-hoc
 *   rationalization and political cover for the extraction of authority and
 *   wealth from Rome and the Church. The constraint runs from the beginning
 *   of the theological dispute (1517: Luther's 95 Theses) through to the
 *   Peace of Westphalia (1648), which codified the political outcome:
 *   territorial sovereignty supersedes religious authority; rulers determine
 *   the religion of their territories; the Pope has no secular authority in
 *   Christian states. This reading does NOT claim theology was unimportant —
 *   it was the vehicle of political transformation — but claims it was not
 *   the primary driver. The primary driver was the material incentive
 *   structure facing territorial rulers: the ability to break papal financial
 *   extraction and consolidate territorial control was worth the
 *   institutional rupture.
 *
 * KEY AGENTS:
 *   - german_princes: Exploited theological disputes to break papal appointment authority and confiscate Church lands (30% of German territory); established territorial religious monopoly; captured ecclesiastical revenue; applied force to suppress resistance.
 *   - english_crown: Used theological dispute (Henry VIII's marriage annulment, then break with Rome) as pretext for comprehensive authority transfer; confiscated monastic lands worth 25% of Crown revenue; installed loyal clergy under royal supremacy; royal agents enforced theological conformity.
 *   - roman_catholic_church: The victim of asset seizure and authority loss; lost direct political control of Northern European territories; lost appointment authority for bishops; lost accumulated monastic lands; structural position dismantled by coordinated extraction from multiple rulers.
 *   - monastic_communities: Powerless victims; faced confiscation, dissolution, forced secularization or relocation; no exit options; absorption into secular order or exile.
 *   - papal_curia: Institutional victim; lost territorial revenues, appointment authority, and legitimacy claim to universal Christian authority; response was defensive (Counter-Reformation, diplomatic campaigns); identity fused with the lost authority claim.
 *   - westphalia_settlement (1648): Codified the political transformation: cuius regio eius religio principle enshrined territorial sovereignty as supreme over religious authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.81).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.76).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Authority Swap and Asset Seizure").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical/political/commitment_systems").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '380aa8a0-414c-43fd-9457-0965803e9790').
narrative_ontology:cs_kernel_codification('380aa8a0-414c-43fd-9457-0965803e9790', fixed_text).
narrative_ontology:cs_authority_grounding('380aa8a0-414c-43fd-9457-0965803e9790', extraction).
narrative_ontology:cs_interpretation_layer_present('380aa8a0-414c-43fd-9457-0965803e9790').
narrative_ontology:cs_reading_relation('380aa8a0-414c-43fd-9457-0965803e9790', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('380aa8a0-414c-43fd-9457-0965803e9790', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('380aa8a0-414c-43fd-9457-0965803e9790', foundational, political_motivation_primacy).
narrative_ontology:cs_axiom_status(political_motivation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('380aa8a0-414c-43fd-9457-0965803e9790', political_motivation_primacy, empirically_contingent).
narrative_ontology:cs_axiom('380aa8a0-414c-43fd-9457-0965803e9790', secondary, theology_instrumental_rationalization).
narrative_ontology:cs_axiom_status(theology_instrumental_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('380aa8a0-414c-43fd-9457-0965803e9790', theology_instrumental_rationalization, empirically_contingent).
narrative_ontology:cs_reference_frame('380aa8a0-414c-43fd-9457-0965803e9790', papal_trans_territorial_authority_framework).
narrative_ontology:cs_drift_state('380aa8a0-414c-43fd-9457-0965803e9790', westphalia_settlement_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('380aa8a0-414c-43fd-9457-0965803e9790', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, german_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, english_crown).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, scandinavian_rulers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, roman_catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, monastic_communities).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, local_clergy_dependent_on_rome).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, local_clergy_dependent_on_rome).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, roman_curia).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_principle).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, sovereign_state_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German territorial princes seized the opportunity presented by theological controversy to break free of Roman financial extraction (indulgence sales, investiture fees, papal appointments). They adopted the reform theology as political cover, confiscated church lands (approximately 30% of the Empire's territory), installed loyal clergy, and consolidated territorial sovereignty. Their structural position shifted from subordinate to Rome to independent agents capable of setting religious policy and capturing ecclesiastical revenue streams.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, german_princes, agenda_setter,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, german_princes, beneficiary).

% The English Crown under Henry VIII used theological dispute as pretext to break papal authority entirely and assert royal supremacy over the Church of England. Confiscated monastic lands worth an estimated 25% of Crown revenue by mid-century. Theology (transubstantiation, clerical celibacy, papal infallibility) became a policy instrument deployed to justify asset seizure and authority consolidation. Royal agents administered religious reform doctrine to cement Crown control of ecclesiastical appointment and property.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, english_crown, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, english_crown, beneficiary).

% Swedish, Danish, and Norwegian monarchs adopted Lutheran theology to centralize authority and capture church revenues, following the German and English pattern. Their theological commitments tracked material incentives: adopting reform doctrine enabled breaking local bishop autonomy, seizing church assets, and consolidating the nation-state apparatus. Their break from Rome was strategic authority consolidation, with theology as the declared justification.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, scandinavian_rulers, beneficiary,
    powerful, generational, mobile, regional).

% The Catholic Church lost direct political authority over the territories of Northern Europe, lost control of ecclesiastical appointment in those territories, and lost the asset base of confiscated monastic lands, tithes from those lands, and investiture fees the territorial rulers no longer paid to Rome. The Church's structural position was dismantled by coordinated extraction from multiple secular rulers exploiting theological controversy as cover. The Church bore the costs: loss of revenue, loss of territorial authority, loss of organizational coherence in the reformed regions.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, roman_catholic_church, payer,
    institutional, civilizational, trapped, continental).

% Monasteries and their communities faced confiscation of accumulated lands, dissolution of their institutional structures, and loss of their role as the principal literate, administrative, and charitable infrastructure in many regions. Monks were either secularized, relocated to the Crown's ecclesiastical appointments, or dispersed. The monasteries were converted to secular functions (administrative buildings, military fortifications, noble estates, schools). Their exit options were non-existent: resistance meant death or exile; compliance meant absorption into the new secular order.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, monastic_communities, payer,
    powerless, biographical, trapped, local).

% Parish priests and lower clergy faced a forced choice: accept the new theological doctrine and swear allegiance to the territorial ruler (gaining continued stipend and position under new authority) or refuse and lose livelihood, face exile, or risk violent suppression. Those who conformed were repaid with continued benefices but under territorial rather than papal authority. Those who resisted lost their positions. The lower clergy's exit was constrained to conformity or exclusion.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, local_clergy_dependent_on_rome, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, local_clergy_dependent_on_rome, beneficiary).

% The papal administrative apparatus lost the territorial revenues that funded its operations, the appointment authority that supplied its personnel, and the institutional prestige that sustained its legitimacy claim to universal Christian authority. The Curia remained structurally committed to recovering lost territory and authority but lacked the power to do so unilaterally. Their response (Counter-Reformation, Council of Trent, diplomatic campaigns) was defensive, not generative. The Curia's identity was fused with the claim to Christendom-wide authority; that claim was now violently contested and partially superseded by territorial sovereignty.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, roman_curia, payer,
    institutional, civilizational, identity_locked, continental).

% The doctrinal claims (justification by faith alone, sola scriptura, priesthood of all believers, vernacular scripture) were vindicated in the sense that they gained institutional adoption, spread through the adopted territories, and were embedded in new church structures. But the doctrine's vindication was contingent on rulers' political needs: doctrine spreads where rulers permit it, doctrine is suppressed where rulers prohibit it, doctrine is amended where rulers demand it. The theological innovation rode the political wave rather than driving it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, theological_reform_advocates, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__political_swap_reading, theological_reform_advocates).

% The 1648 Peace of Westphalia codified the political outcome of the Reformation: territorial sovereignty trumps religious authority; rulers determine the religion of their territories (cuius regio eius religio); the pope has no secular authority in Christian states. The architects of Westphalia were finalizing a political transformation already complete by that date. Theology was a tool in the argument; territorial authority was the prize.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, westphalia_settlement_architects, agenda_setter,
    institutional, generational, analytical, continental).

% The theological-climb reading — that the Reformation was primarily a doctrinal innovation event whose genuine theological discoveries compelled institutional separation — is excluded from this story's frame. That reading would be articulated by theologians, Church historians emphasizing Luther's intellectual breakthrough, and denominational narratives of the mainline Protestant churches. This reading's frame excludes that interpretation because its structural premise is incompatible with the political-swap framing.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, historical_orthodox_interpretation, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__political_swap_reading, historical_orthodox_interpretation).

% The composite-overdetermination reading — that theological innovation, institutional collapse, political realignment, and denominational emergence occurred simultaneously and are irreducibly joint — is excluded from this story's frame. That reading would emphasize the inextricability of causation and resist periodization schemes. This reading's frame excludes that interpretation because it denies the causal priority of political motivation that anchors this reading.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, overdetermination_framework, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__political_swap_reading, overdetermination_framework).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, german_princes).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralized Christian authority (Rome) had provided a trans-territorial institutional coordination mechanism for ecclesiastical governance, appointment of bishops, canonical law, and capital flow. The coordination problem it solved was: how does a spatially fragmented institutional network maintain doctrinal coherence, settle disputes, and channel resources? Territorial rulers broke that coordination system and replaced it with territorial control of religion — trading trans-territorial coordination for sovereignty.
% TRANSFER_FUNCTION: Moved ecclesiastical authority and revenue streams from Rome to territorial rulers. Specific transfers: (1) confiscated monastic lands (approximately 20-40% of arable land in Northern Europe depending on region); (2) diverted investiture fees, indulgence revenues, tithes, and annates from Rome to territorial treasuries; (3) transferred appointment authority for bishops and major clergy from the Pope to the territorial ruler; (4) captured the intellectual and administrative resources of the Church for state consolidation.
% ABSENT_VOICES: The peasantry (whose revolt in 1524-26 was brutally suppressed by the same princes who adopted reform theology), lower clergy whose autonomy was eliminated by territorial control, monastic communities facing dissolution, papal partisans in the reformed territories who could not speak openly without risk, and the overwhelming majority of lay Christians who did not participate in theological or political dispute. These voices are structurally excluded from the event-boundary because the event was negotiated by princes, theologians, and Rome. The peasants' demand for social reform was co-opted, then abandoned once rulers had consolidated power.
% DISAPPEARANCE_RATIONALE: If the political realignment had not occurred — if rulers had not seized the opportunity of theological dispute to break papal authority and confiscate church assets — the entire structure of European sovereignty would have evolved differently. Rome would have retained financial and appointment authority in Northern Europe; territorial rulers would have remained subordinate to Rome in ecclesiastical matters; the nation-state system would have consolidated more slowly and under different institutional constraints; the accumulation of capital by territorial rulers would have followed a different trajectory. The subsequent history of European political authority, sovereignty doctrine, the rise of state centralization, and the separation of ecclesiastical and secular authority would not have occurred in the form they did.
% FOUNDING_PROBLEM: Territorial rulers faced two intersecting problems: (1) financial extraction by Rome through investiture fees, indulgences, and annates drained revenue that could be captured domestically; (2) appointment authority remaining in Rome meant that bishops were answerable to the Pope, not the territorial ruler, which limited territorial control of the Church's wealth, land, and institutional capacity. Theological dispute provided a vehicle for breaking papal authority without frontal military attack (which would have been prohibitively costly).
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Landes, McNeill, Acemoglu & Robinson) document the fiscal extraction mechanisms Rome operated and the financial gains rulers accrued from confiscation. Political historians (Parker, Brady, Dickens) document the correlation between theological adoption and territorial sovereignty consolidation. Contemporary sources (princely correspondence, administrative records) show rulers adopting theology after confiscation commenced, not preceding it. This corroboration comes from outside the benefiting parties: academic historians with no stake in the rulers' legitimacy — though historians sympathetic to theological explanations (Oberman, McGrath) interpret the same evidence differently.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.81 at period end) reflects the magnitude of asset transfer and authority consolidation: approximately 20-40% of Northern European arable land confiscated, ecclesiastical appointment authority transferred to territorial rulers, tithes and fees redirected from Rome to territorial treasuries, and the institutional independence of the Church eliminated in those territories. Suppression reaches 0.76 because the political realignment required active enforcement: suppression of peasant rebellion (1524-26) that threatened elite consolidation, enforcement of religious conformity on the lower clergy, elimination of the Church's institutional autonomy, and diplomatic/military pressure against papal counter-efforts. Theater ratio is high (0.62) and stable after 1555 because theological doctrine and institutional reform became increasingly performative after the initial asset seizure and authority transfer were complete — doctrinal disputes continued in a terrain already settled by power, not the reverse. Accessibility collapse is moderate (0.48) because alternatives to territorial religious authority existed (papal authority, local bishopric autonomy, lay religious communities) but became progressively unavailable as rulers consolidated control; alternatives did not collapse completely because resistance persisted (religious wars, rebellions, theological contestation). Resistance is substantial (0.71) because the Catholic Church, papal partisans, monastic communities, and elements of the lower clergy mounted active resistance; the Thirty Years' War (1618-48) was partly an expression of this resistance; Counter-Reformation efforts by the Church were defensive resistance to the political realignment.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute radically different constraint types from this structural data. From the ruler seats (german_princes, english_crown, scandinavian_rulers), the constraint emerges as a tangled_rope or even a rope — coordination of religious authority under territorial control, plus extractive benefit. From the Catholic Church and victim seats (monastic_communities, local_clergy_dependent_on_rome), the constraint is a snare: asset seizure, authority elimination, suppression of resistance, no coordination benefit, pure extraction. The engine's per-seat computation should reveal this divergence: high directionality toward targets (victims near 1.0), low directionality toward beneficiaries (beneficiaries near 0.0), resulting in drastically different effective extraction across seats and different type classifications. This perspectival gap is exactly what the framework is built to measure — different seats see structurally different constraints from the same rule.
 *
 * DIRECTIONALITY LOGIC:
 *   German princes, English Crown, and Scandinavian rulers occupy beneficiary seats with powerful institutional position, arbitrage/mobile exit options, and continental/regional scope. They benefit directly from confiscated assets and consolidated authority. Their directionality is near 0.0 (full beneficiary). The Roman Catholic Church occupies a victim seat with institutional power but identity-locked exit (the Church's identity is its claim to authority; losing that claim means losing what it means to be the Church). The Church has trapped exit in this reading's frame — it cannot simply leave Christendom or relinquish its claim to universal authority without ceasing to exist as an institution. Its directionality is near 1.0 (full target). Monastic communities and lower clergy occupy victim seats with powerless/moderate power and trapped/constrained exit. They cannot exit the territorial structure; they cannot exit the requirement to conform or face dispossession. Their directionality is also high (0.8-0.95). Device users and local populations occupy seats with diffuse costs (religious coercion, confiscation filtering into local economy through reduced charitable services) but some coordination benefit (territorial religious authority provides stability, reduces inter-territorial warfare). Their directionality is roughly symmetric (0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's classification as tangled_rope (rather than snare) rests on the presence of a genuine coordination function: moving from trans-territorial papal authority to territorial religious authority does solve a real coordination problem (how to organize religious authority in a now-sovereign territorial system). The problem is that the coordination function is achieved THROUGH asymmetric extraction. The rulers get authority and assets; the Church loses both; the victims have no choice. The mandate to coordinate religious authority is live in 1648 — it is still necessary that someone determine what the territorial religion is — but the mandate no longer serves the Church or the victims; it serves the rulers. A pure snare would have no coordination function at all, merely extractive cover. A tangled rope has a real coordination function that is also asymmetrically extractive. This story is the latter: the coordination problem is real, the extraction is asymmetric and suppressed, and active enforcement maintains both. If the coordination mandate were abandoned (if territories no longer needed religious authority coordination), the constraint might become snare-only. But the founding problem (centralizing authority in fragmented territories) remains live, so the extraction persists coupled to coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_vs_pretext_boundary,
    'Was theological doctrine the primary driver of the Reformation, or was it post-hoc rationalization deployed by rulers seeking to break papal authority and seize Church assets?',
    'Chronological analysis of doctrine adoption vs. asset confiscation and authority consolidation; contemporaneous correspondence of rulers explaining their motivation; sequencing of theological adoption relative to financial incentives. If asset seizure and authority consolidation preceded doctrine adoption consistently, doctrine is post-hoc. If doctrine adoption preceded material incentives and drove them, doctrine is primary.',
    'If doctrine was post-hoc rationalization, this reading (political-swap, tangled_rope with theology as scaffold) is structurally true. If doctrine was primary driver, the theological-climb reading would be structurally true, and this reading would be inverted: theology would be the beneficiary and politics would be the enabling vehicle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_pretext_boundary, empirical, 'Whether theological doctrine was the primary driver or post-hoc cover for political realignment.').

omega_variable(
    papal_authority_loss_mechanism,
    'Did territorial rulers break papal authority because theology provided cover for what would otherwise be indefensible (pure extraction), or did theology genuinely convince people that papal authority was illegitimate?',
    'Analysis of the proportion of population that adopted reformed theology voluntarily vs. coercively; examination of whether theological adoption preceded or followed ruler enforcement; evidence of theological persuasiveness independent of coercive backing; contemporaneous accounts of lay belief.',
    'If theological persuasion was primary, the constraint''s suppression might be lower than measured (people believed; they were not primarily coerced). If theology was primarily a cover for coercion, suppression is primary and high, as measured. This affects whether the constraint is better classified as rope (with genuine coordination belief) or snare (with coercive cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_loss_mechanism, empirical, 'Whether theological persuasion or coercive authority-transfer drove the break with Rome.').

omega_variable(
    sibling_reading_foreclosure,
    'Is the political-swap reading logically compatible with the theological-climb reading, or do they foreclose each other?',
    'Examine whether a single committed actor could hold both: (1) theology was the primary driver AND (2) politics was the primary driver. If they mutually entail their opposite (theology-primary forecloses politics-primary in any framework), they foreclose. If both could be true simultaneously from different angles (theology motivated some actors; politics motivated others), they coexist.',
    'The reading_relations declaration determines this: if foreclosed, the engine routes the constraint toward mutual exclusivity testing; if coexists_with, the engine treats them as competing but live positions held by different academic and denominational communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the political-swap and theological-climb readings logically foreclose each other or coexist as live competing positions.').

omega_variable(
    monastic_community_agency,
    'Were monastic communities and lower clergy primarily victims of confiscation and forced conformity, or did they possess agency in choosing conformity and benefiting from territorial religious authority consolidation?',
    'Analysis of documented resistance and compliance rates; evidence of voluntary adoption of reformed theology vs. coerced adoption; monastic land seizure outcomes (were monks offered benefices in territorial structures; did they actively resist or passively accept). If resistance was systematic and suppressed, victimhood is primary. If adoption was rapid and voluntary among lower clergy, agency is primary.',
    'If victimhood is primary, the payer role is correctly assigned and the constraint shows asymmetric extraction. If agency is primary, some portion of the lower clergy might be reclassified as beneficiaries or symmetric participants, reducing measured victimhood and asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monastic_community_agency, empirical, 'Whether monastic communities were primarily victims or possessed agency in choosing conformity and benefiting from territorial consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1500, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1500, reformation_event_boundary__political_swap_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement_basis(refo_tr_t1500, observed).
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.35).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__political_swap_reading, theater_ratio, 1530, 0.52).
narrative_ontology:measurement_basis(refo_tr_t1530, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__political_swap_reading, theater_ratio, 1555, 0.61).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__political_swap_reading, theater_ratio, 1600, 0.62).
narrative_ontology:measurement_basis(refo_tr_t1600, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.62).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1500, reformation_event_boundary__political_swap_reading, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement_basis(refo_be_t1500, observed).
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__political_swap_reading, base_extractiveness, 1530, 0.45).
narrative_ontology:measurement_basis(refo_be_t1530, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__political_swap_reading, base_extractiveness, 1555, 0.68).
narrative_ontology:measurement_basis(refo_be_t1555, observed).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__political_swap_reading, base_extractiveness, 1600, 0.78).
narrative_ontology:measurement_basis(refo_be_t1600, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.81).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1500, reformation_event_boundary__political_swap_reading, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement_basis(refo_su_t1500, observed).
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__political_swap_reading, suppression_requirement, 1530, 0.48).
narrative_ontology:measurement_basis(refo_su_t1530, observed).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__political_swap_reading, suppression_requirement, 1555, 0.64).
narrative_ontology:measurement_basis(refo_su_t1555, observed).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__political_swap_reading, suppression_requirement, 1600, 0.72).
narrative_ontology:measurement_basis(refo_su_t1600, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.76).
narrative_ontology:measurement_basis(refo_su_t1648, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1500, tn=1648
narrative_ontology:measurement(refo_grid_01, reformation_event_boundary__political_swap_reading, accessibility_collapse(class), 1500, 0.05).
narrative_ontology:measurement(refo_grid_02, reformation_event_boundary__political_swap_reading, accessibility_collapse(class), 1648, 0.45).
narrative_ontology:measurement(refo_grid_03, reformation_event_boundary__political_swap_reading, accessibility_collapse(individual), 1500, 0.02).
narrative_ontology:measurement(refo_grid_04, reformation_event_boundary__political_swap_reading, accessibility_collapse(individual), 1648, 0.38).
narrative_ontology:measurement(refo_grid_05, reformation_event_boundary__political_swap_reading, accessibility_collapse(organizational), 1500, 0.08).
narrative_ontology:measurement(refo_grid_06, reformation_event_boundary__political_swap_reading, accessibility_collapse(organizational), 1648, 0.62).
narrative_ontology:measurement(refo_grid_07, reformation_event_boundary__political_swap_reading, accessibility_collapse(structural), 1500, 0.15).
narrative_ontology:measurement(refo_grid_08, reformation_event_boundary__political_swap_reading, accessibility_collapse(structural), 1648, 0.48).
narrative_ontology:measurement(refo_grid_09, reformation_event_boundary__political_swap_reading, resistance(class), 1500, 0.0).
narrative_ontology:measurement(refo_grid_10, reformation_event_boundary__political_swap_reading, resistance(class), 1648, 0.52).
narrative_ontology:measurement(refo_grid_11, reformation_event_boundary__political_swap_reading, resistance(individual), 1500, 0.0).
narrative_ontology:measurement(refo_grid_12, reformation_event_boundary__political_swap_reading, resistance(individual), 1648, 0.38).
narrative_ontology:measurement(refo_grid_13, reformation_event_boundary__political_swap_reading, resistance(organizational), 1500, 0.0).
narrative_ontology:measurement(refo_grid_14, reformation_event_boundary__political_swap_reading, resistance(organizational), 1648, 0.64).
narrative_ontology:measurement(refo_grid_15, reformation_event_boundary__political_swap_reading, resistance(structural), 1500, 0.0).
narrative_ontology:measurement(refo_grid_16, reformation_event_boundary__political_swap_reading, resistance(structural), 1648, 0.71).
narrative_ontology:measurement(refo_grid_17, reformation_event_boundary__political_swap_reading, stakes_inflation(class), 1500, 0.0).
narrative_ontology:measurement(refo_grid_18, reformation_event_boundary__political_swap_reading, stakes_inflation(class), 1648, 0.54).
narrative_ontology:measurement(refo_grid_19, reformation_event_boundary__political_swap_reading, stakes_inflation(individual), 1500, 0.0).
narrative_ontology:measurement(refo_grid_20, reformation_event_boundary__political_swap_reading, stakes_inflation(individual), 1648, 0.42).
narrative_ontology:measurement(refo_grid_21, reformation_event_boundary__political_swap_reading, stakes_inflation(organizational), 1500, 0.0).
narrative_ontology:measurement(refo_grid_22, reformation_event_boundary__political_swap_reading, stakes_inflation(organizational), 1648, 0.78).
narrative_ontology:measurement(refo_grid_23, reformation_event_boundary__political_swap_reading, stakes_inflation(structural), 1500, 0.0).
narrative_ontology:measurement(refo_grid_24, reformation_event_boundary__political_swap_reading, stakes_inflation(structural), 1648, 0.81).
narrative_ontology:measurement(refo_grid_25, reformation_event_boundary__political_swap_reading, suppression(class), 1500, 0.0).
narrative_ontology:measurement(refo_grid_26, reformation_event_boundary__political_swap_reading, suppression(class), 1648, 0.68).
narrative_ontology:measurement(refo_grid_27, reformation_event_boundary__political_swap_reading, suppression(individual), 1500, 0.0).
narrative_ontology:measurement(refo_grid_28, reformation_event_boundary__political_swap_reading, suppression(individual), 1648, 0.58).
narrative_ontology:measurement(refo_grid_29, reformation_event_boundary__political_swap_reading, suppression(organizational), 1500, 0.0).
narrative_ontology:measurement(refo_grid_30, reformation_event_boundary__political_swap_reading, suppression(organizational), 1648, 0.82).
narrative_ontology:measurement(refo_grid_31, reformation_event_boundary__political_swap_reading, suppression(structural), 1500, 0.0).
narrative_ontology:measurement(refo_grid_32, reformation_event_boundary__political_swap_reading, suppression(structural), 1648, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__political_swap_reading, 0.18).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_principle).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, westphalia_peace_territorial_sovereignty_codification).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel is instantiated by three structurally distinct constraint stories: (1) political_swap_reading (this story) — Reformation as rulers' strategic authority-capture; theology as post-hoc cover; Church as victim; (2) theological_climb_reading — Reformation as doctrinal breakthrough; institutional separation as consequence of genuine theological innovation; theology as primary driver; (3) composite_overdetermination_reading — theological innovation, institutional collapse, political realignment, and denominational emergence as simultaneous and irreducibly joint; no causal hierarchy. Each story has different ε, different beneficiary/victim structures, and different type classifications. The political-swap reading shows high asymmetric extraction (0.81) with identified victims and beneficiaries. The theological-climb reading would show lower extraction and reframe the Church as eventually vindicated by doctrinal truth. The overdetermination reading would resist the causal ordering this reading asserts. These are not perspectives on the same constraint; they are three distinct constraints that share a kernel (the Reformation as historical event) and compete for causal explanation. Link via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, institutional, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
