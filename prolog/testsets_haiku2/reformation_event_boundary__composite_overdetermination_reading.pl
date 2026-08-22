% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite_overdetermination, []).

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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation as Composite Overdetermined Event (Historiographical Reading)
 *   domain: historical_epistemology/religious_history/commitment_systems
 *
 * SUMMARY:
 *   This is one reading of a contested kernel: the historical event known as
 *   the Reformation. The composite-overdetermination reading holds that the
 *   Reformation cannot be truthfully narrated as a single causal process
 *   (theological, political, or institutional) because four irreducible
 *   pressures operated simultaneously and each modified the others.
 *   Theological innovation (sola fide, scriptural authority) was
 *   interdependent with political realignment (princes breaking papal power),
 *   which was interdependent with institutional collapse (fragmentation of
 *   the unified Catholic hierarchy), which was interdependent with epistemic
 *   rupture (printing, vernacular literacy, humanist scholarship). No
 *   periodization scheme that privileges one pressure captures the
 *   phenomenon; the composite claim is that overdetermination is a structural
 *   feature, not a historiographical failure.
 *
 * KEY AGENTS:
 *   - Territorial secular rulers (esp. German princes) — primary beneficiaries of political realignment; secondarily aligned with theological innovation
 *   - Papal institutional authority — trapped target; simultaneous theological, political, institutional delegitimation
 *   - Proto-reformed clergy (Luther, Calvin, their networks) — primary innovators of theological claim; secondary beneficiaries of political alignment; identity-locked to doctrine
 *   - Lower-level Catholic clergy — caught in institutional collapse; powerless, constrained, bearing diffuse costs
 *   - Emergent denominational institutions (Lutheran, Reformed, Anabaptist churches) — emergent properties of the composite event, not pre-existing agents
 *   - European literary/printing class — secondary beneficiaries of proliferating institutional demand for textual authority
 *   - Historiographical tradition — observer seat; faces the problem of narrating overdetermination without reducing to one cause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.71).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation as Composite Overdetermined Event (Historiographical Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_systems").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '7bb11f44-901a-4682-aeee-fa4cdc579b91').
narrative_ontology:cs_kernel_codification('7bb11f44-901a-4682-aeee-fa4cdc579b91', distributed).
narrative_ontology:cs_authority_grounding('7bb11f44-901a-4682-aeee-fa4cdc579b91', extraction).
narrative_ontology:cs_interpretation_layer_present('7bb11f44-901a-4682-aeee-fa4cdc579b91').
narrative_ontology:cs_reading_relation('7bb11f44-901a-4682-aeee-fa4cdc579b91', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bb11f44-901a-4682-aeee-fa4cdc579b91', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('7bb11f44-901a-4682-aeee-fa4cdc579b91', foundational, irreducible_simultaneity_of_causal_dimensions).
narrative_ontology:cs_axiom_status(irreducible_simultaneity_of_causal_dimensions, holdable).
narrative_ontology:cs_axiom_grounding('7bb11f44-901a-4682-aeee-fa4cdc579b91', irreducible_simultaneity_of_causal_dimensions, empirically_contingent).
narrative_ontology:cs_axiom('7bb11f44-901a-4682-aeee-fa4cdc579b91', secondary, historiographical_overdetermination_is_structural_not_narrative_failure).
narrative_ontology:cs_axiom_status(historiographical_overdetermination_is_structural_not_narrative_failure, holdable).
narrative_ontology:cs_axiom_grounding('7bb11f44-901a-4682-aeee-fa4cdc579b91', historiographical_overdetermination_is_structural_not_narrative_failure, conventional).
narrative_ontology:cs_reference_frame('7bb11f44-901a-4682-aeee-fa4cdc579b91', medieval_christendom_unified_synthesis).
narrative_ontology:cs_drift_state('7bb11f44-901a-4682-aeee-fa4cdc579b91', post_westphalia_denominational_fragmentation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('7bb11f44-901a-4682-aeee-fa4cdc579b91', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, territorial_secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, proto_reformed_clergy).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, emergent_denominational_institutions).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, papal_institutional_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, lower_clergy_caught_in_collapse).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, religious_dissenters_not_aligned_with_rulers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, european_literary_class).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, proto_reformed_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German and northern European princes seized the opportunity to consolidate power by breaking papal authority, confiscating church lands, and establishing territorial churches subordinate to princely authority. They framed this as response to theological reform demands but directed the institutional collapse to political advantage. The composite reading shows they were neither pure theological innovators nor pure exploiters — they were institutional realigners whose power derived from the simultaneous occurrence of multiple legitimacy crises. They could arbitrage between theological factions, papal weakness, and emerging reformed churches to enhance their territorial sovereignty.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, territorial_secular_rulers, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, territorial_secular_rulers, agenda_setter).

% The papacy faced simultaneous challenges: theological contestation (sola fide, scriptural authority), political delegitimation (princes seizing initiative), institutional defection (clergy defecting to reformed positions), and loss of territorial revenue sources. No single response could address all four pressures at once. The institution was trapped because exit meant cessation of papal authority itself. The composite event was especially devastating because each dimension reinforced the others: political weakness made the papacy unable to enforce theological orthodoxy, theological contestation made the papacy culturally vulnerable to political challenge, institutional defection reduced the papacy's administrative capacity to retain power.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, papal_institutional_authority, payer,
    institutional, civilizational, trapped, universal).

% Theologians, parish priests, and friars who embraced justification by faith alone, scriptural primacy, and clerical marriage faced institutional persecution but also discovered that their theological position aligned with political forces seeking to break papal power. They benefited institutionally from the composite realignment (reformed churches, protection from secular rulers, pulpits for their theology, printing presses publishing their works) but bore the cost of institutional instability, doctrinal contests with each other, and the identity-lock of their theological commitment (retracting the doctrine meant retracting their whole epistemic frame and professional identity). Their role shifted: early period (1517-1550s) as persecuted innovators, mid-period (1555-1598) as institutional beneficiaries backed by princes, late period (1598-1648) as administrators of normalized denominations.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, proto_reformed_clergy, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, proto_reformed_clergy, payer).

% Parish priests, monks, and mendicant friars not aligned with any party — the majority of the clerical workforce — faced institutional collapse as the papal organization fractured, the mendicant orders split, and monastic lands were confiscated. They had no exit: remaining Catholic meant losing status and livelihood as territories reformed; converting to reformed positions meant retracting identity-constituted commitments or risking mob violence or accusation of cowardice. They were the collateral damage of the composite event. Local pressures (peasant revolts, princely mandates, mob religious violence) determined their fate more than their own theological commitments. The constraint was especially extractive for them because it operated simultaneously on all four dimensions: theological uncertainty about which doctrine was correct, institutional uncertainty about which church would survive, political vulnerability to princely mandate, and epistemic confusion about reliable sources of authority.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, lower_clergy_caught_in_collapse, payer,
    powerless, biographical, constrained, local).

% Lutheran, Reformed, and Anabaptist institutional structures crystallized during the event and became self-sustaining. They set their own theological agendas after the initial volatility, established their own authority claims, and competed for territory and souls. The composite reading highlights that these institutions were EMERGENT PROPERTIES of the event, not pre-existing agents — they came into being because multiple causal pressures (theological + political + institutional) intersected. Early in the period they had mobile exit (could be suppressed and disappear); by 1555 (Peace of Augsburg) they acquired territorial establishment and trapped institutional identity. They became extractive institutions themselves (tithing, conformity enforcement, theological monopoly in their territories).
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, emergent_denominational_institutions, agenda_setter,
    institutional, generational, mobile, regional).

% Anabaptists, radical reformers (Zwickau prophets, Münster rebels), spiritualists, and other groups whose theological positions did not align with territorial princes' political interests faced suppression from both Catholic and magisterial Protestant authorities. Their victimhood was structural: the political realignment (princes securing power by backing reformed theology) left them outside the beneficiary coalition. The event sorted them into the payer category because their theological innovation was decoupled from political utility. They were trapped because conversion to magisterial Protestantism meant retracting their radical theology, remaining radical meant persecution from both old and new authorities, and exit (emigration) was dangerous and difficult.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, religious_dissenters_not_aligned_with_rulers, payer,
    powerless, biographical, trapped, local).

% Printers, scholars, humanist intellectuals, and the emergent reading public benefited from massive expansion of vernacular publishing, theological debate, and intellectual freedom from certain forms of monastic gatekeeping. The composite event created demand for printed vernacular scripture, polemical theology, and reformed devotional works. They were secondary beneficiaries — not the primary drivers but positioned to capture rents from the multiplication of competing institutional claims. Each new denomination wanted to control printing of scripture and theology; scholars and printers mediated these efforts. They had mobile exit (could move between princely territories, could switch language communities) which moderates extraction pressure.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, european_literary_class, beneficiary,
    moderate, biographical, mobile, regional).

% Historians, theologians reflecting on the event, and philosophers of history attempting to periodize and narrate the Reformation. They face the irreducible overdetermination: any periodization that privileges one causal dimension (theology, politics, institution, epistemology) excludes lived experience of the others. The composite reading is a historiographical stance about how to honor multiple causal pressures simultaneously without reducing to false necessity or false contingency. They occupy an analytical seat: they do not participate in the extraction, but they have stakes in how the event is narrated (professional reputation, interpretive tradition, epistemological commitments).
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historiographical_consensus_builders, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, emergent_denominational_institutions).
narrative_ontology:fixing_cost_class(reformation_event_boundary__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Multiple irreducible legitimacy crises converged such that solving any one required addressing the others: secular rulers needed theological cover to break papal authority; theological innovators needed institutional independence to sustain their doctrine; institutional collapse created space for both; the printing press and vernacular literacy created epistemic conditions for contestation of authority. The coordination problem was how to reorganize European Christendom when the medieval synthesis (theological, institutional, political, epistemic) became impossible to maintain under conditions of simultaneous pressure.
% TRANSFER_FUNCTION: The Reformation transferred authority claims from papal-universal to denominational-territorial (political transfer), transferred scriptural interpretation from Latin scholasticism to vernacular theology (epistemic transfer), transferred institutional legitimacy from inherited monastic order to reformed doctrine (institutional transfer), and transferred assets from Catholic institutional control to secular princes and reformed churches (economic transfer). The composite reading emphasizes that these four transfers occurred simultaneously and interdependently, not sequentially. Wealth flowed from papal coffers and monastic lands to princely treasuries and reformed church endowments; authority flowed from Rome to denominational centers; epistemic authority flowed from Latin clerical scholars to vernacular theologians and printers.
% ABSENT_VOICES: Women (excluded from clerical voice entirely, though active in reformation households and Anabaptist communities; wrote published testimonies but were not integrated into official theological debates), Islamic intellectual tradition (suppressed by Christian epistemology despite shared mystical and textual concerns with reformed theology), Jewish communities (both benefited from some Protestant attacks on Catholic authority and suffered from some reformed anti-Semitism; their own interpretive tradition was structurally excluded from the debate), peasants (whose revolts were crushed by reformed princes, showing that theological innovation did not extend to social leveling), radical reformers outside the princely/magisterial consensus (Anabaptists, spiritualists, evangelical rationalists), and women mystics and theologians (like Marguerite of Navarre) whose voices were marginalized even when they aligned theologically with reformers.
% DISAPPEARANCE_RATIONALE: The political/institutional reading says: if the Reformation vanished overnight, secular territorial consolidation would have found another pretext to break papal power (the political realignment would have occurred through dynastic wars or other institutional mechanisms; theology was the pretext, not the driver). The theological reading says: if Luther had never lived, Christian theology would still have faced the sola fide question through other reformers (Zwingli, Calvin, or their precursors); theological evolution is independent of individual actors. The institutional reading says: if the medieval papacy had reformed itself effectively around 1400-1450, the institutional collapse might have been avoided (the founding problem is the papacy's failure to adapt). The composite reading says: the specific historical configuration — territorial reformed churches backed by secular princes, organized denominations with lay participation, vernacular scriptural authority, printing-mediated theological dispute — would NOT have emerged from any single causal pressure absent the others; some other reconfiguration would have resulted from some subset of the pressures (princes would have consolidated differently, theology would have evolved differently, literacy would have affected church organization differently). The fact of European Christendom fragmenting is contestable in cause; the specific form of fragmentation (territorial Protestant and Catholic zones, denominational competition, printing-driven theology, lay religious reading) is composite-dependent.
% FOUNDING_PROBLEM: The medieval synthesis — papal authority over all Christendom, sacramental theology administered by the institutional hierarchy, political legitimacy derived from ecclesiastical coronation, epistemic authority localized in Latin-educated clerics — became impossible to maintain because (1) secular rulers consolidated territorial power and needed sovereignty independent of papal approval (political pressure); (2) scriptural scholarship and printing technology made the gaps between biblical text and scholastic interpretation widely visible (epistemic pressure); (3) the institutional church's wealth, corruption, and administrative brittleness generated protest (institutional pressure); (4) theological debate about justification, authority, and the nature of grace intensified and produced sustained doctrinal alternatives (theological pressure). These four pressures operated on different timescales, affected different populations, and generated different coalitions. No single founding problem reduces the others to secondary status — each pressure would have required institutional response even absent the others. The composite founding problem is the structural impossibility of holding all four systems in medieval alignment once all four began shifting.
% FOUNDING_PROBLEM_CORROBORATION: Political historians (e.g., Ranke lineage: Lutz, Ozment) attest secular rulers needed power consolidation independent of Rome and used theological disputes as opportunity. Theological historians (McGrath, Pelikan, Ozment from theological angle) attest the theological questions about justification and authority were substantive, generative, and would have generated institutional challenges even absent political opportunity. Institutional historians (Bossy, Southern, Swanson) attest the papacy's institutional brittleness and corruption created internal vulnerability and generated reform demands from within the church. Social/cultural historians (Peter Burke, Andrew Pettegree, Elizabeth Eisenstein) attest printing technology and literacy created epistemic conditions for contestation and lay theological engagement. NO external party attests a single founding problem as the driver — historiographical tradition identifies different causal pressures as primary depending on disciplinary focus. The composite reading is that overdetermination itself — the impossibility of reducing to one founding problem — is the corroborated fact. What IS universally attested: by 1648, European Christendom was fragmented into denominational zones, scriptural authority had become vernacular and plural, secular princes exercised authority over church matters in their territories, and theological dispute was permanent rather than resolved.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 (1450: pre-Reformation institutional stability, low institutional contestation) to 0.35 (1517: Luther's 95 Theses; theological innovation begins but institutional/political alignment not yet clear) to 0.58 (1555: Peace of Augsburg; political realignment is complete, new denominational institutions are extractive from their own subjects, papal authority is fragmenting) to 0.65 (peak institutional extraction as new churches consolidate their monopolies territorially) to 0.62 (1648: post-Westphalia stabilization; extractiveness plateaus as the new institutional order becomes normalized). Suppression requirement follows a similar arc, reaching peak at 1598 (height of Counter-Reformation suppression and reformed church-state enforcement) and declining slightly by 1648 (new institutional order is accepted enough that coercive overhead can relax). Theater ratio rises steadily from 1450 to 1598 (increasing performative conflict, doctrinal debate, ecclesiastical spectacle) and declines slightly by 1648 (new order is normalized, performance reduces). The composite reading explains this trajectory: early period shows theological ferment (low extraction); mid-period shows institutional collapse and political realignment (rising extraction and suppression as new orders compete and consolidate); late period shows stabilization (extraction stays high but suppression and theater decline as the new order becomes accepted). Single-cause readings cannot explain this multi-modal trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The territorial secular ruler seat sees a political realignment: theology is the pretext, power consolidation is the substance. The theologian seat sees a genuine doctrinal breakthrough: political utility is incidental to theological necessity. The papal seat sees institutional collapse: both theology and politics are secondary to the loss of authority and resources. The lower clergy seat sees catastrophic institutional instability: all three dimensions feel like simultaneous crises with no clear causality. The composite reading honors all four perspectives as partial and true: they are not competing interpretations of the same thing; they are non-overlapping aspects of an irreducibly composite phenomenon. The engine should compute different type classifications per seat: rulers might see this as Rope (cooperative political realignment), theologians as Mountain (theological truth-discovery), papal authority as Snare (trap of power loss), and lower clergy as Piton (institutional inertia turning to collapse). The claim is Tangled Rope because the arrangement enforces multiple incompatible coordination functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers are beneficiaries (d near 0.0): they gain political power, confiscated assets, territorial church control. Proto-reformed clergy are mixed (d near 0.3-0.4): they gain institutional independence and theological platform but lose security and face persecution; identity-locked exit keeps d elevated. Papal authority is full target (d near 1.0): loses power, wealth, institutional control, and faces theological delegitimation. Lower clergy are powerless targets (d near 1.0): caught in institutional collapse with no exit options (constrained). Emergent denominations are ambiguous (d near 0.5): they benefit from institutional consolidation and political backing but face existential vulnerability during the unstable period and must enforce their own orthodoxies once stabilized. The composite reading shows that directionality is not a fixed property but emerges from which sub-event is foregrounded: from the political lens, rulers benefit most (low d); from the theological lens, reformed clergy benefit most (low-to-moderate d); from the institutional lens, emergent denominations benefit (moderate d) and the papacy suffers (high d). No single d-assignment captures all four lenses simultaneously — this is the irreducible overdetermination at the directionality level.
 *
 * MANDATROPHY ANALYSIS:
 *   Does this constraint's original coordinating function persist? The founding problem was structural impossibility of holding medieval synthesis. That problem is DEAD by 1648: the new synthesis (denominational Christianity within sovereign territorial states) is established and (mostly) accepted. Yet the constraint persists: suppression and theater remain high, extraction remains significant. The constraint has become a Piton: the original justification (breaking an impossible medieval synthesis) is no longer operative, yet the new denominational institutions continue to extract (from subjects through tithes and conformity, from states through privileged access to legitimacy). Mandatrophy is partially resolved: the institutional arrangements show signs of theater and inertia rather than genuine coordination. However, the composite reading complicates mandatrophy diagnosis: different sub-events have different founding problems and different mandatrophy trajectories. The theological innovation (sola fide) becomes foundational doctrine rather than temporary reform — its founding problem is LIVE (the question of justification doctrine persists). The political realignment's founding problem (breaking papal universal authority) is DEAD — princes have consolidated power. The institutional collapse's founding problem (medieval synthesis is impossible) is DEAD. The epistemic rupture (printing enables vernacular challenge) is LIVE (the technology persists). The composite reading suggests mandatrophy should be assessed per sub-event: partial mandatrophy on the political and institutional fronts, live function on the theological and epistemic fronts. This explains why Reformation institutions persist despite the original political/institutional founding problems being obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducibility_vs_analytical_reduction,
    'Is the Reformation''s composite structure genuinely irreducible (each dimension would not have produced the observed outcome without the others), or can a single causal mechanism (theological / political / institutional) be identified as dominant with others as secondary?',
    'Counterfactual analysis: remove one dimension and model whether the other three would still produce the observed event. If removal of any dimension significantly alters the outcome, the structure is irreducible; if one dimension is necessary and sufficient, reducibility is supported.',
    'If irreducible, the event requires composite modeling and no single periodization scheme is adequate; if reducible, one reading (theological_climb or political_swap) captures the phenomenon and the composite reading is historiographically useful but not epistemically necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreducibility_vs_analytical_reduction, conceptual, 'Whether the Reformation''s multiple causal pressures are genuinely independent or one is dominant.').

omega_variable(
    overdetermination_as_cover_story,
    'Is the claim of overdetermination itself a cover story that obscures an underlying political realignment, allowing beneficiary parties (secular rulers, reformed institutions) to narrate the event as inevitable and multi-causal rather than as consolidation of power?',
    'Analysis of who benefits from the overdetermination narrative versus single-cause narratives; examination of whether historical actors experienced the event as composite or as primarily one type of change; comparison of political outcomes under overdetermination framing versus political-primacy framing.',
    'If overdetermination is a cover story, the constraint reduces to Snare (pure political extraction dressed as composite inevitability); if genuine, the constraint is Tangled Rope (legitimate multiple coordination problems operating simultaneously).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_as_cover_story, empirical, 'Whether composite narration serves to mask political extraction.').

omega_variable(
    periodization_sovereignty,
    'Does the choice of period endpoints (1450-1517 vs. 1450-1555 vs. 1517-1648) determine which reading the historian will adopt? That is, does periodization choice cause the reading, or does reading choice cause periodization?',
    'Meta-historiographical study of how different historians'' endpoint choices correlate with their causal emphasis; analysis of whether the same span of years supports different readings depending on where historians draw the boundaries.',
    'If periodization determines reading, the composite reading is partially an artifact of boundary selection and historiographical convention rather than a structural property of the event. If reading constrains periodization, the readings are more epistemically robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_sovereignty, conceptual, 'Whether periodization choices drive or reflect historiographical reading selection.').

omega_variable(
    beneficiary_set_variability,
    'Does the set of identified beneficiaries and victims actually vary depending on which sub-event (theological / political / institutional / epistemic) is foregrounded, or is there a consistent beneficiary/victim core that emerges across all four perspectives?',
    'Systematic analysis of beneficiary and victim identification across historical works that emphasize different causal dimensions; mapping of whether secular rulers are beneficiaries in all readings, or only in political readings, etc.',
    'If beneficiary sets are consistent, the constraint''s extraction structure is stable and the composite reading is historiographically convenient but not structurally necessary. If sets vary radically, the readings are genuinely incommensurable and the composite reading is epistemically required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_set_variability, empirical, 'Whether victim and beneficiary identification depends on which causal dimension is foregrounded.').

omega_variable(
    theological_sola_fide_necessity,
    'Would the institutional fragmentation and political realignment have occurred without the specific theological innovation of sola fide (justification by faith alone), or was the theological content incidental to the political and institutional dynamics?',
    'Comparison with other historical moments of institutional fragmentation (Great Schism, various heresies) that did not produce sustained institutional division; analysis of whether Protestant denominations that reject sola fide (e.g., some Anabaptist groups) still emerged as institutional alternatives.',
    'If sola fide was necessary, the theological innovation is structurally necessary to the composite event; if incidental, theology is cover story and political reading captures the phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_sola_fide_necessity, empirical, 'Whether the specific theological content (sola fide) was structurally necessary to the event.').

omega_variable(
    reformation_boundary_artifact,
    'Is ''the Reformation'' itself a historiographical artifact created by choosing to highlight certain events (1517-1648) and ignore or minimize others (late medieval church reform movements, Italian Renaissance, early modern state-building), or is there a natural boundary between the pre-Reformation synthesis and the post-Reformation fragmentation?',
    'Analysis of late medieval reform efforts and their relationship to the 1517+ events; examination of whether a natural discontinuity exists in theological, institutional, political, and epistemic dimensions around 1517, or whether the boundaries are historiographically imposed.',
    'If ''the Reformation'' is largely historiographical artifact, the composite reading''s claim about overdetermination applies to an event category created by historians rather than discovered in history; if natural boundary exists, the event is real even if composite.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformation_boundary_artifact, conceptual, 'Whether ''the Reformation'' is a natural historical boundary or historiographical construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1450, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1450, 0.12).
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1517, 0.22).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1555, 0.41).
narrative_ontology:measurement(refo_tr_t1598, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1598, 0.52).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1648, 0.48).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1450, 0.18).
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1555, 0.58).
narrative_ontology:measurement(refo_be_t1598, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1598, 0.65).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1648, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1450, 0.25).
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1517, 0.42).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1555, 0.68).
narrative_ontology:measurement(refo_su_t1598, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1598, 0.74).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1648, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__composite_overdetermination_reading, 0.14).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, european_nation_state_consolidation).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, printing_technology_epistemic_rupture).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel decomposes into three constraint stories, one per reading. The COMPOSITE_OVERDETERMINATION_READING (this file) claims irreducible simultaneity of four causal dimensions. The THEOLOGICAL_CLIMB_READING argues doctrinal innovation was the driver (Tangled Rope: theology + institutional separation). The POLITICAL_SWAP_READING argues state consolidation was the driver (Snare: political extraction dressed as theology). These three readings are not competing interpretations of a single constraint — they instantiate three different constraints from the same kernel via different committer framings. The composite reading INFLUENCES (not FORECLOSES) the other two: it creates pressure on both the theological and political readings to acknowledge that their dimension did not operate in isolation. But neither reading is logically eliminated by the composite reading — historians can defensibly forefront one dimension even while acknowledging others operated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, powerless, 0.92).
constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
