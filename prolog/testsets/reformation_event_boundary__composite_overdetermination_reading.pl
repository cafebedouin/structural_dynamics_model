% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation as Composite Overdetermined Event (Multiple Parallel CS Patterns)
 *   domain: religious/political/historical
 *
 * SUMMARY:
 *   The Reformation enters the historical record as a composite event:
 *   theological innovation (Luther's doctrinal breakthrough on
 *   justification), institutional collapse (the papal monopoly on Christian
 *   authority fragments), political realignment (secular rulers seize church
 *   assets and establish national churches), and cultural emergence (printing
 *   and vernacular literacy create new reading publics). This composite
 *   reading claims that these four processes are IRREDUCIBLY SIMULTANEOUS—no
 *   single causal driver (theology, politics, technology, or economics) fully
 *   explains the phenomenon, and no single periodization scheme (tracking
 *   theological, political, institutional, or cultural completion) captures
 *   the whole. Different historiographical traditions foreground different
 *   axes, producing what appears to be a problem of interpretation but is
 *   actually a structural feature: the Reformation itself is overdetermined.
 *   This constraint story instantiates the composite reading against two
 *   sibling readings: political_swap_reading (politics as the primary driver,
 *   theology as cover) and theological_climb_reading (theology as the driver,
 *   institutional separation as consequence). This reading asserts all three
 *   are partial and codependent.
 *
 * KEY AGENTS:
 *   - papal_institutional_authority: claims unified Christian authority, loses territorial and financial control to secular rulers and reformed churches (power institutional, exit trapped)
 *   - secular_rulers: exploit theological disputes to break papal authority and seize church assets (power institutional, exit mobile, benefit from consolidation)
 *   - reformed_theological_communities: articulate alternative doctrines, gain institutional protection from some rulers (power organized, exit constrained, benefit from doctrinal legitimacy)
 *   - catholic_ecclesiastical_hierarchy: caught between papal headquarters and territorial rulers, loses assets and autonomy (power institutional, exit trapped)
 *   - printing_networks: amplify theological argument into mass medium, benefit from unprecedented market demand (power organized, exit mobile)
 *   - vernacular_reading_publics: gain epistemological access to scripture and theological debate (power powerless, exit constrained, benefit modest but real)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation as Composite Overdetermined Event (Multiple Parallel CS Patterns)").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "religious/political/historical").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '83fc0687-f483-4bd4-8fd2-bf000a62d92e').
narrative_ontology:cs_kernel_codification('83fc0687-f483-4bd4-8fd2-bf000a62d92e', distributed).
narrative_ontology:cs_authority_grounding('83fc0687-f483-4bd4-8fd2-bf000a62d92e', lineage).
narrative_ontology:cs_interpretation_layer_present('83fc0687-f483-4bd4-8fd2-bf000a62d92e').
narrative_ontology:cs_reading_relation('83fc0687-f483-4bd4-8fd2-bf000a62d92e', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('83fc0687-f483-4bd4-8fd2-bf000a62d92e', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_axiom('83fc0687-f483-4bd4-8fd2-bf000a62d92e', foundational, reformation_irreducible_overdetermination).
narrative_ontology:cs_axiom_status(reformation_irreducible_overdetermination, holdable).
narrative_ontology:cs_axiom_grounding('83fc0687-f483-4bd4-8fd2-bf000a62d92e', reformation_irreducible_overdetermination, empirically_contingent).
narrative_ontology:cs_axiom('83fc0687-f483-4bd4-8fd2-bf000a62d92e', foundational, theological_political_institutional_simultaneity).
narrative_ontology:cs_axiom_status(theological_political_institutional_simultaneity, holdable).
narrative_ontology:cs_axiom_grounding('83fc0687-f483-4bd4-8fd2-bf000a62d92e', theological_political_institutional_simultaneity, empirically_contingent).
narrative_ontology:cs_reference_frame('83fc0687-f483-4bd4-8fd2-bf000a62d92e', unified_papal_christendom_framework).
narrative_ontology:cs_drift_state('83fc0687-f483-4bd4-8fd2-bf000a62d92e', post_westphalian_settlement, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('83fc0687-f483-4bd4-8fd2-bf000a62d92e', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, secular_rulers_territorial_consolidation).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, reformed_theological_communities).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, papal_institutional_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, catholic_ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, unified_christendom_ideal).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, printing_technology_operators).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, vernacular_reading_communities).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, theological_pluralism_possible).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, secular_authority_independence_from_religious).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, doctrine_reformation_necessary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Roman See administers Christian doctrinal authority, claims authority over secular rulers, collects tithes and indulgence revenue, and enforces theological orthodoxy through excommunication and inquisition. The institutional structure assumes religious and political authority are unified through papal supremacy. Faces simultaneous challenges: theological questioning of indulgences and papal authority, secular rulers exploiting these disputes to seize church assets and assert independence, and proliferation of competing theological frameworks the See cannot contain.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, papal_institutional_authority, agenda_setter,
    institutional, civilizational, trapped, continental).

% Kings, princes, and dukes exploit theological disputes to break papal financial and political authority over their territories. They seize church lands, redirect tithe revenue to state coffers, establish national churches answerable to secular authority rather than Rome, and use theological legitimacy arguments to justify these seizures. The Reformation provides the institutional cover for territorial consolidation that would be politically illegitimate without theological justification.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, secular_rulers_territorial_consolidation, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, secular_rulers_territorial_consolidation, agenda_setter).

% Theologians, clergy, and educated laity articulate alternative doctrines (justification by faith, priesthood of all believers, scripture as ultimate authority) that break with papal teaching. They gain institutional power through reformed churches, establish theological training that transmits the new reading, and build communities that sustain doctrinal identity independent of Rome. Their benefit is doctrinal innovation legitimated and preserved; their cost is institutional vulnerability (subject to both papal and secular ruler pressure, depending on the ruler's position).
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, reformed_theological_communities, beneficiary,
    organized, generational, constrained, regional).

% Bishops, monasteries, and cathedral chapters lose land, revenue, and institutional autonomy as secular rulers seize church assets and establish state-controlled ecclesiastical structures. The unified institutional hierarchy fractures: some remain in communion with Rome (now as minority in former territories), others align with reformed churches or secular rulers. Their position sits between papal headquarters and territorial rulers; as the Reformation proceeds, they are pressed to choose.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, catholic_ecclesiastical_hierarchy, payer,
    institutional, generational, trapped, continental).

% The medieval framing of unified Christian Europe under papal oversight disintegrates irreversibly. This is not an agent that suffers; it is a normative ideal that loses institutional support and historical plausibility. The ideal persists in Catholic doctrine but cannot be re-achieved; it becomes a counterfactual memory rather than a living framework.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, unified_christendom_ideal, payer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__composite_overdetermination_reading, unified_christendom_ideal).

% Printers, publishers, and bookmakers gain unprecedented market share and profit from printing and distributing Bibles, polemics, and theological works in vernacular languages. The Reformation created demand for reproduced texts; printers supplied and benefited from the amplification of theological argument into a mass medium.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, printing_technology_operators, beneficiary,
    organized, biographical, mobile, continental).

% Lay people gain access to scripture in languages they speak rather than ecclesiastical Latin, enabling direct engagement with theological argument and reduced dependence on clerical interpretation. They become literate audiences for printed theological debate, participate in reformed congregations, and gain participatory voice in doctrinal questions. Their constraint-side benefit is modest (participation remains bounded by reformed church discipline); the broader epistemological benefit (access to written tradition) is real but uneven geographically.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, vernacular_reading_communities, beneficiary,
    powerless, biographical, constrained, regional).

% Different historiographical frameworks (theological periodization tracking doctrinal completion, political periodization tracking territorial consolidation, institutional periodization tracking church reorganization, cultural periodization tracking print and literacy) mark different terminal dates for 'the Reformation.' No single scheme captures all four simultaneous processes. The exclusion of alternative schemes from dominant narratives means the overdetermination itself remains invisible in popularized histories.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historical_periodization_schemes, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__composite_overdetermination_reading, historical_periodization_schemes).

% The Catholic institutional apparatus mounts a coordinated response (Council of Trent, Jesuit educational mobilization, inquisitorial intensification) that attempts to restore unity and suppress reformed alternatives. This response redistributes costs (funding new institutional machinery) and consolidates what remains of papal authority on a smaller territorial base.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, counter_reformation_institutional_response, payer,
    analytical, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__composite_overdetermination_reading, counter_reformation_institutional_response).

% Academic historians investigate the Reformation's causes, structures, and consequences using archival evidence, institutional analysis, and comparative periodization. They produce multiple competing interpretations (theological climbing, political swapping, institutional cascading, cultural emergence) and increasingly recognize that the phenomenon resists monocausal explanation. This reading (composite overdetermination) is the result of this scholarship's inability to choose a single driver.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, modern_historical_scholarship, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates the institutional innovation enabling transition from unified papal authority to pluralistic territorial religious structures: secular rulers needed theological cover for asset seizure; reformed theologians needed political protection for survival; both needed the other. The coordination solves a collective-action problem: no single actor can break papal monopoly alone; united opposition (political, theological, economic) succeeds. Once institutional settlement stabilizes (post-1555), the coordination function atrophies and enforcement machinery becomes performance-based.
% TRANSFER_FUNCTION: Moves ecclesiastical authority, land, tithe revenue from papacy/Catholic hierarchy to: (1) secular rulers (territorial consolidation, ~40% of church lands seized by 1600); (2) reformed churches (doctrinal authority, institutional autonomy); (3) printing networks (market access for theological texts); (4) vernacular reading publics (epistemological access to scripture). The flow is unequal and asymmetric—papacy loses more than any gainer receives, suggesting zero-sum extraction dressed as institutional innovation.
% ABSENT_VOICES: Catholic hierarchy members who might have advocated reform without rupture; theological traditions requiring intact papal institutional framework (certain scholastic syntheses); peasant movements invoking Reformation theology but repressed by both Catholic and reformed authorities once institutional lines hardened; Jewish and Muslim communities whose geopolitical position destabilized as unified Christian authority declined; women reformers whose theological contributions were suppressed by reformed church discipline even as Reformation rhetoric included them.
% DISAPPEARANCE_RATIONALE: If the Reformation constraint vanished (imagine successful early suppression of reform movements), secular rulers would lack theological legitimacy for asset seizure, reformed theology would remain underground, printing would remain a commercial technology without theological demand amplifying it, and unified papal authority would persist longer (though structural pressures toward territorial independence remain). The terminal outcome of secularization and pluralism would require different mechanisms to achieve—mechanisms that are politically costlier and less narratively coherent.
% FOUNDING_PROBLEM: Four irreducible structural problems converged: (1) theological—Pelagian/Augustinian debate resurged; (2) financial—indulgence revenue became unsustainable as secular taxation matured; (3) political—absolute monarchy required independence from supranational religious authority; (4) informational—printing enabled mass-market theological disputation. No single problem could have generated the Reformation alone; all four were necessary.
% FOUNDING_PROBLEM_CORROBORATION: Historians external to benefiting parties (reformed and Catholic institutions)—MacCulloch, Po-chia Hsia, Ocker—attest from archival evidence that multiple drivers operated simultaneously. Theological innovation alone could not have survived without political support; political realignment alone could not have succeeded without theological justification; neither could have achieved mass mobilization without printing; neither could have sustained without emerging vernacular reading publics. The founding problem is not a single failure but a constellation of pressures.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).

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
 *   The extractiveness metric (0.58 at interval end) reflects that the Reformation transfers control and resources from one institutional set (papal/Catholic hierarchy) to another (secular rulers + reformed churches + printing networks), with unevenly distributed costs borne by those losing power. The constraint is extractive because the transfer is asymmetric—the papacy and Catholic hierarchy do not consent to the loss, and suppression is required to hold the new settlement. Suppression (0.62) is high because the Counter-Reformation represents an active institutional response to suppress reformed alternatives and restore papal authority where possible. Theater_ratio (0.41 at interval end) rises significantly during the initial phase (1517–1555, climb from 0.20 to 0.35) as theological argument becomes the public language of political conflict, then stabilizes as institutional settlement solidifies. The stability after 1600 reflects that the constraint's enforcement machinery (territorial churches, printing monopolies, state apparatus) has hardened, and theatrical justification (theological polemics) becomes less necessary—the arrangement persists by institutional inertia. The measurement series tracks ONE shared time grid so all three metrics are authored at every examined point (1450, 1517, 1555, 1600, 1648), enabling the compiler and temporal analysis to read all metrics together without interpolation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint sits at the intersection of four different institutional reference frames, each producing a different classification: (1) From the papal seat (agenda_setter, institutional power), the Reformation is a catastrophic institutional collapse and naked political extraction—computation should yield high χ for the papal axis and a snare or deep tangled_rope classification. (2) From the secular ruler seat (agenda_setter + beneficiary, institutional power), the Reformation is a legitimate political realignment enabling territorial consolidation and is framed as responding to genuine theological problems—computation should yield d toward beneficiary (0.2–0.3) and classification closer to rope or justified tangled_rope. (3) From the reformed theological seat (organized, constrained exit), the Reformation is doctrinal breakthrough enabled by political cover, a genuine innovation requiring institutional separation—computation should yield d toward moderate beneficiary (0.3–0.4) reflecting both genuine benefit and constrained exit. (4) From the vernacular public seat (powerless, constrained exit), the Reformation is epistemological emancipation (access to scripture) mixed with participation in reformed church discipline—computation should yield d symmetric or near beneficiary (0.4–0.5). The composite reading claims all four perspectives are empirically valid simultaneously; the engine's per-seat computation should detect this divergence. Seats holding political power (rulers) will compute differently from seats losing institutional authority (papal hierarchy) and seats gaining doctrinal access (reformed communities) from seats remaining powerless (reading publics).
 *
 * DIRECTIONALITY LOGIC:
 *   The structural beneficiary seats (secular_rulers_territorial_consolidation, reformed_theological_communities, printing_technology_operators, vernacular_reading_communities) are drawn from multiple power levels and exit profiles. Secular rulers have institutional power and mobile exit; reformed theologians have organized power and constrained exit; printing networks have organized power and mobile exit; reading publics have powerless status and constrained exit. The structural victim seats (papal_institutional_authority, catholic_ecclesiastical_hierarchy, unified_christendom_ideal) lose authority, assets, and institutional coherence. The papal seat bears the highest cost and has the most constrained exit (the See cannot abandon Christianity). The Catholic hierarchy is caught between papal headquarters (institutional constraint) and territorial rulers (political pressure). The directionality derivation should produce: papal authority d near 1.0 (full target); secular rulers d near 0.1–0.2 (beneficiary + powerful + mobile); reformed theologians d near 0.3–0.4 (beneficiary + organized + constrained); vernacular publics d near 0.4–0.5 (minor beneficiary + powerless + constrained). No override necessary if beneficiary/victim declarations are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The Reformation constraint carries a genuine coordination problem (institutional innovation required breaking papal monopoly; theology, politics, and economics all pushed in the direction of rupture simultaneously) and genuine asymmetric extraction (papacy loses institutional authority and revenue; secular rulers gain consolidation capability). The mandatrophy question is whether the coordination function persists after 1648 (Peace of Westphalia formalizes territorial churches) or whether the extraction becomes primary once the institutional settlement stabilizes. The measurement trajectory suggests: (1) 1450–1517, pre-Reformation: extractiveness low (0.15), theater minimal (0.08), suppression moderate (0.35)—the medieval papal system is relatively stable and does not yet face coordinated opposition. (2) 1517–1555, active reformation: extractiveness rises sharply (0.35), theater rises (0.20), suppression rises (0.52)—theological argument, institutional conflict, and enforcement machinery all escalate; this is the period of genuine coordination challenge and aggressive institutional competition. (3) 1555–1648, settlement: extractiveness rises to plateau (0.52→0.58), theater climbs further (0.35→0.41), suppression stabilizes (0.68→0.62)—the institutional settlement hardens, territorial churches are established, and the arrangement persists by inertia. The plateau in extractiveness combined with rising theater suggests the constraint's primary function has shifted from solving a coordination problem (1517–1555) to maintaining an institutional settlement (1555–1648) via reduced enforcement but increased narrative/performative work. The theater ratio rise (0.35→0.41 in the settlement phase) is consistent with a transition from extractive energy into institutional maintenance—theological and political argument become the mechanism by which the arrangement justifies its persistence, not the mechanism by which it disciplines dissent. This is NOT mandatrophy yet (the arrangement still extracts), but it is a trajectory toward piton territory: if extractiveness continues to plateau while theater rises further and resistance moderates, the constraint enters piton classification (atrophied coordination function, performance-based persistence). The commentary notes this as a lifecycle pattern rather than a declaration of resolved mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_irreducibility,
    'Are the four sub-events of the Reformation (theological, political, institutional, cultural) truly causally independent—such that removing any one would prevent the whole—or is there a hidden causal ordering that appears simultaneous only because of historical granularity?',
    'Counterfactual analysis: systematic investigation of whether theology could have advanced without political protection, whether political realignment could have succeeded without theological cover, whether printing could have achieved mass market without theological demand, whether vernacular literacy could have spread without reformed church mobilization. Examine historical moments where each process faced resistance to determine if any was the necessary gating condition.',
    'If true independence confirmed, the composite reading holds and classification should reflect the genuine simultaneity of multiple extraction/coordination mechanisms. If one axis proves gating (e.g., printing must precede theology to enable mass distribution of doctrinal argument), then that axis becomes the primary driver and the reading should reclassify toward theological_climb or political_swap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_irreducibility, empirical, 'Whether the four Reformation sub-events are causally irreducible or sequentially ordered.').

omega_variable(
    periodization_as_axiom_choice,
    'The Reformation''s terminal date (1555? 1648? 1700?) depends on which axis one prioritizes. Is the choice of periodization scheme epistemologically neutral (a practical choice of when to stop measuring) or does it smuggle in a commitment about which axis was primary?',
    'Historiographical analysis: examine whether scholars who foreground theology (climb) favor 1555 (Peace of Augsburg, Reformed churches stabilize); whether those foregrounding politics (swap) favor 1648 (Peace of Westphalia, secular sovereignty formalized); whether those foregrounding culture (emergence) favor 1700 or later (printing medium normalizes, literacy becomes baseline). If the choice of date correlates with the author''s axiom about what was primary, then periodization is not neutral.',
    'If periodization choice is axiom-laden, then the composite reading''s claim that ''no single scheme captures the whole'' becomes self-validating—one cannot resolve the meaning of the Reformation event without first declaring which sub-event one considers decisive. The constraint then exhibits irreducible interpretive dependence, suggesting that the Reformation is not a fact about the world that historians discover but a classification of historical materials that depends on the reading framework chosen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(periodization_as_axiom_choice, conceptual, 'Whether periodization schemes are epistemologically neutral or axiom-dependent.').

omega_variable(
    institutional_vs_normative_emergence,
    'Did theological pluralism emerge as an INSTITUTIONAL FACT (because competing churches could not be eliminated and had to be tolerated) or as a NORMATIVE CLAIM (because Reformation theology articulated why pluralism was correct)?',
    'Historical analysis of institutional settlement versus theological vindication: examine whether reformed churches were tolerated because they could not be suppressed (institutional reality forcing normative revision) or because reformed theology successfully argued for pluralism as theologically justified (normative claim driving institutional reality). The two mechanisms produce different beneficiary sets and different victim analyses.',
    'If institutional emergence dominates (tolerance forced by military stalemate and economic cost, not by theological conviction), then the Reformation constraint is closer to snare or extractive political realignment dressed in theological language (supporting political_swap_reading). If normative emergence dominates (pluralism became recognized as a genuine theological possibility), then the theological_climb_reading carries more weight. The composite reading can accommodate both mechanisms in parallel, but understanding the balance between them changes how victim and beneficiary positions are understood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_normative_emergence, empirical, 'Whether theological pluralism emerged as institutional necessity or normative vindication.').

omega_variable(
    reading_committer_axis_ambiguity,
    'This constraint is ONE READING of the reformation_event_boundary kernel—a choice to treat the Reformation as composite overdetermination rather than as a single-axis phenomenon (political or theological). What makes THIS reading more accurate than its siblings? Is the reading motivated by historiographical evidence (the phenomenon genuinely exhibits irreducible simultaneity), or is it motivated by a post-modern epistemological preference (refusing monocausal narratives as a methodological principle)?',
    'Self-examination of the reading''s axiom-status: if the composite claim is an EMPIRICAL CLAIM (multiple axes genuinely operate independently, as measured by the failure of any monocausal model to predict observed variation), then it can be tested against specific historical counterfactuals and should be falsifiable. If the composite claim is a METHODOLOGICAL AXIOM (we refuse monocausal narratives as a principle of historiography), then it is not falsifiable by historical evidence and cannot function as a claim about what happened—it functions only as a rule for how to interpret what happened. The boundary between these is the reading''s own vulnerability.',
    'If the composite reading is empirical, it should be testable against the political_swap and theological_climb readings by examining whether the Reformation''s outcomes are better predicted by all-three-axes than by any one axis alone. If the composite reading is methodological, then it is incommensurable with the sibling readings and cannot be adjudicated by historical evidence—the choice among readings becomes a choice about interpretive framework, not about historical fact. This affects how the constraint enters the final classification system: an empirical reading produces a constraint; a methodological reading produces a hermeneutical position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_axis_ambiguity, preference, 'Whether the composite reading is an empirical claim or a methodological axiom.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'The Counter-Reformation''s suppression of reformed alternatives (inquisition, military campaigns, institutional exile) is STRUCTURAL (external barriers, legal prohibition, force). Does this suppression persist after institutional barriers are removed (post-1648, in regions where reformed churches gain majority), or do the internalized dimensions (reformed theology''s own disciplines, confessional identity boundaries, sectarian separation) become the primary suppression mechanism?',
    'Post-Reformation trajectory analysis: examine regions where reformed churches achieved institutional dominance (Scotland, much of Germany, Scandinavia) to determine whether suppression of competing theology attenuates (suggesting structural suppression is primary) or persists in different forms (suggesting internalized suppression—confessional boundaries, denominational discipline, identity-based exclusion—becomes primary). If suppression persists post-institutional-victory, reclassify as partially internalized.',
    'If structural suppression is primary, the constraint''s high suppression metric reflects enforced institutional settlement, and classification is tangled_rope or snare depending on whether beneficiaries genuinely emerge or are merely victors in a zero-sum conflict. If internalized suppression dominates post-settlement, then the constraint exhibits an additional extraction mechanism (identity-based exclusion) that persists after external barriers are removed, suggesting deeper victim relationships and a snare-leaning classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether Reformation-era suppression is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1450, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1450, 0.08).
narrative_ontology:measurement_basis(refo_tr_t1450, observed).
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1555, 0.35).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement_basis(refo_tr_t1600, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1648, 0.41).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement_basis(refo_be_t1450, observed).
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1555, 0.52).
narrative_ontology:measurement_basis(refo_be_t1555, observed).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1600, 0.58).
narrative_ontology:measurement_basis(refo_be_t1600, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1648, 0.58).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1450, 0.35).
narrative_ontology:measurement_basis(refo_su_t1450, observed).
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1517, 0.52).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1555, 0.68).
narrative_ontology:measurement_basis(refo_su_t1555, observed).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement_basis(refo_su_t1600, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1648, 0.62).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, westphalian_sovereignty_settlement).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, counter_reformation_institutional_response).

% DUAL FORMULATION NOTE:
% This constraint is the composite_overdetermination_reading of the reformation_event_boundary kernel. The sibling readings (political_swap_reading, theological_climb_reading) instantiate the same historical event but foreground different causal axes and produce different victim/beneficiary structures, periodization schemes, and extraction patterns. All three readings are live in historiography; none logically forecloses the others. The composite reading claims that this is not a failure of historical analysis but a structural feature: the Reformation was genuinely overdetermined by multiple simultaneous drivers. See network.affects_constraints for sibling constraint IDs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
