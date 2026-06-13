% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Authority Swap (Secular Rulers vs. Papal Church)
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   This constraint models the Reformation as a political realignment event
 *   in which secular rulers (German princes, English monarchy, Scandinavian
 *   kings) instrumentalized theological grievances to break papal authority,
 *   establish state-sponsored churches, and seize church property and
 *   revenues. The reading frames theology as post-hoc rationalization for
 *   extraction: the political agenda precedes the theological innovation, and
 *   the theological frameworks (justification by faith, clerical marriage,
 *   vernacular scripture, rejection of papal supremacy) are selectively
 *   adopted and enforced to the extent they serve rulers' consolidation of
 *   territorial power under state monopoly. The constraint runs from ~1500
 *   (before Luther, as principled grievance against Rome crystallized)
 *   through 1648 (Peace of Westphalia, which institutionalizes cuius regio
 *   eius religio and fixes the political settlement). Under this reading, the
 *   Catholic Church is the victim of systematic asset confiscation; secular
 *   rulers and reformed state churches are the beneficiaries; religious
 *   minorities and peasants are collateral enforcement targets; and theology
 *   is the scaffold upon which power consolidation is built. This is ONE
 *   READING of a contested kernel (reformation_event_boundary); sibling
 *   readings (theological_climb_reading, composite_overdetermination_reading)
 *   are separate constraint stories with different ε values, beneficiary
 *   structures, and causal framings.
 *
 * KEY AGENTS:
 *   - Secular rulers (agenda_setters): German princes, English monarchy, Scandinavian kings—set the agenda for reformation and enforce the new arrangement through state power; beneficiaries of asset seizure and religious monopoly
 *   - Reformed state churches (beneficiaries): Lutheran, Reformed, Anglican institutions—vehicles of state consolidation; legitimize princely power; themselves extractive but derivative from secular authority
 *   - Catholic Church (victim/payer): Rome and the institution—loses territorial authority, property, revenue, and universal jurisdiction claims; structurally constrained response through Counter-Reformation and defensive consolidation
 *   - Protestant theologians (beneficiaries with constraints): Luther, Calvin, Zwingli—achieve unprecedented influence but are instrumentalized for political ends; identity-locked to the movement; face suppression if theology diverges from ruler preferences
 *   - Religious minorities (payer/enforcement targets): Anabaptists, radical reformers, Jews, Muslims—face expulsion, forced conversion, confiscation, execution; demonstrate through their suppression that the new arrangement is enforced
 *   - Peasantry (payer): lose common lands from confiscated monasteries, face increased feudal obligations, religious coercion, violent suppression of uprisings; bears material costs while receiving theology of liberation
 *   - Papal Curia (excluded): Rome's reactive Counter-Reformation response arrives after political fait accompli; excluded from agenda-setting; institutional voice overridden by emerging state-sovereignty norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.71).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Authority Swap (Secular Rulers vs. Papal Church)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '56eac93c-c97f-4f79-b693-90cfb52d6dfe').
narrative_ontology:cs_kernel_codification('56eac93c-c97f-4f79-b693-90cfb52d6dfe', fixed_text).
narrative_ontology:cs_authority_grounding('56eac93c-c97f-4f79-b693-90cfb52d6dfe', extraction).
narrative_ontology:cs_interpretation_layer_present('56eac93c-c97f-4f79-b693-90cfb52d6dfe').
narrative_ontology:cs_reading_relation('56eac93c-c97f-4f79-b693-90cfb52d6dfe', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('56eac93c-c97f-4f79-b693-90cfb52d6dfe', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('56eac93c-c97f-4f79-b693-90cfb52d6dfe', foundational, political_primacy_over_theological_causation).
narrative_ontology:cs_axiom_status(political_primacy_over_theological_causation, holdable).
narrative_ontology:cs_axiom_grounding('56eac93c-c97f-4f79-b693-90cfb52d6dfe', political_primacy_over_theological_causation, empirically_contingent).
narrative_ontology:cs_axiom('56eac93c-c97f-4f79-b693-90cfb52d6dfe', secondary, theology_as_post_hoc_legitimation).
narrative_ontology:cs_axiom_status(theology_as_post_hoc_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('56eac93c-c97f-4f79-b693-90cfb52d6dfe', theology_as_post_hoc_legitimation, empirically_contingent).
narrative_ontology:cs_reference_frame('56eac93c-c97f-4f79-b693-90cfb52d6dfe', papal_universal_authority_and_property_control).
narrative_ontology:cs_drift_state('56eac93c-c97f-4f79-b693-90cfb52d6dfe', post_westphalia_territorial_sovereignty, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('56eac93c-c97f-4f79-b693-90cfb52d6dfe', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, reformed_state_churches).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, religious_minorities).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_theologians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, european_merchants_bankers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, protestant_theologians).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, cuius_regio_eius_religio_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, state_sovereignty_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The German princes, English monarchy, Scandinavian kings, and other territorial rulers set the agenda for reformation by selectively adopting theological grievances that justified breaking papal authority and seizing church lands and revenues. They frame the rupture as doctrinal necessity (justification by faith, clerical marriage, vernacular liturgy) while executing a coordinated asset transfer from Rome to princely coffers. They actively enforce the new arrangement through state authority—forbidding Catholic practice, confiscating monastery holdings, establishing state-sponsored churches, and employing violence against resistance.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_rulers, agenda_setter,
    institutional, generational, arbitrage, continental).

% Lutheran, Reformed, and Anglican state churches become the institutional vehicles through which secular rulers consolidate authority. These churches benefit from state protection, establishment funding, and monopoly privilege in their territories. In exchange, they legitimize princely power through doctrinal alignment and pulpit support. They are not independent agents but extensions of state policy, despite retaining theological vocabulary and clerical hierarchy.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, reformed_state_churches, beneficiary,
    institutional, generational, mobile, continental).

% The Catholic Church loses territorial authority, property holdings (monasteries, convents, priories, episcopal lands representing ~25-30% of arable land in many regions), revenue streams (tithes, indulgences, donations), and the ability to enforce canon law in defecting territories. Rome's claims to universal jurisdiction are repudiated by force. The Church survives but restructures defensively (Counter-Reformation) in remaining Catholic territories and overseas missionary expansion, ceding European political primacy to secular rulers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, civilizational, constrained, continental).

% Figures like Luther, Calvin, Zwingli, and their intellectual networks achieve unprecedented influence: their theological frameworks become state doctrine, their writings are printed and disseminated at scale, their institutional positions are secured by princely patronage. However, their theological agenda is instrumentalized for political ends they did not fully control. They must continuously justify doctrinal innovation in political language; those whose theology diverges from ruler preferences (radical reformers, Anabaptists, spiritualists) face suppression alongside Catholics. Their intellectual freedom is conditional on political utility.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, protestant_theologians, beneficiary,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, protestant_theologians, payer).

% Anabaptists, radical reformers, Jews, Muslims, and dissident Catholics become the enforcement targets through which princes and state churches demonstrate coercive power. They face expulsion, forced conversion, property confiscation, and execution. The theological language of 'error' and 'heresy' provides the moral cover for suppression that serves the political function of establishing uniformity under state authority. Their persecution is not incidental to the Reformation but structural to it—proof that the new arrangement is enforced.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, religious_minorities, payer,
    powerless, biographical, trapped, local).

% Rural commoners bear the material costs of the Reformation: loss of common lands confiscated from dissolved monasteries, increased feudal obligations as princes consolidate holdings, religious coercion (forced adoption of reformed practice), and violent suppression of peasant uprisings (Peasants' War, 1524–1526) that rulers brutally crush while invoking Luther's authority. The theological message of Christian liberty circulates while peasant freedoms contract.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, peasantry, payer,
    powerless, biographical, trapped, local).

% Rome's institutional response—the Counter-Reformation, Council of Trent, Inquisition intensification—is reactive and arrives after the political fait accompli. The Curia defends Catholic doctrine and authority vigorously but cannot recover the territorial and temporal power ceded to secular rulers. Its voice is excluded from the agenda-setting that determines which rulers adopt reform and which remain Catholic; the decision is made by princes, not by theological persuasion. The Curia's appeals to universal Christian unity are overridden by the emerging norm of state sovereignty.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_curia, excluded,
    institutional, civilizational, trapped, continental).

% Commercial elites and banking houses (Fuggers, Medici, Hanseatic merchants) benefit from the redistribution of church wealth into circulation, new markets created by confiscated monastic lands and properties, reduced papal fiscal demands that had extracted coin for indulgences and Rome, and the political stability that consolidated secular authority provides for long-distance trade networks. They are not active agenda-setters but material beneficiaries who support princely reformation financially and profit from the asset transfer.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, european_merchants_bankers, beneficiary,
    powerful, generational, mobile, continental).

% Historians, theologians, and analysts examine whether the Reformation was primarily a political realignment (this reading) or a theological innovation (sibling reading) or an overdetermined composite of multiple irreducible drivers. They assess evidence about ruler motivations, asset seizures, timeline alignment with political disputes, theological argument frequency in primary sources, and institutional outcomes to adjudicate competing causal narratives.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the coordination function is explicitly political: Reformation doctrine (justification by faith, clerical marriage, vernacular scripture, two-sacrament theology) provides a legitimating vocabulary through which secular rulers can repudiate papal authority, consolidate territorial control under state churches, establish uniform religious practice as a state monopoly, and justify the seizure of church property. The theology solves the ruler's coordination problem: how to break Rome's authority and retain legitimacy while doing so. Coordination is not genuine—it is extraction dressed in theological language.
% TRANSFER_FUNCTION: Moves church assets (land, buildings, revenues, tithes, patronage rights representing ~25-30% of arable land and a major portion of annual revenue flows in Protestant regions) from the Catholic Church to secular rulers and their allies. Moves religious authority from Rome to territorial princes. Moves doctrinal definition from papal councils to princely courts and state-sponsored theological faculties. Moves the legitimacy of religious practice from universal Church authority to state decree (cuius regio eius religio). The transfer is enforced through confiscation, legal prohibition, and violence.
% ABSENT_VOICES: The Catholic Church itself—Rome's institutional response arrives after the political settlement is fait accompli, and popes are excluded from the agenda-setting that determines which rulers adopt reform. Radical reformers and Anabaptists, who disagreed with both Catholicism AND the state-church settlement, are expelled or executed and their voices are not heard in the establishment of the new order. The peasantry, whose uprisings invoked reformation theology but were crushed by the same rulers who invoked it, are silenced. Religious minorities (Jews, Muslims, dissident Christians) whose persecution the new arrangement requires are not consulted. The absent voices are those who would have contested the weaponization of theology for political extraction.
% DISAPPEARANCE_RATIONALE: If this constraint—the Reformation as the political swap of church authority and assets to secular rulers—had not occurred, European political development would have been fundamentally different: the Papacy would have retained temporal authority and property in Protestant regions; centralized territorial states would have lacked the legitimizing doctrinal break from Rome that the Reformation provided; the asset transfer that funded state consolidation would not have occurred; the norm of cuius regio eius religio (territorial religious monopoly) would not have become enshrined; the subsequent Wars of Religion and Peace of Westphalia would have had entirely different configurations. The constraint is constitutive of the modern nation-state system that emerged from 1648 onward.
% FOUNDING_PROBLEM: The founding problem under this reading is not a theological truth-question but a political one: How can secular rulers break the Papacy's claims to universal jurisdiction over Christian souls and property, consolidate territorial control, and retain legitimacy while doing so? How can they justify to their subjects the seizure of Church lands without appearing to be mere thieves? How can they establish a religious uniformity under state authority without the religious universalism that Rome claims?
% FOUNDING_PROBLEM_CORROBORATION: This reading is corroborated by historians and political analysts outside the Catholic institutional tradition—scholars like Christopher Hill, R. H. Tawney, Charles Tilly, and Peter Blickle document the timing of reformation adoption by rulers relative to territorial disputes with Rome, the pattern of monastery confiscation and redistribution to princely allies, the explicit framing of reformation in princely decrees as political necessity, and the subsequent use of state churches to enforce territorial authority. The pattern is also corroborated indirectly by the Peace of Westphalia (1648), which enshrined cuius regio eius religio as a political principle solving the problem of territorial religious monopoly—the endpoint of the process this reading describes. Catholic sources and theological historians dispute this causal reading and emphasize genuine theological innovation; their counter-corroboration comes from within the tradition seeking to defend Catholic theological positions, not from external political analysis.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).

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
 *   Under this reading, extractiveness is HIGH (0.78 at interval end) because the constraint's primary function is to transfer church assets (estimated at 25-30% of arable land and major revenue flows) from Rome to secular rulers and their allies. This is not incidental extraction but the core mechanism. Suppression is HIGH (0.71) because the arrangement is enforced through explicit prohibition of Catholic practice, confiscation, religious coercion of minorities, and violent suppression of resistance—enforcement is structural to the constraint, not external to it. Theater ratio is MODERATE-HIGH (0.62 at interval end, increasing from 0.25 at 1500), indicating that the constraint's functional purpose (establishing state religious monopoly and tax uniformity) requires continuous theological justification and legitimation. Over time, the theological vocabulary becomes increasingly standardized and ritualized—it must be performed and maintained even as the underlying extraction is stabilized. The measurement series track extractiveness rising sharply from 1500–1570 (as confiscations accelerate and state churches solidify), then plateauing 1570–1648 as the arrangement becomes normalized and suppression requirement stabilizes. Theater ratio rises throughout but levels off after 1570, indicating the constraint has achieved institutional routinization. The shared time grid ensures every metric is authored at every examined time point, preventing alignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The political-swap reading should compute VERY DIFFERENTLY across stakeholder seats. From the secular ruler seat (agenda_setter, institutional power, arbitrage exit), the arrangement appears as genuine coordination—establishing religious uniformity under state law is a legitimate problem, and reformation doctrine provides the necessary legitimizing break from Rome. From the Catholic Church seat (institutional power, constrained exit), the same arrangement is experienced as predatory extraction and power usurpation. From the peasantry seat (powerless, trapped), the arrangement oscillates between liberation theology and violent enforcement—promises of Christian freedom couple with crushing of uprisings. The engine should compute these divergent types from the structural data: the agenda-setter seat likely classifies as rope (coordination function is real for state consolidation); the victim seat likely classifies as snare (systematic extraction with suppression); the powerless-payer seat likely classifies as snare or piton (theatrical liberation with actual costs and constraints). The claim is tangled_rope because it asserts both coordination (establishing territorial religious uniformity solves a genuine problem) AND asymmetric extraction (the solution transfers assets and power to princes), but the seat-level divergence is where the real structure lives.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular rulers are the structural beneficiaries—they collect the asset transfer, establish religious monopoly, consolidate territorial control, and face minimal exit costs (they can shift doctrinal framing or intensity of enforcement if politically advantageous). Directionality d for this seat should be very low (near 0.0), indicating they are full beneficiaries. The Catholic Church is the primary victim—it bears systematic extraction of assets, loses authority claims, and is forced to restructure defensively. Directionality for Rome should be very high (near 1.0), indicating full target status. Reformed state churches occupy an intermediate position: they benefit from establishment and state support but are instrumentalized for political ends and face suppression if they diverge from ruler preferences. Directionality for them should be moderate (0.3–0.5), indicating they are coordinated but somewhat constrained. Religious minorities and peasants are trapped payers—they experience suppression and coercion with minimal exit options. Directionality should be high (0.7–0.9) for these powerless seats. Beneficiary groups feed low d; victim groups feed high d; trapped victims with constrained identity_locked exit produce the highest d values. The derivation chain runs from beneficiary/victim declarations + power atom + exit_options through the engine's directionality computation; no overrides are needed if the structural data is honest.
 *
 * MANDATROPHY ANALYSIS:
 *   The political-swap reading is NOT a mandatrophic constraint in the classical sense (a commitment whose founding problem has outlived its function). Instead, it models the founding problem as live and actively maintained: the problem (establishing state religious uniformy and territorial monopoly over subjects' religious practice) remains a persistent goal of the state and is continuously re-enforced through legal, educational, and ecclesiastical structures. However, the constraint carries a subtle mandatrophy profile: the founding problem under this reading is POLITICAL (how to consolidate secular authority and break Rome), not THEOLOGICAL (how to achieve doctrinal truth). Once the political settlement is established (by 1648), the theological justification for reformation becomes increasingly residual—modern Protestant churches maintain reformation doctrines not because they solve current political problems but because they are institutionally inherited. The theater ratio rising to 0.62 by 1648 indicates that by the endpoint of the interval, a growing fraction of reformation practice is performative maintenance rather than functional response to the original political need. The constraint has not fully decayed into pure piton (it retains genuine functions in establishing state-church relationships and religious uniformity), but it has acquired a growing theatrical component. This suggests the constraint is transitioning from tangled_rope (genuine political coordination + extraction) toward piton (inherited enforcement with attenuated original function) over the 1500–1648 interval, though the full piton classification does not emerge until the 18th century when enlightenment critique and state secularization further erode the underlying political rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_primacy_vs_political_instrumentalization,
    'Was the theological innovation in the Reformation (justification by faith, clerical marriage, vernacular scripture) the primary cause of institutional separation, or was theology post-hoc rationalization for rulers'' pre-existing political agenda to break Rome and seize church assets?',
    'Historical evidence: (1) Timeline analysis—did theological disputes arise before or after rulers began confiscating church property and making political overtures to break Rome? (2) Motivation documentation—what do rulers'' private correspondence, decrees, and councils reveal about their expressed reasons for adopting reform? (3) Theological uptake patterns—did rulers adopt reformation theology selectively (keeping doctrines that served them, rejecting those that did not), or did they accept comprehensive theological packages? (4) Counter-factual—would the same theological innovations have achieved institutional separation without the political cover of princely power?',
    'If theology was primary, this reading''s claim that theology is post-hoc rationalization is foreclosed and the theological_climb_reading moves to the center. If theology was secondary and instrumentalized, this reading''s tangled-rope classification (real coordination function in establishing territorial uniformity + asymmetric extraction of church assets) is strengthened. The omega documents an irreducible epistemic contest about causation that historians have not definitively resolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_primacy_vs_political_instrumentalization, conceptual, 'Whether theological innovation or political power-seeking is the primary driver of the Reformation''s institutional separation.').

omega_variable(
    reformation_periodization_endpoint,
    'Does the political swap reading''s account of the Reformation end at Luther''s 1517 theses and the early 1520s ruptures, or does it extend to 1648 when the Peace of Westphalia institutionalizes cuius regio eius religio as a political settlement?',
    'Definitional choice: The traditional periodization of the Reformation (1517–ca. 1555 Peace of Augsburg) treats it as a religious movement and doctrinal rupture. The political-swap reading extends the periodization to 1648 because the underlying extraction process—rulers consolidating church assets and establishing state religious monopolies—is not complete until Westphalia formalizes the principle and the Wars of Religion resolve into territorial settlements. The ending point determines what counts as part of the constraint.',
    'If the constraint ends at 1555, the theater_ratio and suppression metrics may be measured differently—enforcement is still being contested and violent during the Wars of Religion (1562–1598). If it extends to 1648, the constraint has achieved institutional stability (normalized theater, reduced active suppression). This omega documents a legitimate disagreement about what ''the Reformation'' is as a historical object—a theological event (ends early) or a political settlement (ends 1648).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformation_periodization_endpoint, conceptual, 'Whether the Reformation is periodized as a 16th-century doctrinal rupture or a 1517–1648 political settlement process.').

omega_variable(
    overdetermination_alternative,
    'Is this reading''s causal singularity (political realignment) defensible, or is the composite_overdetermination_reading''s claim that multiple irreducible drivers (theological innovation, institutional collapse, political realignment, denominational emergence) occurred simultaneously more accurate?',
    'Methodological: The political-swap reading treats political extraction as the primary mechanism and theology as post-hoc. The composite reading asserts all four drivers co-constituted the Reformation and no single causal priority can be assigned without collapsing the others. This is partly empirical (evidence for competing causal mechanisms) and partly philosophical (whether historical causation can have a single primary driver or is inherently overdetermined). The resolution involves assessing whether the four mechanisms are genuinely independent or whether one is the ''real'' mechanism and the others are derivative.',
    'If overdetermination is correct, the political-swap reading''s claim to causal primacy is falsified, and the theological_climb_reading and composite_overdetermination_reading coexist as equally valid framings. If political extraction is indeed primary, the other framings are auxiliary narratives. The omega documents the philosophical challenge of historical causation—whether monocausal explanation is ever justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_alternative, conceptual, 'Whether the Reformation is a monocausal political event or an overdetermined composite of irreducible drivers.').

omega_variable(
    catholic_vulnerability_preconditions,
    'Was the Catholic Church''s vulnerability to reformation attacks and asset seizure inherent to papal governance structures (corruption, absenteeism, doctrinal complacency), or was the vulnerability created by prior political fragmentation of European authority that made large asset holdings defensible only under centralized princely power?',
    'Historical evidence: (1) Did the Church''s institutional problems invite reformation critique independent of ruler motivations? (2) Were these problems unique to this historical moment or longstanding? (3) Without the prior fragmentation of secular authority (which the Reformation exacerbated), could a centralized Church have retained assets? (4) Counter-factual: if the Church had reformed its practices in the 1490s–1510s, would rulers have seized assets anyway?',
    'If the Church''s internal vulnerability was primary, the political-swap reading may overstate ruler agency and understate structural collapse. If political preconditions (fragmentation that made large centralized institutions defensible) were primary, the reading''s emphasis on ruler opportunism is justified. This omega documents whether the Reformation is best understood as predatory (rulers attacking a functioning institution) or parasitic (rulers exploiting an already-failing institution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_vulnerability_preconditions, empirical, 'Whether the Catholic Church''s vulnerability to reformation attacks was intrinsic or politically preconditioned.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1500, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1500, reformation_event_boundary__political_swap_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement_basis(refo_tr_t1500, observed).
narrative_ontology:measurement(refo_tr_t1520, reformation_event_boundary__political_swap_reading, theater_ratio, 1520, 0.35).
narrative_ontology:measurement_basis(refo_tr_t1520, observed).
narrative_ontology:measurement(refo_tr_t1540, reformation_event_boundary__political_swap_reading, theater_ratio, 1540, 0.48).
narrative_ontology:measurement_basis(refo_tr_t1540, observed).
narrative_ontology:measurement(refo_tr_t1570, reformation_event_boundary__political_swap_reading, theater_ratio, 1570, 0.58).
narrative_ontology:measurement_basis(refo_tr_t1570, observed).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__political_swap_reading, theater_ratio, 1600, 0.62).
narrative_ontology:measurement_basis(refo_tr_t1600, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.62).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1500, reformation_event_boundary__political_swap_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement_basis(refo_be_t1500, observed).
narrative_ontology:measurement(refo_be_t1520, reformation_event_boundary__political_swap_reading, base_extractiveness, 1520, 0.28).
narrative_ontology:measurement_basis(refo_be_t1520, observed).
narrative_ontology:measurement(refo_be_t1540, reformation_event_boundary__political_swap_reading, base_extractiveness, 1540, 0.52).
narrative_ontology:measurement_basis(refo_be_t1540, observed).
narrative_ontology:measurement(refo_be_t1570, reformation_event_boundary__political_swap_reading, base_extractiveness, 1570, 0.68).
narrative_ontology:measurement_basis(refo_be_t1570, observed).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__political_swap_reading, base_extractiveness, 1600, 0.74).
narrative_ontology:measurement_basis(refo_be_t1600, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.78).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1500, reformation_event_boundary__political_swap_reading, suppression_requirement, 1500, 0.2).
narrative_ontology:measurement_basis(refo_su_t1500, observed).
narrative_ontology:measurement(refo_su_t1520, reformation_event_boundary__political_swap_reading, suppression_requirement, 1520, 0.38).
narrative_ontology:measurement_basis(refo_su_t1520, observed).
narrative_ontology:measurement(refo_su_t1540, reformation_event_boundary__political_swap_reading, suppression_requirement, 1540, 0.54).
narrative_ontology:measurement_basis(refo_su_t1540, observed).
narrative_ontology:measurement(refo_su_t1570, reformation_event_boundary__political_swap_reading, suppression_requirement, 1570, 0.65).
narrative_ontology:measurement_basis(refo_su_t1570, observed).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__political_swap_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement_basis(refo_su_t1600, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.71).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__political_swap_reading, 0.12).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, peace_of_westphalia_state_sovereignty_settlement).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, wars_of_religion_confessional_coercion).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family modeling the Reformation as a contested historical event. The political_swap_reading frames it as state rulers exploiting theological disputes for asset seizure; the theological_climb_reading frames it as genuine doctrinal innovation; the composite_overdetermination_reading denies any single causal driver. Each reading instantiates a different constraint with different ε values, beneficiary/victim sets, and classifications. The upstream theological_climb_reading provides the doctrinal vocabulary that this reading instrumentalizes; the reading influences the composite_overdetermination_reading by asserting one of its component drivers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, institutional, 0.05).
constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
