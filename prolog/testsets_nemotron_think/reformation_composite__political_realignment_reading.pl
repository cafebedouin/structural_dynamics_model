% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Cuius Regio Eius Religio as Sovereignty Assertion
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   The Reformation, read as a political realignment event, centers on the
 *   principle cuius regio eius religio (whose realm, his religion)
 *   established at Augsburg (1555) and confirmed at Westphalia (1648).
 *   Emerging territorial rulers — German princes, Scandinavian monarchs,
 *   Henry VIII in England — used religious differentiation to seize
 *   ecclesiastical assets, assert legislative supremacy over church affairs,
 *   and withdraw from imperial and papal jurisdiction. The constraint is the
 *   territorial state church system: a package of enforced religious
 *   uniformity, confiscated church property, and jurisdictional sovereignty
 *   that territorial rulers imposed on their populations and extracted from
 *   universal authorities. The coordination function is real — religious
 *   uniformity reduced internal confessional conflict and provided a
 *   legitimation basis for territorial sovereignty — but the extraction is
 *   asymmetric: territorial rulers gained sovereign rights (taxation,
 *   legislation, appointment) while imperial and papal authority lost
 *   effective governance capacity over German and northern European
 *   territories. Religious dissenters (Anabaptists, Calvinists in Lutheran
 *   territories, Catholics in Protestant territories, etc.) bore the
 *   suppression costs. The measurement series tracks three phases:
 *   pre-Augsburg fluidity (1517-1555), the Augsburg settlement and its
 *   breakdown (1555-1618), and the Thirty Years' War/Westphalia resolution
 *   (1618-1648).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.78).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.82).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Eius Religio as Sovereignty Assertion").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '7837bf4a-b2b8-4dbe-bb20-c687d401d5c0').
narrative_ontology:cs_kernel_codification('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', distributed).
narrative_ontology:cs_authority_grounding('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', extraction).
narrative_ontology:cs_interpretation_layer_present('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0').
narrative_ontology:cs_reading_relation('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', foundational, territorial_sovereignty_supersedes_universal_authority).
narrative_ontology:cs_axiom_status(territorial_sovereignty_supersedes_universal_authority, holdable).
narrative_ontology:cs_axiom_grounding('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', territorial_sovereignty_supersedes_universal_authority, conventional).
narrative_ontology:cs_axiom('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', foundational, religious_uniformity_is_political_order).
narrative_ontology:cs_axiom_status(religious_uniformity_is_political_order, holdable).
narrative_ontology:cs_axiom_grounding('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', religious_uniformity_is_political_order, instrumental).
narrative_ontology:cs_reference_frame('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', imperial_papal_universalism).
narrative_ontology:cs_drift_state('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', westphalian_settlement, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('7837bf4a-b2b8-4dbe-bb20-c687d401d5c0', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, imperial_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, religious_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, urban_magistrates_and_councils).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, urban_magistrates_and_councils).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, merchant_and_artisan_classes).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, peasant_populations).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, state_sovereignty_against_universal_empire).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, territorial_religious_uniformity_as_political_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German princes, Scandinavian kings, and English monarchs who adopt Reformation to seize ecclesiastical lands, taxation rights, episcopal appointments, and legislative control over doctrine. They set the confessional policy for their territories (cuius regio), enforce it through visitations and consistories, and use religious uniformity as a legitimation basis for sovereign authority. Their exit is arbitrage-grade: they choose the confession that maximizes political advantage (Lutheran, Reformed, Anglican, or remaining Catholic).
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    powerful, generational, arbitrage, regional).

% Consolidating monarchies (France, Spain, Sweden, Denmark, England) that use religious policy to centralize authority. In Protestant cases, they become the territorial rulers above. In Catholic cases (France, Spain), they extract gallican liberties from papal authority (concordat of Bologna, patronato real) using the Reformation threat as leverage. They benefit from weakened universal authorities but face different constraint dynamics than German princes.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_states, beneficiary,
    institutional, generational, mobile, national).

% The Habsburg Holy Roman Emperor (Charles V, Ferdinand I, Maximilian II, Rudolf II, Ferdinand II, Ferdinand III) loses effective sovereignty over German territories as princes claim jus reformandi. The imperial constitution (Reichsrecht) binds the emperor to defend the Catholic Church but provides no enforcement mechanism against Protestant estates. Trapped in a universalist framework that requires religious unity to function, the emperor cannot exit the constraint without dissolving the Empire. Bears extraction of legislative, judicial, and fiscal rights to the estates.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, imperial_authority, payer,
    institutional, civilizational, trapped, continental).

% The Roman papacy loses jurisdiction, revenue (annates, Peter's pence), and appointment rights across northern Europe. Unlike the emperor, the papacy has constrained exit: it can (and does) launch Counter-Reformation (Council of Trent, Jesuits, Index, Inquisition) to reclaim territory and reform internally. Retains spiritual authority and southern European territories. The constraint extracts territorial governance but not the papacy's core claim to spiritual primacy.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, constrained, global).

% Anabaptists, spiritualists, anti-Trinitarians, Calvinists in Lutheran territories, Lutherans in Catholic territories, Catholics in Protestant territories, Jews. Forced into confessional migration (the only exit) or conformity. Identity_locked because religious identity fuses with salvation ontology — exit is not merely costly but existentially unthinkable for many. Bear suppression (exile, execution, property loss, civil disabilities) without political representation. The constraint's suppression is experienced as total.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, religious_dissenters, payer,
    powerless, biographical, identity_locked, local).

% Imperial free cities (Nuremberg, Augsburg, Strasbourg, Ulm, etc.) that adopt Reformation to seize church property, control clergy, and assert autonomy from both bishop and emperor. They benefit from ecclesiastical assets and legislative independence but pay enforcement costs (visitations, poor relief, school systems) and face imperial legal challenges. Their exit is constrained: they must negotiate within the imperial framework.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, urban_magistrates_and_councils, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, urban_magistrates_and_councils, payer).

% Urban populations subject to magistrate confessional policies. Pay through mandatory church attendance, tithes redirected to state church, exclusion from guilds/civic office for non-conformists. Some benefit from secularized church poor relief and school systems. Exit is constrained: migration possible but economically costly; confessional affinity networks facilitate movement to co-religionist cities.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, merchant_and_artisan_classes, payer,
    moderate, biographical, constrained, regional).

% Rural populations bound to lord's confessional policy by serfdom/tenancy and geographic immobility. Bear the brunt of wartime suppression (Thirty Years' War devastation), mandatory attendance, tithe burdens. The Peasants' War (1524-25) showed the cost of resistance. No meaningful exit; identity_locked for those who internalize the territorial confession, trapped for those who do not.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, peasant_populations, payer,
    powerless, immediate, trapped, local).

% Analytical seat observing the constraint's operation across the full interval. Sees the structural asymmetry: territorial rulers gain sovereignty, imperial/papal authority lose governance capacity, populations bear suppression. The kernel contest (political vs. technological vs. theological) is visible from this seat as a classification dispute about the same historical referent.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Territorial religious uniformity solves the coordination problem of legitimate authority in a fragmenting imperial system: it provides a clear jurisdictional boundary (the ruler's territory = the church's territory), a legitimation source (divine right mediated through state church), and a conflict-reduction mechanism (subjects know which confession is legal, reducing confessional violence within territories). The Peace of Augsburg (1555) and Westphalia (1648) institutionalize this as a European coordination system.
% TRANSFER_FUNCTION: Moves sovereign rights (legislation, taxation, judicial authority, episcopal appointments) and ecclesiastical assets (land, tithes, endowments) from imperial/papal authority to territorial rulers. Moves conformity costs (conscience, migration, civil disabilities) from territorial rulers to dissenting populations. Moves enforcement costs (visitations, consistories, military suppression) from universal authorities to territorial states.
% ABSENT_VOICES: The non-elite populations whose religious lives were reorganized without consultation: peasant communities, urban poor, women (excluded from confessional politics), Jewish communities (subject to expulsion/restriction regimes intensified by confessionalization), and colonial subjects (the Reformation's sovereignty logic extended to papal donation of Americas). Also absent: the pre-Reformation imperial reform movement that sought a national German church under imperial authority — a third way foreclosed by the princes' appropriation.
% DISAPPEARANCE_RATIONALE: If cuius regio eius religio and the territorial state church system vanished in 1555, the Holy Roman Empire would lack a constitutional settlement for religious difference, likely leading to earlier and more chaotic confessional wars. The territorial sovereigns would lose their primary legitimation basis and ecclesiastical revenue stream. The papacy would retain theoretical jurisdiction but no enforcement mechanism in Germany. The confessional migration system (millions displaced) would not have formed. The Westphalian sovereignty order — built on the cuius regio principle extended to full territorial sovereignty — would lack its historical foundation.
% FOUNDING_PROBLEM: The Holy Roman Empire's constitutional failure to integrate rising territorial sovereignty with universal imperial authority, combined with papal fiscal extraction (annates, reservations, indulgences) from German lands, and the demand for a 'Reformation in head and members' that the imperial-papal system could not deliver. Territorial rulers needed a legal basis to appropriate ecclesiastical resources and legislate for their territories without imperial interference.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (imperial constitutional failure + papal extraction + demand for reform) is attested as dead by the Peace of Westphalia (1648), which recognized territorial sovereignty (jus belli ac pacis, jus reformandi) and ended papal jurisdiction in the Empire — corroborated by the imperial estates themselves (the beneficiaries) and by contemporary jurists (Grotius, Pufendorf) outside the benefiting parties. The arrangement (state churches) persisted for centuries after the problem was resolved, confirming mandatrophy.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because territorial rulers extracted substantial sovereign rights (episcopal appointments, church taxation, legislative control over doctrine) from imperial/papal authority and imposed conformity costs on dissenting populations. Suppression (0.82) is very high because the constraint's persistence depended on active enforcement: state church visitations, mandatory attendance laws, exile/execution of dissenters, and military suppression of resistance (Peasants' War, Schmalkaldic War, Thirty Years' War). Theater ratio (0.45) is moderate: theological justification was sincere for many actors (Luther, Calvin, Catholic reformers) but the political extraction function grew more visible over time, especially after 1555 when the Augsburg settlement legitimated territorial confiscation of church property. Accessibility collapse (0.88) is near-mountain level: once cuius regio was established, religious exit required physical migration (the confessional migration system), making alternatives structurally inaccessible for most subjects. Resistance (0.75) is high: imperial authority, papal authority, and dissenting populations mounted sustained military, legal, and theological resistance across 130 years.
 *
 * PERSPECTIVAL GAP:
 *   The territorial ruler seat experiences this as a rope/scaffold: genuine coordination (religious peace, legitimation) with manageable enforcement costs. The imperial authority seat experiences it as a snare: extraction of sovereign rights with no exit from the imperial constitution. The dissenter seat experiences it as a snare: pure extraction of conscience and property with identity-locked exit. The papal authority seat experiences it as a tangled rope: loses territorial jurisdiction but gains Counter-Reformation coordination function. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the constraint's aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers are primary beneficiaries (d ≈ 0.15): they collect sovereign rights, church assets, and legitimacy with near-arbitrage exit (they set the rules). Emerging nation-states (Sweden, Denmark, England) are beneficiaries at institutional scale (d ≈ 0.20). Imperial authority (Habsburg) is a primary victim (d ≈ 0.85): loses effective sovereignty over German territories, trapped by the constitutional structure of the Empire. Papal authority is a victim (d ≈ 0.75): loses jurisdiction, revenue, and appointment rights in northern Europe but retains spiritual authority and adapts via Counter-Reformation (more mobile than imperial). Religious dissenters are victims (d ≈ 0.90): identity_locked exit (confessional migration only), bear full conformity costs. Urban merchant classes and peasant populations are payers with constrained exit (d ≈ 0.70-0.80).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented imperial authority, papal overreach, demand for German national church) was substantially resolved by 1648: territorial sovereignty was recognized, papal jurisdiction in the Empire ended, and the confessional map stabilized. Yet the state church system persisted for centuries — in Germany until 1918, in Scandinavia until 2000, in England to present. The mandate (religious uniformity as political order) atrophied into institutional inertia: established churches became pitons (theatrical maintenance, diffuse costs, no concentrated beneficiary). This reading captures the extraction phase (1517-1648); the piton phase requires a separate constraint story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the reformation_composite kernel, or does it describe the same structural constraint as the theological_fragmentation_reading and technological_mediation_reading under different observables?',
    'Decompose the Reformation into separate constraint stories per reading (per ε-invariance principle): political_realignment_reading (this story), technological_mediation_reading, theological_fragmentation_reading. Each gets its own ε, stakeholders, and classification. Link via network.affects_constraints.',
    'If the three readings share a single ε, the kernel is one constraint with measurement-dependent classification (violates ε-invariance). If they have distinct ε values, they are a constraint family linked by structural influence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Reformation kernel decomposes into multiple ε-invariant constraints or one observable-dependent constraint').

omega_variable(
    political_vs_theological_extraction_boundary,
    'How much of the measured extraction (ε=0.78) is political sovereignty extraction versus theological conformity enforcement? Are they separable?',
    'Compare territories where rulers adopted reform without theological conviction (e.g., Henry VIII, some German princes) against territories with genuine theological movements (e.g., Geneva, Württemberg). Measure extraction differential.',
    'If political extraction operates independently of theological enforcement, the constraint is a political snare wearing theological cover. If inseparable, the tangled_rope classification holds: coordination (religious uniformity) and extraction (sovereignty) are structurally fused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_extraction_boundary, empirical, 'Separability of political extraction from theological coordination function').

omega_variable(
    imperial_papal_victim_structure,
    'Do imperial authority and papal authority experience this constraint as a unified victim class, or do they occupy distinct structural positions with different exit options and directionalities?',
    'Trace the divergent trajectories: papal authority loses direct territorial control but retains spiritual authority and eventually adapts (Counter-Reformation); imperial authority (Habsburg) loses effective sovereignty over German territories permanently. Different outcomes suggest different structural positions.',
    'If distinct, the victim class should be split in stakeholders with different power/exit profiles. If unified, the current single victim declaration is adequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_papal_victim_structure, conceptual, 'Whether imperial and papal authority are structurally distinct victim seats').

omega_variable(
    cuius_regio_coordination_genuineness,
    'Does cuius regio eius religio solve a genuine coordination problem (religious peace, reduced internal conflict) or is the coordination story entirely cover for sovereignty extraction?',
    'Measure internal religious violence before/after cuius regio adoption in adopting vs. non-adopting territories. Compare the Schmalkaldic War, French Wars of Religion, Thirty Years'' War casualty rates against counterfactual.',
    'If genuine coordination, tangled_rope is correct. If coordination is negligible and extraction dominates, the constraint may be a snare with theological cover. This determines the beneficiary/victim balance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cuius_regio_coordination_genuineness, empirical, 'Whether the coordination function of territorial religious uniformity is structurally real or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_political_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(reformation_political_tr_t1529, reformation_composite__political_realignment_reading, theater_ratio, 1529, 0.3).
narrative_ontology:measurement(reformation_political_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.42).
narrative_ontology:measurement(reformation_political_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.55).
narrative_ontology:measurement(reformation_political_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.45).

% Extraction over time
narrative_ontology:measurement(reformation_political_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(reformation_political_be_t1529, reformation_composite__political_realignment_reading, base_extractiveness, 1529, 0.52).
narrative_ontology:measurement(reformation_political_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.68).
narrative_ontology:measurement(reformation_political_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.81).
narrative_ontology:measurement(reformation_political_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(reformation_political_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(reformation_political_su_t1529, reformation_composite__political_realignment_reading, suppression_requirement, 1529, 0.65).
narrative_ontology:measurement(reformation_political_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.78).
narrative_ontology:measurement(reformation_political_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.88).
narrative_ontology:measurement(reformation_political_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, westphalian_sovereignty_system).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, state_church_establishment_europe).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel decomposes into three constraint stories with distinct ε values. This reading (political_realignment) has ε=0.78 (high extraction). The technological_mediation_reading likely has lower ε (coordination via information infrastructure). The theological_fragmentation_reading likely has moderate ε (coordination via doctrinal boundary maintenance). The political reading extracts sovereignty from universal authorities; the technological reading coordinates dissent across distances; the theological reading coordinates belief communities. They form a constraint family linked by structural influence: the political reading's cuius regio settlement shaped the operating environment for the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, institutional, 0.75).
constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
