% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as License for Conquest and Indigenous Subjugation West of the Line (Spanish Conquest Legitimation Reading)
 *   domain: international law / colonial history / sovereignty theory
 *
 * SUMMARY:
 *   Between 1493 and the mid-seventeenth century, the papal donation (Inter
 *   caetera and the 1494 treaty) operated west of the demarcation line as the
 *   juridical license under which the Spanish crown claimed sovereignty over
 *   inhabited non-Christian lands, dispossessed their polities, and
 *   commandeered their labor through encomienda, repartimiento, and the
 *   Andean mita. Evangelization supplied the announced purpose; bullion,
 *   land, and labor command supplied the actual yield. This story
 *   instantiates ONE reading of the tordesillas demarcation kernel — the
 *   conquest-legitimation reading — and authors that reading alone as a
 *   clean, epsilon-invariant constraint: the exploration-rights allocation
 *   among Christian powers is the sibling reading, a separate file with its
 *   own (far lower) epsilon and no comparable victim set. KEY AGENTS (by
 *   structural relationship): - spanish_crown: agenda-setting beneficiary
 *   (institutional/arbitrage) — collects sovereignty, the royal fifth, and
 *   appellate jurisdiction - colonial_encomenderos: concentrated local
 *   beneficiaries (powerful/constrained) — receive labor and tribute grants -
 *   spanish_colonial_administration: administering beneficiary
 *   (institutional/mobile) — runs collection and enforcement -
 *   colonial_church_establishment: dual-positioned beneficiary
 *   (institutional/constrained) — gains flock and revenue, generates the
 *   sharpest internal opposition - taino_caribbean_peoples,
 *   nahua_altepetl_commoners, andean_mitayos: primary targets
 *   (powerless/trapped) — bear dispossession, labor drafts, demographic
 *   collapse - chichimeca_frontier_groups: targets with partial mobility
 *   (powerless/constrained) - non_christian_sovereigns_west_of_line: excluded
 *   seat — consent bypassed by proclamation fiction -
 *   scholastic_jurists_of_salamanca: analytical observer — dismantled the
 *   doctrine from inside the tradition
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.84).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.85).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.84).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as License for Conquest and Indigenous Subjugation West of the Line (Spanish Conquest Legitimation Reading)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international law / colonial history / sovereignty theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '83e6109c-61c8-406c-8a0b-639326eaeadf').
narrative_ontology:cs_kernel_codification('83e6109c-61c8-406c-8a0b-639326eaeadf', fixed_text).
narrative_ontology:cs_authority_grounding('83e6109c-61c8-406c-8a0b-639326eaeadf', extraction).
narrative_ontology:cs_interpretation_layer_present('83e6109c-61c8-406c-8a0b-639326eaeadf').
narrative_ontology:cs_reading_relation('83e6109c-61c8-406c-8a0b-639326eaeadf', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('83e6109c-61c8-406c-8a0b-639326eaeadf', foundational, papal_vicariate_disposes_infidel_dominion).
narrative_ontology:cs_axiom_status(papal_vicariate_disposes_infidel_dominion, overridden).
narrative_ontology:cs_axiom_grounding('83e6109c-61c8-406c-8a0b-639326eaeadf', papal_vicariate_disposes_infidel_dominion, theological).
narrative_ontology:cs_axiom('83e6109c-61c8-406c-8a0b-639326eaeadf', secondary, infidel_refusal_after_proclamation_licenses_war).
narrative_ontology:cs_axiom_status(infidel_refusal_after_proclamation_licenses_war, overridden).
narrative_ontology:cs_axiom_grounding('83e6109c-61c8-406c-8a0b-639326eaeadf', infidel_refusal_after_proclamation_licenses_war, theological).
narrative_ontology:cs_reference_frame('83e6109c-61c8-406c-8a0b-639326eaeadf', papal_dispensation_of_infidel_dominion).
narrative_ontology:cs_drift_state('83e6109c-61c8-406c-8a0b-639326eaeadf', post_valladolid_post_sublimis_deus, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('83e6109c-61c8-406c-8a0b-639326eaeadf', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, colonial_encomenderos).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, colonial_church_establishment).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, taino_caribbean_peoples).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, nahua_altepetl_commoners).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, andean_mitayos).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, chichimeca_frontier_groups).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_plenitudo_potestatis_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, requerimiento_proclamation_fiction).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, just_war_against_unbelievers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the papal donation as the foundation of its American titles. Receives the royal fifth of all bullion, customs, and ecclesiastical revenues; appoints viceroys and the Council of the Indies; issues the laws governing conquest and settlement. When theologians attacked the donation's validity, it supplemented rather than abandoned its title claims, drawing on conquest, possession, and dynastic argument as needed.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% Viceroys, audiencia judges, and treasury officials run the colonial machine the donation authorized: collecting taxes and tribute, organizing labor drafts, adjudicating disputes between settlers and indigenous communities. Careers advance through service in the Indies; officials return to Spain with fortunes or debts.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, beneficiary,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter).

% Hold grants of indigenous communities' labor and tribute in exchange for nominal duties of protection and evangelization. Their households, estates, and local status rest on the grant; when the New Laws threatened to curb inherited encomiendas, they petitioned, lobbied, and in Peru rose in armed revolt.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, colonial_encomenderos, beneficiary,
    powerful, biographical, constrained, regional).

% Operates parishes, missions, schools, and the inquisitorial tribunal across the Indies; receives tithes and endowments and gains its flock through conversion. Its own ranks produced the fiercest critics of the conquest — Dominican and Franciscan chroniclers who documented the destruction their institution otherwise administered.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, colonial_church_establishment, beneficiary,
    institutional, generational, constrained, continental).

% First peoples subjected under the grant's authority: gold-quota labor, encomienda tribute, and epidemic disease collapsed their numbers within a generation of contact. Surviving communities were resettled into supervised villages; flight to other islands or the interior was the main remaining recourse.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, taino_caribbean_peoples, payer,
    powerless, biographical, trapped, regional).

% Live in city-states incorporated by conquest; pay tribute in goods and rotating labor, furnish porters and auxiliaries for campaigns, and see their temples and books destroyed and their rites proscribed. Commoners bear the drafts while surviving noble lineages negotiate continuity by collaborating.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, nahua_altepetl_commoners, payer,
    powerless, biographical, trapped, regional).

% Supply rotating contingents of workers to the silver and mercury mines under the mita draft; many walk for weeks from highland villages to Potosi or Huancavelica, and village economies bend around each man's absence. Communities owe collective quotas, so kin absorb the shortfall when a draftee dies on the road or underground.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, andean_mitayos, payer,
    powerless, biographical, trapped, regional).

% Semi-nomadic peoples north of the settled plateau, raided for slaves and pressed by garrisons and missions. Their mobility let them raid, retreat, and negotiate from strength for decades, until purchase-for-peace payments and mission resettlement narrowed their range.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, chichimeca_frontier_groups, payer,
    powerless, biographical, constrained, regional).

% Rulers of the Mexica, Inca, Maya, and other polities whose lands the donation disposed of. No instrument sought their consent; a proclamation read in Spanish, sometimes to empty villages or from ships offshore, stood in for it. Those who survived conquest governed as clients or captives.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, non_christian_sovereigns_west_of_line, excluded,
    organized, generational, trapped, regional).

% Theologians and jurists who examined the conquest's legitimacy at the crown's own request. Vitoria and his students argued that infidel peoples held true dominion before and after baptism and that the pope was no temporal lord of the world — premises that dissolved the donation's core claim from inside the tradition that produced it.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, scholastic_jurists_of_salamanca, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocated hemispheric claims between the two Iberian crowns, forestalling inter-Christian war over newly encountered territories; within the Spanish sphere, supplied a single legal-theological script (the Requerimiento procedure) that coordinated conquistadors, clergy, and administrators on one legitimation routine.
% TRANSFER_FUNCTION: Moved land title, jurisdiction, and labor — tribute, drafted mine and estate labor, personal service — from indigenous polities and commoners west of the line to the Spanish crown, its encomendero grantees, and the colonial church establishment; moved bullion, chiefly the royal fifth of silver production, from the Americas to Castile.
% ABSENT_VOICES: The non-Christian sovereigns and communities whose lands were disposed of were never party to any instrument; their consent was simulated by proclamation. Rival European powers were excluded by the allocation itself and objected from outside by rejecting the grant's validity rather than from within any negotiating forum.
% DISAPPEARANCE_RATIONALE: If the grant-as-conquest-license vanished overnight, the entire juridical floor of Spanish America disappears: no encomienda grants, no mita drafts, no royal fifth, no viceregal jurisdiction resting on the donation. Land tenure, labor command, church structure, and transatlantic finance all reorganize around whatever title theory replaced it.
% FOUNDING_PROBLEM: How to confer legitimate title on Iberian expansion into inhabited non-Christian lands while preventing war between the two Catholic crowns — solved, on this reading, by papal donation transferring dominion over the inhabitants west of the line to the Spanish monarchy.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Salamanca school (Vitoria, Soto) concluded the pope held no temporal lordship over the whole world and that infidel peoples possessed true dominion; the papal bull Sublimis Deus (1537) formally affirmed indigenous liberty and property against deprivation; the Valladolid junta (1550-51) suspended conquests pending the legitimacy inquiry. The crown's own commissioned jurists, not its enemies, attested that the founding premise had failed.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.84 at interval end) because the arrangement transferred land, jurisdiction, and labor wholesale: encomienda tribute, rotating mine drafts at Potosi and Huancavelica, and a demographic collapse on the order of most of the indigenous population within a century of contact. Suppression (0.85) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's arithmetic — reflecting military conquest, forced resettlement (congregacion and reduccion), destruction of temples and codices, proscription of indigenous rites, and inquisitorial jurisdiction; only extractiveness gets scaled, by directionality and scope. Theater (0.40) is moderate: the Requerimiento — a legal proclamation read in Spanish to people who spoke no Spanish, sometimes to empty villages or from ships — was legitimation theater laid over entirely functional coercion, and conversion ceremonies overlaid labor exaction; the coercion was never performative, but a large fraction of the constraint's self-justifying activity was. Accessibility collapse (0.75): once a region was conquered, alternatives to participation nearly vanished — residual exits were flight (maroon communities), frontier migration, and negotiated local autonomies. Resistance (0.70): revolt was continuous (Mixton War 1541, the Arauco war from the 1550s, the Chichimeca wars 1550-1590, the Taki Onqoy movement in the 1560s), alongside marronage and the intra-Spanish opposition of Dominican and Franciscan critics that forced the New Laws. Coalition potential among the powerless was real but systematically broken: the crown fought indigenous polities with indigenous allies (Tlaxcalans against the Mexica; native auxiliaries in Chile and the north), so class-wide coalition formation across conquered peoples rarely achieved scale. The measurement series run on one shared time grid (1493, 1513, 1542, 1570, 1610, 1650) with every tracked metric authored at every point; the series show extraction accumulating through the conquest decades, peaking with the silver-mita complex around 1570, then easing slightly as encomienda decayed while suppression stayed high because coercion changed form (debt peonage, casta enforcement, inquisition) rather than declining.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a straightforwardly coercive arrangement: from inside an altepetl or a mita quota, the grant is the paper that dispossessed them. The beneficiary seats compute differently: from the encomendero's position the same structure is a divinely sanctioned order and a livelihood; from the crown's position it is a diplomatic instrument managing inter-crown rivalry; from the administrative position it is career infrastructure. The church seat splits internally — the same institution contains both the inquisitorial tribunal and Las Casas — so its computed position depends on which internal faction occupies the seat. The excluded sovereigns' seat never got to compute anything: the arrangement was executed on them without their participation. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the crown, the administration, the encomenderos, and the church hierarchy near the beneficiary pole (low d, subsidized or damped effective extraction); the crown sits nearest the pole as the seat that both wrote the rules and collected the largest share. Victim declarations place the four indigenous classes near the full-target pole; trapped exit pushes the Taíno, Nahua commoners, and mitayos to the extreme target end, while the Chichimeca's constrained-but-real mobility (raid, retreat, negotiate) holds them slightly short of it. The excluded sovereigns and the analytical jurists sit outside the beneficiary/victim derivation. No directionality override is authored: the derivation from declared roles plus exit options reproduces the true relationships, and the church's internal split is handled by its dual positioning in the stakeholder surface rather than by a power-atom override, which would misstate the crown's and administration's positions along with it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing title to inhabited non-Christian lands while keeping peace between the Catholic crowns — died within living memory of the grant: the Salamanca school refuted its doctrinal premise, Sublimis Deus (1537) formally affirmed what the conquest denied, Valladolid (1550-51) suspended conquests pending an inquiry the crown never honored in practice, and bilateral treaties (Zaragoza, 1529) and emergent international law replaced papal allocation. Yet the extraction arrangement persisted for three centuries. The classification disciplines two mislabelings: calling the whole complex a rope (it did coordinate the two crowns) launders the conquest license through the kernel's genuine coordination residue; calling it a piton (mere inertia) understates the active enforcement — armies, tribunals, labor drafts — and the concentrated capture at the crown. Snare captures the structure: a coordination cover story, coercion-dependent persistence, identifiable victims, concentrated receipt. The R5 mismatch (founding_problem_status=dead with disappearance_verdict=world_rearranges) flags the zombie structure directly: the arrangement outlived its justification and rearranged the world to keep collecting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (spanish_conquest_legitimation) of the tordesillas_demarcation_kernel; what structurally changes if the sibling reading (portuguese_exploration_legitimation) is adopted instead?',
    'Authoring the sibling story and comparing computed classifications: the sibling''s victim set is empty or limited to excluded European rivals, its epsilon is far lower, and its type should compute toward coordination rather than extraction.',
    'If the sibling reading dominates, the demarcation instruments classify as a genuine inter-crown coordination device and the conquest license stands exposed as a separate, parasitic structure; if this reading dominates, the instruments themselves are extractive at the root.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which reading of the Tordesillas kernel this constraint instantiates and what the sibling would change.').

omega_variable(
    counterfactual_title_independence,
    'How much of the measured extraction is attributable to the papal-grant title instrument specifically, versus conquest economics that would have proceeded under any available title theory?',
    'Comparative analysis of colonial extraction under differing title doctrines: Portuguese Brazil (same bulls, different reading), English settlement charters (no papal warrant), French alliance-and-trade patterns — measuring whether victim outcomes track the title doctrine or the imperial form.',
    'If extraction tracks the doctrine, the grant is load-bearing and its repudiation mattered materially; if outcomes are doctrine-independent, the grant is a legitimating overlay and the epsilon properly attaches to the imperial apparatus beneath it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_title_independence, empirical, 'Whether the title instrument caused the extraction or merely clothed it.').

omega_variable(
    suppression_internalization_split,
    'What share of post-conquest compliance rested on ongoing structural coercion versus internalized Christian-colonial identity fusion?',
    'Post-independence and post-enforcement-decay trajectories: persistence of communal religious forms, syncretic practice, and village-level self-governance after the coercive machinery weakened indicates how much compliance was carried internally rather than imposed.',
    'If a large share was internalized, the constraint''s effective suppression outlasted its enforcement infrastructure and classification should weight identity-lock mechanisms; if compliance tracked enforcement capacity, the structural measure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized components of the measured suppression.').

omega_variable(
    operative_death_date,
    'When did this reading cease to operate as live law — at the Valladolid settlement and Sublimis Deus (1537-1550), or at the Bourbon reforms and independence (18th-early 19th century)?',
    'Diplomatic and juridical citation analysis: tracking whether the crown continued invoking the donation in boundary disputes and title chains after 1550, and whether colonial courts still rested judgments on it.',
    'An early death date makes the post-1550 arrangement a successor constraint running on inertia; a late death date extends this constraint''s extractive lifetime by two centuries and shifts its lifecycle classification window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_death_date, empirical, 'Dating the reading''s operative death for lifecycle analysis.').

omega_variable(
    requerimiento_binding_force,
    'Was the Requerimiento ever treated as binding by its own operators, or was it universally understood as theater from the start?',
    'Contemporary testimony: conquistador and clerical accounts (Las Casas''s report that he did not know whether to laugh or weep; officials reading it to empty villages or from ships) weighed against cases where refusal-after-proclamation was formally pleaded as a war justification.',
    'If it bound consciences, the legitimation layer was partly functional and theater_ratio is overstated; if it was theater throughout, the entire procedural facade was extraction cover and the moral structure of the constraint is simpler than its paperwork suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(requerimiento_binding_force, conceptual, 'Whether the proclamation fiction was functional law or pure performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.12).
narrative_ontology:measurement_basis(tord_tr_t1493, observed).
narrative_ontology:measurement(tord_tr_t1513, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1513, 0.52).
narrative_ontology:measurement_basis(tord_tr_t1513, observed).
narrative_ontology:measurement(tord_tr_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1542, 0.44).
narrative_ontology:measurement_basis(tord_tr_t1542, observed).
narrative_ontology:measurement(tord_tr_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1570, 0.4).
narrative_ontology:measurement_basis(tord_tr_t1570, observed).
narrative_ontology:measurement(tord_tr_t1610, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1610, 0.37).
narrative_ontology:measurement_basis(tord_tr_t1610, observed).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.4).
narrative_ontology:measurement_basis(tord_tr_t1650, observed).

% Extraction over time
narrative_ontology:measurement(tord_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.32).
narrative_ontology:measurement_basis(tord_be_t1493, observed).
narrative_ontology:measurement(tord_be_t1513, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1513, 0.71).
narrative_ontology:measurement_basis(tord_be_t1513, observed).
narrative_ontology:measurement(tord_be_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1542, 0.88).
narrative_ontology:measurement_basis(tord_be_t1542, observed).
narrative_ontology:measurement(tord_be_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1570, 0.91).
narrative_ontology:measurement_basis(tord_be_t1570, observed).
narrative_ontology:measurement(tord_be_t1610, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1610, 0.87).
narrative_ontology:measurement_basis(tord_be_t1610, observed).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.84).
narrative_ontology:measurement_basis(tord_be_t1650, observed).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.35).
narrative_ontology:measurement_basis(tord_su_t1493, observed).
narrative_ontology:measurement(tord_su_t1513, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1513, 0.66).
narrative_ontology:measurement_basis(tord_su_t1513, observed).
narrative_ontology:measurement(tord_su_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1542, 0.86).
narrative_ontology:measurement_basis(tord_su_t1542, observed).
narrative_ontology:measurement(tord_su_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1570, 0.88).
narrative_ontology:measurement_basis(tord_su_t1570, observed).
narrative_ontology:measurement(tord_su_t1610, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1610, 0.86).
narrative_ontology:measurement_basis(tord_su_t1610, observed).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.85).
narrative_ontology:measurement_basis(tord_su_t1650, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Treaty of Tordesillas' covers two structurally distinct claims. The sibling story (portuguese_exploration_legitimation) authors the exploration-rights allocation among Christian powers — low epsilon, no indigenous victim set, coordination-flavored. This story authors the conquest-legitimation claim — high epsilon, indigenous victim set, enforcement-dependent. The upstream allocation claim was cited as cover for the downstream conquest claim, so the family edge runs from this reading to the sibling and back; each file carries its own epsilon, stakeholders, and classification, and neither hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
