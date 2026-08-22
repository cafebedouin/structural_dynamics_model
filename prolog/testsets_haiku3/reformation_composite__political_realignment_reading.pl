% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Territorial Sovereignty via Religious Differentiation (Political Realignment Reading)
 *   domain: political_economy/historical_epistemology
 *
 * SUMMARY:
 *   This constraint instantiates the political realignment reading of the
 *   Reformation: the historical event is fundamentally the mechanism by which
 *   emerging nation-states assert sovereignty against supra-territorial
 *   authority (papal and imperial) by adopting religious differentiation as a
 *   legitimacy apparatus. The reading fixes the referent as the standing
 *   arrangement of medieval universal authority (papal and imperial
 *   jurisdiction over Christian Europe) and measures extraction as it
 *   operates under that arrangement. Cuius regio eius religio—the ruler's
 *   religion determines the realm's religion—is the constraint's primary
 *   observable: territorial rulers control religious organization within
 *   their borders as a mechanism of political independence. This reading does
 *   NOT assert that theology is unimportant or that theological conviction
 *   played no role; it asserts that the structural function of the
 *   Reformation was political realignment, with theology serving as the
 *   vehicle. Other readings emphasize the technological (printing press as
 *   mass-distribution engine for local dissent) or theological (incompatible
 *   soteriological commitments) dimensions; this reading holds that political
 *   realignment is the primary structural fact, and the other readings are
 *   secondary mechanisms or epiphenomena of that realignment.
 *
 * KEY AGENTS:
 *   - Territorial rulers (princes, dukes, emerging monarchs): primary beneficiaries; use religious differentiation as a sovereignty mechanism; control clergy appointment and church property within their territories.
 *   - Papal authority (the Roman Catholic Church hierarchy): primary victim; loses jurisdiction over Christian Europe and revenue streams as rulers assert religious sovereignty.
 *   - Holy Roman Imperial authority: secondary victim; loses religious legitimacy that underwrote theoretical Christendom-wide authority; constrained to accept territorial religious sovereignty.
 *   - Reformed clergy (Lutheran, Reformed, Anglican theologians and church administrators): organized beneficiary with identity-lock constraint; gain social authority within territorial structures but are now instruments of ruler policy.
 *   - Peasantry and urban commons: powerless payers; trapped within territorial religious assignments; bear the cost of moral enforcement regimes and taxation restructuring.
 *   - Merchants and trading cities: moderate beneficiaries; gain regulatory simplification and reduced papal interference; constrained by territorial boundaries once established.
 *   - Religious minorities (Jews, Muslims, radical sects): excluded parties; face forced conformity or exile; structurally barred from the conversation that defines the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Territorial Sovereignty via Religious Differentiation (Political Realignment Reading)").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "political_economy/historical_epistemology").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '487b018b-623d-4f1e-aae2-a5bd5ee5c16f').
narrative_ontology:cs_kernel_codification('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', formalized).
narrative_ontology:cs_authority_grounding('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', extraction).
narrative_ontology:cs_interpretation_layer_present('487b018b-623d-4f1e-aae2-a5bd5ee5c16f').
narrative_ontology:cs_reading_relation('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', foundational, sovereignty_via_religious_differentiation).
narrative_ontology:cs_axiom_status(sovereignty_via_religious_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', sovereignty_via_religious_differentiation, instrumental).
narrative_ontology:cs_axiom('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', foundational, territorial_ruler_authority_supersedes_supra_territorial).
narrative_ontology:cs_axiom_status(territorial_ruler_authority_supersedes_supra_territorial, holdable).
narrative_ontology:cs_axiom_grounding('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', territorial_ruler_authority_supersedes_supra_territorial, deontological).
narrative_ontology:cs_reference_frame('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', medieval_universal_christendom_authority).
narrative_ontology:cs_drift_state('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', post_peace_of_westphalia, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('487b018b-623d-4f1e-aae2-a5bd5ee5c16f', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_imperial_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, reformed_clergy).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, merchants_and_trading_cities).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, reformed_clergy).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, peasantry_and_urban_commons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Princes, dukes, and emerging monarchs adopt reformed theological vocabularies as a mechanism to claim sovereignty over domestic religious organization, displacing papal and imperial adjudication. The theological innovation is real, but its primary function is to provide a legitimacy framework for political independence. They control appointment of clergy, tax church lands, and define what counts as heresy within their territories. The constraint's operation vindicates the principle of cuius regio eius religio—the ruler's religion determines the realm's religion—which converts religious identity into a tool of state consolidation.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    powerful, generational, arbitrage, continental).

% Loses jurisdiction over Christian Europe's religious organization as territorial rulers claim sovereign authority. The papal see's income streams from indulgences, benefices, and tithes erode as rulers assert control. The Pope cannot exit this constraint—the alternative is acknowledging loss of universal Christian authority. Forced to accept the doctrine that religious authority is locally administered by territorial sovereigns rather than universally administered from Rome.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, trapped, continental).

% The Holy Roman Emperor's claim to coordinate Christendom dissolves as component territories assert religious independence through reformed theology. The Emperor retains nominal authority but loses the religious legitimacy that once made that authority enforceable. Constrained by geography and military capability: cannot reintegrate reformed territories without massive military and political cost that exceeds the benefit of re-subjugation.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_imperial_authority, payer,
    institutional, civilizational, constrained, continental).

% Gain organizational independence and increased social authority within territorial structures (marriage, property inheritance, moral teaching authority). They are also constrained: their theology now serves ruler interests, and deviating from the territorial religion risks exile or execution. Identity-locked because their professional existence is constituted through the reformed ecclesiastical structure they helped legitimize.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, reformed_clergy, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, reformed_clergy, payer).

% Bear the costs of territorial consolidation through the reformed church: new moral enforcement regimes, mandatory participation in state-sponsored religion, taxation that now flows to the secular state rather than optional papal tithes. They cannot exit their territorial assignment. The constraint operates through moral capture (religious identity) and structural constraint (born into a reformed territory).
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, peasantry_and_urban_commons, payer,
    powerless, biographical, trapped, local).

% Analytical seat: observes the transformation of his theoretical authority over Christendom into a system where territorial rulers exercise effective religious sovereignty. The constraint's operation undermines his position; he observes rather than controls the process.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_emperor, observer,
    institutional, generational, analytical, continental).

% Gain regulatory simplification as territorial rulers rationalize commercial law under reformed ecclesiastical authority. Reduced papal interference in trade, simplified inheritance rules for merchant families, standardized weights and measures under state authority. Constrained because they cannot arbitrage between multiple competing jurisdictions once the reformed territorial boundary is established.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, merchants_and_trading_cities, beneficiary,
    moderate, biographical, constrained, regional).

% Jews, Muslims, and unorthodox Christian communities are structurally barred from the conversation about religious sovereignty. The constraint is authored by territorial rulers and papal/imperial authority; minorities have no seat at the negotiating table and face either forced conformity or exile. Their exclusion is not incidental—it is what makes the constraint work (homogeneous religious territory is easier to govern).
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, religious_minorities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, verifiable mapping between territorial jurisdiction and religious organization: each ruler administers a single, defined, territorially contiguous religious denomination. This solves the problem of religious heterogeneity within territories—instead of negotiating competing loyalties (papal authority, imperial authority, local lords, clergy), a single ruler defines orthodoxy and enforces it uniformly. Religious organization becomes coterminous with political organization.
% TRANSFER_FUNCTION: Moves religious authority, revenue streams, and appointment power from supra-territorial institutions (papacy, imperial court) to territorial rulers. Territorial rulers gain income from church property and tithes; the papacy and Holy Roman Emperor lose the same revenue. The constraint redirects flows of authority (who can excommunicate, define heresy, appoint bishops) from Rome and the imperial court to territorial capitals.
% ABSENT_VOICES: Religious minorities (Jews, Muslims, radical Christian communities) and the peasant communes that might have claimed direct theocratic authority are structurally excluded from the negotiation. They would argue for religious pluralism or direct communal religious authority, but the constraint's authors (territorial rulers and established clergy) define the legitimate conversation partners as themselves. The theological innovation (reformed doctrine) speaks only the language of territorial sovereignty, not minoritarian rights or communal autonomy.
% DISAPPEARANCE_RATIONALE: If this constraint—the principle that territorial rulers determine religious organization and allegiance—suddenly vanished, the continent's political structure would reorganize radically. Without religious differentiation as a legitimacy mechanism for sovereignty, papal authority would reassert itself over some territories, universal empire might re-cohere, or religions would stratify into transnational networks rather than territorial organizations. The modern nation-state depends on this constraint; its disappearance unmakes the political form that emerged from the Reformation.
% FOUNDING_PROBLEM: The medieval universal church and empire create competing, overlapping authorities: a lord owes allegiance to the Pope, the Emperor, local vassals, and customary law simultaneously. Religious dissent cannot be contained locally because papal authority can override local decisions. This generates chronic jurisdictional conflict and creates opportunities for religious movements to spread beyond the control of any single authority.
% FOUNDING_PROBLEM_CORROBORATION: Territorial rulers and reformed theologians attest the founding problem is real and the constraint solves it (their authored testimony). Papal and imperial historians attest the problem is real but claim the constraint was not the solution—the problem arose from princes' rebellion, not institutional necessity. Modern historians of political development (including those without affiliation to any benefiting party) corroborate that religious differentiation became a critical tool of state consolidation and sovereignty assertion, confirming the founding problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures at 0.68 at the interval end (1650) because the constraint fundamentally redirects authority and revenue from supra-territorial to territorial institutions. The extraction is high because rulers benefit from the transfer without providing proportional new value—the coordination function (mapping territory to religious organization) is real, but it could theoretically be achieved through negotiation rather than through the theological innovations that legitimize unilateral ruler authority. Suppression is high (0.71) because the constraint's persistence depends on active enforcement: inquisitions, heresy prosecution, forced conformity of religious minorities, exclusion of competing religious vocabularies. Theater rises sharply from 1530–1595 (0.28→0.44) as the rhetorical justification of the constraint grows more elaborate, then plateaus, suggesting that the initial novelty of religious differentiation as sovereignty language fades and is replaced by institutional routine. The measurement series reflects the constraint's lifecycle: low initial extraction when the medieval order still held (1500), rapid extraction accumulation as rulers implement reforms (1530–1595), and plateau as the territorial religious settlement becomes institutional norm (1595–1650). One shared time grid: every metric is authored at every examined time point (1500, 1530, 1560, 1595, 1620, 1650).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (territorial rulers) and the victim seats (papal and imperial authority) should compute as opposite directionalities from the engine: beneficiaries have low d (they are subsidized by the constraint), victims have high d (they bear the costs). The commons and powerless payers sit at high d as well (trapped, forced to conform). Reformed clergy occupy an intermediate position with identity-lock: they benefit from the constraint (social authority, organizational power) but are identity-locked into service of ruler interests. The engine's per-seat classification will show rulers computing as beneficiaries of a coordination mechanism, papal authority as targets of political extraction, and the peasantry as trapped payers with no exit. This divergence is exactly the measurement this story takes: structural data shows asymmetric directionality; engine computes type per seat; the constraint is rope to rulers, snare to peasants, tangled_rope at the system level where both coordination and extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers benefit from the constraint (d ≈ 0.1–0.2): they gain sovereignty authority, control clergy, capture church revenue, and achieve religious homogeneity within borders. Their exit option is arbitrage: they can choose which reformed theology to adopt based on strategic advantage (some choose Lutheranism, others Reformed, others Anglicanism), and this choice-set is what preserves their beneficiary status. Papal and imperial authority bear the costs (d ≈ 0.85–0.95): they lose jurisdiction, lose revenue, and lose the legitimacy that underwrote universal authority. Their exit is trapped—they cannot re-integrate reformed territories without impossible military cost, and acknowledging loss of authority is theologically and institutionally devastating. Reformed clergy benefit partially (d ≈ 0.4) but are identity-locked: they gain social authority within territories but cannot leave without losing professional identity and livelihood. Peasants pay (d ≈ 0.9): they are trapped within territorial religious assignment, bear the cost of moral enforcement, and have no alternatives. The beneficiary seats all have mobile or arbitrage exit; the victim seats all have trapped or identity-locked exit. This structural asymmetry is what drives high effective extraction (χ) for victims and low/negative χ for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to establish stable, territorially coterminous religious organization—to solve the problem of overlapping papal, imperial, and local religious authority creating chronic conflict. This mandate is achieved and remains live throughout the interval (1500–1650). The founding problem (jurisdictional conflict, religious heterogeneity causing instability) is addressed by the constraint itself: by 1620, territorial religious settlement is the new normal, and cross-territory jurisdictional disputes over religion have been substantially reduced (though religious wars persist within the framework of territorial religion). The constraint does not exhibit mandatrophy in this reading—the mandate has not outlived its function. However, the theater ratio rises from 1530–1595, suggesting that the rhetorical justification (theological innovation as sovereignty mechanism) becomes increasingly performative as the constraint institutionalizes. By 1650, the theater ratio plateaus, indicating that the performance of the constraint becomes normalized routine rather than novel justification. The constraint is NOT a piton (mandatrophic degradation) because the coordination function remains live and extraction remains high; it would become piton if territorial rulers stopped benefiting from the arrangement (e.g., if nation-states had transcended the need for territorial religious homogeneity) while still maintaining the enforcement machinery. No evidence for that in this interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Is the Reformation fundamentally a political realignment using religious differentiation as the vehicle, or is it fundamentally a theological fragmentation with political realignment as the consequence?',
    'Counterfactual analysis: if the theological innovations (Luther, Zwingli, Calvin''s doctrines) had emerged but printing technology had not existed, would territorial rulers have adopted them as sovereignty mechanisms? Conversely, if printing technology had existed and local theological dissent had spread but rulers had not found sovereignty benefit in religious differentiation, would the Reformation have persisted? The reading that explains a larger proportion of observed political realignment without the other conditions is more plausible as the primary structural fact.',
    'If political realignment is primary, this reading stands; extraction is fundamentally driven by sovereignty competition and religious differentiation is the mechanism. If theological fragmentation is primary, the constraint''s ε should be lower and the beneficiaries should be reformed theologians rather than territorial rulers; the type might shift to rope (genuine theological coordination) rather than tangled_rope (coordination + extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Whether political realignment or theological innovation is the primary structural driver of the Reformation''s emergence and persistence.').

omega_variable(
    reading_sibling_foreclosure,
    'Do the political_realignment, technological_mediation, and theological_fragmentation readings coexist as multiple valid interpretations of the same kernel, or does one reading''s structural success logically foreclose the others?',
    'Examine whether the three readings make contradictory claims about the causal order and role of key actors (rulers, theologians, printing merchants, clergy). If they assign incompatible causal primacy (e.g., political actors are primary drivers in reading 1, but secondary to technological agents in reading 2), and if one reading''s assigned primacy renders the other''s secondary role incoherent, foreclosure exists. If they can all be true simultaneously under different levels of analysis (political dynamics at one level, technological dynamics at another, theological dynamics at a third), they coexist.',
    'If readings coexist, they should all be authored and linked as a constraint family, each with separate stories. If one forecloses others, the foreclosing reading is the primary constraint and the foreclosed readings should not be authored independently (or should be authored as refuted hypotheses). The corpus design and analysis strategy depend on this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Whether the three Reformation readings are logically compatible or whether one logically rules out the others.').

omega_variable(
    extraction_magnitude_verification,
    'How much of territorial rulers'' political gain from the Reformation comes from religious differentiation specifically (and how much could have been achieved through other mechanisms of sovereignty assertion)?',
    'Comparative institutional analysis: examine cases where rulers asserted sovereignty without religious differentiation (e.g., consolidation of feudal fragmentation into centralized monarchy in non-religious domains) and cases where rulers attempted to use other legitimacy mechanisms instead of religious differentiation (e.g., claims to Roman law, natural law, divine right independent of theological innovation). Measure the speed and stability of sovereignty consolidation across these cases.',
    'If rulers achieved comparable sovereignty gains through non-religious mechanisms, the extraction attributed to religious differentiation is overstated, and the constraint''s ε should be lowered. If religious differentiation provided unique acceleration and stability to sovereignty consolidation, ε is correctly measured or understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_magnitude_verification, empirical, 'Whether religious differentiation was uniquely efficacious for sovereignty assertion or whether equivalent political realignment could have been achieved through other mechanisms.').

omega_variable(
    suppression_mechanism_structural_internalized,
    'Is the measured suppression (0.71 at interval end) structural (legal barriers, inquisition machinery, exile threat) or internalized (moral conviction, identity fusion, self-imposed conformity born from acceptance of reformed doctrine)?',
    'Post-reform-establishment trajectory analysis: in territories where reformed religion was established and the inquisition machinery subsequently weakened (e.g., 17th-century Protestant regions where heresy prosecution declined), does the suppression of competing religious voices persist? If suppression persists after structural enforcement machinery is withdrawn, the suppression is partially internalized. If suppression collapses when enforcement is withdrawn, it was primarily structural.',
    'If suppression is partially internalized, the effective suppression experienced by victims and excluded minorities is higher than the institutional machinery alone suggests—moral capture is self-perpetuating. If purely structural, the constraint could be changed by dismantling enforcement machinery without addressing ideological commitment. The distinction affects the cost and difficulty of constraint reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_internalized, empirical, 'Whether suppression is driven by structural enforcement mechanisms or by internalized moral commitment to reformed religion.').

omega_variable(
    kernel_reading_authorship_bias,
    'Is the political_realignment reading a description of objective structural fact, or is it a reading authored from the vantage point of historians who benefit from the nation-state system that the constraint established?',
    'Meta-historical analysis: examine the political positioning of historians who endorse the political_realignment reading versus historians who endorse theological_fragmentation or technological_mediation readings. Assess whether the distribution of reading endorsement correlates with historians'' institutional embeddedness in nation-states (which benefit from the constraint''s legitimacy) versus international or post-national scholarly communities. Do historians in post-national institutions (international law, global ethics, continental philosophy) endorse different readings than historians in national academies?',
    'If the reading''s plausibility correlates with historians'' structural position relative to the constraint, the reading may be partially self-serving rather than objectively descriptive. This does not falsify the reading, but it signals that the reading is more empirically contaminated than a reading endorsed across diverse institutional positions. The ε measurement and type classification would need to account for possible observational bias.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_authorship_bias, conceptual, 'Whether the political realignment reading reflects objective structural fact or whether it is a beneficiary-authored narrative vindicating the nation-state system it describes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1500, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1500, reformation_composite__political_realignment_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement_basis(refo_tr_t1500, projected).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.28).
narrative_ontology:measurement_basis(refo_tr_t1530, observed).
narrative_ontology:measurement(refo_tr_t1560, reformation_composite__political_realignment_reading, theater_ratio, 1560, 0.38).
narrative_ontology:measurement_basis(refo_tr_t1560, observed).
narrative_ontology:measurement(refo_tr_t1595, reformation_composite__political_realignment_reading, theater_ratio, 1595, 0.44).
narrative_ontology:measurement_basis(refo_tr_t1595, observed).
narrative_ontology:measurement(refo_tr_t1620, reformation_composite__political_realignment_reading, theater_ratio, 1620, 0.43).
narrative_ontology:measurement_basis(refo_tr_t1620, observed).
narrative_ontology:measurement(refo_tr_t1650, reformation_composite__political_realignment_reading, theater_ratio, 1650, 0.42).
narrative_ontology:measurement_basis(refo_tr_t1650, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1500, reformation_composite__political_realignment_reading, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement_basis(refo_be_t1500, projected).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.52).
narrative_ontology:measurement_basis(refo_be_t1530, observed).
narrative_ontology:measurement(refo_be_t1560, reformation_composite__political_realignment_reading, base_extractiveness, 1560, 0.64).
narrative_ontology:measurement_basis(refo_be_t1560, observed).
narrative_ontology:measurement(refo_be_t1595, reformation_composite__political_realignment_reading, base_extractiveness, 1595, 0.71).
narrative_ontology:measurement_basis(refo_be_t1595, observed).
narrative_ontology:measurement(refo_be_t1620, reformation_composite__political_realignment_reading, base_extractiveness, 1620, 0.7).
narrative_ontology:measurement_basis(refo_be_t1620, observed).
narrative_ontology:measurement(refo_be_t1650, reformation_composite__political_realignment_reading, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement_basis(refo_be_t1650, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1500, reformation_composite__political_realignment_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement_basis(refo_su_t1500, projected).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.54).
narrative_ontology:measurement_basis(refo_su_t1530, observed).
narrative_ontology:measurement(refo_su_t1560, reformation_composite__political_realignment_reading, suppression_requirement, 1560, 0.65).
narrative_ontology:measurement_basis(refo_su_t1560, observed).
narrative_ontology:measurement(refo_su_t1595, reformation_composite__political_realignment_reading, suppression_requirement, 1595, 0.74).
narrative_ontology:measurement_basis(refo_su_t1595, observed).
narrative_ontology:measurement(refo_su_t1620, reformation_composite__political_realignment_reading, suppression_requirement, 1620, 0.73).
narrative_ontology:measurement_basis(refo_su_t1620, observed).
narrative_ontology:measurement(refo_su_t1650, reformation_composite__political_realignment_reading, suppression_requirement, 1650, 0.71).
narrative_ontology:measurement_basis(refo_su_t1650, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.18).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% The 'reformation_composite' kernel is decomposed into three constraint stories, each instantiating a different structural reading of the Reformation event. This story (political_realignment_reading) emphasizes political realignment and sovereignty assertion. The sibling stories (theological_fragmentation_reading and technological_mediation_reading) emphasize incompatible doctrinal commitments and printing-press-enabled mass distribution, respectively. All three share the same kernel (the Reformation as a historical event) and span the same interval (1500–1650), but they measure different ε values and instantiate different beneficiary/victim structures. Each reading is a structurally valid constraint; the readings coexist as competing interpretations rather than being logically foreclosed by each other. The political_realignment reading influences the other two: the success of political realignment created the conditions under which theological fragmentation became institutionally stable and printing press technology could be deployed to distribute reformed theology at continental scale. Sibling stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__political_realignment_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
