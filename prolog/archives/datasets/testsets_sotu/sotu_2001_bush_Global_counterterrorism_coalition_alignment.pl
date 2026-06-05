% ============================================================================
% CONSTRAINT STORY: sotu_2001_bush_Global_counterterrorism_coalition_alignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2001_bush_Global_counterterrorism_coalition_alignment, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_2001_bush_Global_counterterrorism_coalition_alignment
 *   human_readable: Global Counterterrorism Coalition Alignment (2001 SOTU Framework)
 *   domain: foreign_policy/international_security
 *
 * SUMMARY:
 *   The September 2001 SOTU speech established a structural constraint that
 *   reframed international relations around a unified counterterrorism
 *   objective: a coalition of 80+ nations united against a shared enemy. This
 *   constraint serves two simultaneous functions: genuine multilateral
 *   coordination on counterterrorism intelligence and military operations,
 *   and asymmetric extraction of policy alignment that benefits U.S.
 *   strategic positioning in bilateral relationships. The constraint carries
 *   the legitimacy label ('global coalition,' 'unified response') while
 *   encoding extractive mechanisms (aid conditionality for policy alignment,
 *   surveillance integration under U.S. oversight, targeting authority
 *   concentrated in U.S. hands despite multilateral rhetoric). The theater
 *   ratio (0.64) reflects the gap between the coalition's performative
 *   legitimacy and its actual operational structure — states are expected to
 *   participate in the symbolic register (pledging commitment, joining public
 *   coalitions) while U.S. strategic agencies retain decision authority in
 *   the operational register. For developing nations, the suppression
 *   mechanism operates through material dependence (military aid, security
 *   assistance, IMF coordination) that makes exit nominally possible but
 *   economically catastrophic. For allied states, suppression operates
 *   through institutional inertia (NATO/Five Eyes integration, bilateral
 *   security relationships) and the diplomatic cost of breaking coalition
 *   solidarity. The constraint exhibits genuine tangled-rope characteristics:
 *   coordination function is real (multilateral intelligence-sharing does
 *   improve counterterrorism capability), but it is fused with asymmetric
 *   extraction (policy alignment, surveillance integration, targeting
 *   priority favor U.S. preferences). The measurement trajectory shows
 *   extractiveness rising from 2001 to 2006 (peak extraction during Iraq
 *   escalation and intelligence expansion) then stabilizing as coalition
 *   members negotiated constraints on U.S. unilateralism and public
 *   legitimacy crisis deepened (Abu Ghraib, Guantánamo disclosure,
 *   extraordinary rendition exposure). Theater ratio peaks at 0.68 around
 *   2006–2008 (maximum symbolic commitment vs. actual divergence in partners'
 *   preferences on Iraq and detention) then declines slightly as transparency
 *   about intelligence-sharing and drone operations increases, reducing the
 *   gap between stated and actual function.
 *
 * KEY AGENTS:
 *   - United States Strategic Apparatus: Primary beneficiary (institutional/arbitrage) — converts bilateral relationships into unified framework justifying integrated military, intelligence, law enforcement operations; retains operational authority while claiming multilateral legitimacy
 *   - NATO Allies (UK, Western Europe): Powerful constrained beneficiaries (powerful/constrained) — gain intelligence-sharing, collective defense framing, but pressured to align security policies with U.S. doctrine; exit is costly but possible
 *   - Developing Nation Signatories (Pakistan, Egypt, Saudi Arabia, Indonesia, etc.): Moderate victims (moderate/constrained) — face material constraints (aid conditionality, security assistance dependence) that make exit economically catastrophic; experience asymmetric policy alignment extraction
 *   - Global Counterterrorism Legitimacy: Powerless victim (powerless/trapped) — abstract commons that bears the cost as coalition framework becomes cover for extractive geopolitical actions; cannot exit or organize
 *   - Civil Liberties Protections (across signatory states): Powerless victim (powerless/trapped) — surveillance and detention policies adopted under coalition pressure violate domestic constitutional constraints in signatory nations; citizens lack exit options
 *   - International Legal Institutions (ICJ, UN Security Council): Degraded institutional actor (institutional/arbitrage) — maintain theatrical form of legitimacy review while actual enforcement capacity erodes; coalition framework bypasses these institutions entirely
 *   - Muslim-Majority Signatory States: Organized victims with coordination benefits (organized/constrained) — experience both genuine counterterrorism coordination gains and significant extraction of policy alignment; face internal legitimacy challenges from appearing to align with U.S. regional interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2001_bush_Global_counterterrorism_coalition_alignment, 0.52).
domain_priors:suppression_score(sotu_2001_bush_Global_counterterrorism_coalition_alignment, 0.58).
domain_priors:theater_ratio(sotu_2001_bush_Global_counterterrorism_coalition_alignment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2001_bush_Global_counterterrorism_coalition_alignment, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_2001_bush_Global_counterterrorism_coalition_alignment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_2001_bush_Global_counterterrorism_coalition_alignment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2001_bush_Global_counterterrorism_coalition_alignment, tangled_rope).
narrative_ontology:human_readable(sotu_2001_bush_Global_counterterrorism_coalition_alignment, "Global Counterterrorism Coalition Alignment (2001 SOTU Framework)").
narrative_ontology:topic_domain(sotu_2001_bush_Global_counterterrorism_coalition_alignment, "foreign_policy/international_security").

domain_priors:requires_active_enforcement(sotu_2001_bush_Global_counterterrorism_coalition_alignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2001_bush_Global_counterterrorism_coalition_alignment, united_states_strategic_positioning).
narrative_ontology:constraint_beneficiary(sotu_2001_bush_Global_counterterrorism_coalition_alignment, major_power_security_agencies).
narrative_ontology:constraint_victim(sotu_2001_bush_Global_counterterrorism_coalition_alignment, global_counterterrorism_legitimacy).
narrative_ontology:constraint_victim(sotu_2001_bush_Global_counterterrorism_coalition_alignment, developing_nation_sovereignty).
narrative_ontology:constraint_victim(sotu_2001_bush_Global_counterterrorism_coalition_alignment, civil_liberties_protections).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUNTERTERRORISM LEGITIMACY COMMONS (SNARE) — The abstract global interest in legitimate counterterrorism frameworks cannot exit or organize. The coalition framework captures the legitimacy label ('global unity against terrorism') while encoding extractive geopolitical priorities (U.S. bilateral realignment, intelligence integration favoring U.S. preferences, targeting of non-U.S. adversaries). The commons bears the cost as the constraint becomes cover for extractive actions labeled 'counterterrorism.' Maximum extraction with no exit mechanism.
constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EUROPEAN ALLIED STATES (TANGLED ROPE) — NATO allies experience genuine coordination benefits (intelligence-sharing, military interoperability, collective defense framing) alongside significant constraints (pressure to adopt U.S. security definitions, surveillance and detention policies, and intelligence-sharing agreements that reduce their own intelligence autonomy). Exit is costly (NATO damage, bilateral U.S. relationship harm) but possible at high price. Moderate extractiveness with real coordination function.
constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: DEVELOPING NATION SIGNATORIES (SNARE) — States joining the coalition face material constraints (aid conditionality, security assistance dependence, IMF/World Bank policy alignment pressures) that make exit nominally possible but economically catastrophic. Suppression is high: deviation from U.S.-defined counterterrorism policy risks sanctions, aid cutoff, and diplomatic isolation. These states also lack capacity to influence coalition framing — their signatures are extracted as consent for policies they did not design. Experienced extraction is severe relative to their power.
constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: UNITED STATES STRATEGIC APPARATUS (ROPE) — The U.S. government experiences the coalition as a coordination mechanism: it solves the problem of converting bilateral relationships into a unified multilateral framework that justifies integrated military, intelligence, and law enforcement operations. The coalition also provides arbitrage opportunity — the U.S. can pursue bilateral relationships while using coalition legitimacy to justify policies favored by only a subset of members. Net beneficiary with low or negative experienced extraction.
constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL INSTITUTIONS (PITON) — The International Court of Justice, UN Security Council, and international humanitarian law frameworks maintain the theatrical form of legitimacy review while their actual enforcement capacity has degraded. The coalition framework explicitly bypasses these institutions (bilateral agreements supersede international law review; classified intelligence operations are exempt from disclosure). These institutions persist through bureaucratic inertia and their role in the theater of legitimacy, not through functional authority. Theater ratio rises as institutions validate operations their mandate would normally constrain.
constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MUSLIM-MAJORITY NATION PARTICIPANTS (TANGLED ROPE) — These states (Pakistan, Saudi Arabia, Egypt, Turkey, Indonesia, and others) experience the coalition as a coordinating mechanism with asymmetric extraction. They benefit from counterterrorism cooperation (shared intelligence on extremist networks, capacity building, security assistance) and face pressure to align internal security policies with U.S. definitions of terrorism. The constraint extracts asymmetric policy alignment (e.g., Pakistan's intelligence services operating under U.S. oversight of target selection). Exit is highly constrained — leaving the coalition risks loss of military aid, sanctions, and regional isolation. Genuine coordination with significant extraction.
constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE CONFIRMED) — From civilizational scope, the constraint decomposes into two coordinated functions: (1) genuine multilateral coordination on counterterrorism intelligence and military operations, and (2) asymmetric extraction of policy alignment and surveillance integration favoring U.S. strategic objectives. Both are real and necessary to explain the structure. The theater_ratio (0.64) reflects the gap between the coalition's stated legitimacy (unified response to shared terrorist threat) and its actual function (bilateral relationships reframed as multilateral to amplify U.S. leverage). The constraint is not a false summit — it is genuinely tangled.
constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2001_bush_Global_counterterrorism_coalition_alignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2001_bush_Global_counterterrorism_coalition_alignment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2001_bush_Global_counterterrorism_coalition_alignment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2001_bush_Global_counterterrorism_coalition_alignment, TR),
    TR >= 0.70.

:- end_tests(sotu_2001_bush_Global_counterterrorism_coalition_alignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The coalition extracts asymmetric policy alignment from developing states (whose security definitions are overridden by U.S. preferences), surveillance integration that reduces state intelligence autonomy (particularly for Five Eyes members), and operational decision-making authority concentrated in U.S. hands despite multilateral rhetoric. However, the extraction is not total because genuine multilateral coordination gains exist — intelligence-sharing does improve collective capability against non-state terrorist networks, and signatory states do retain some operational veto authority in their own territory. Suppression (0.58): Moderate-high. Material barriers for developing states (aid conditionality, security assistance dependence, sanctions risk, regional isolation) make exit possible but economically catastrophic. Institutional inertia for allied states (NATO integration, Five Eyes, bilateral security relationships, diplomatic costs of breaking solidarity) creates high suppression without material coercion. Theater ratio (0.64): Moderate-high. The coalition's symbolic legitimacy (unified global response to shared enemy, multilateral consensus) diverges significantly from actual operational function (U.S. unilateral decision authority, intelligence integration favoring U.S. targeting priorities, policy alignment extracted through material pressure). The theater increases over time as coalition operations diverge from counterterrorism (Iraq invasion launched under coalition banner but opposed by many signatories; drone campaigns in Yemen, Pakistan, Somalia integrated into coalition framework despite limited multilateral authorization). Claimed type (Tangled Rope): The constraint exhibits both coordination (genuine multilateral intelligence-sharing, military interoperability, collective defense framing) and extraction (asymmetric policy alignment, surveillance integration reducing state autonomy, targeting authority concentrated in U.S. hands). Active enforcement is required — the coalition framework explicitly requires member states to adopt compatible security policies, intelligence-sharing agreements, and participate in joint operations. Beneficiaries are primary (U.S. strategic positioning, major power security agencies that gain integrated surveillance and operational authority). Victims are multiple (global counterterrorism legitimacy being captured, developing nations' policy autonomy being extracted, civil liberties protections being eroded by cascade of surveillance and detention policies).
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates stark perspectival divergence across five of the seven perspectives. The U.S. apparatus sees coordination (Rope) — the coalition solves the problem of converting bilateral relationships into a unified framework. NATO allies see mixed coordination and extraction (Tangled Rope) — genuine intelligence benefits alongside pressure to align with U.S. preferences. Developing nation signatories see severe extraction (Snare) — material barriers make exit impossible without catastrophic cost, and policy authority is asymmetrically distributed. The global legitimacy commons sees pure extraction (Snare) — the coalition label captures legitimacy while encoding extractive operations that would otherwise violate international norms. International legal institutions see their own degradation (Piton) — the coalition framework explicitly bypasses these institutions, and they maintain their theatrical authority while actual enforcement capacity disappears. Muslim-majority signatories occupy a unique position (Tangled Rope) — they experience genuine counterterrorism benefits alongside significant pressure to align internal security policies with U.S. preferences, and they face additional internal legitimacy challenges from appearing aligned with U.S. regional interests. The analytical observer at civilizational scope confirms the tangled-rope classification — the constraint is genuinely hybrid, not a false summit or a pure extortion mechanism. The perspectival gap reveals that the coalition's success as an extraction mechanism depends on the U.S.'s capacity to maintain the coordination benefits as a sufficient cover for the asymmetric policy alignment it extracts.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to extraction flows. The U.S. strategic apparatus appears as beneficiary + arbitrage exit → low d (approximately 0.10–0.15) → negative effective extraction (the constraint subsidizes this agent by converting bilateral relationships into a unified framework). NATO allies appear as powerful + constrained exit + mixed coordination/extraction → moderate d (approximately 0.45–0.55) → moderate effective extraction. Developing nation signatories appear as moderate + constrained exit + victim status → high d (approximately 0.70–0.80) → high effective extraction despite their nominal power level. The global legitimacy commons appears as powerless + trapped + victim status → maximum d (approximately 0.95) → maximum effective extraction. International legal institutions appear as institutional + arbitrage exit but degraded actual authority → moderate d (approximately 0.55–0.65) for purposes of measuring their experienced extraction, though their theater_ratio spike (0.68–0.75) indicates that experienced extraction is masked by the symbolic authority they perform. The directionality overrides are not used here because the baseline power atoms and exit options already capture the structural differentiation accurately — developing states' moderate power is constraint-relative (globally they have institutional standing; within this specific constraint they are materially dependent), and the exit options (constrained vs. arbitrage) follow directly from observable barriers to policy deviation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by exhibiting genuine hybridity: both coordination and extraction are structurally real and necessary to explain its operation. This is not a case where one type is correct and others are observer bias. Rather, the constraint accomplishes its political function precisely because it fuses coordination (real intelligence benefits, genuine military interoperability) with extraction (asymmetric policy alignment, surveillance integration, targeting authority concentration). The U.S. apparatus maximizes extraction by maintaining the coordination function — if the coalition became pure theater (Piton), signatory states would exit; if it became pure coordination (Rope), the U.S. could not extract asymmetric policy alignment. The theater ratio (0.64) indicates that the constraint is not yet at the Piton threshold (0.70+), meaning the coordination function remains sufficiently credible to sustain participation despite the extraction. The measurement trajectory showing theater rising to 0.68 around 2006–2008 (Iraq escalation, Abu Ghraib disclosure) and then declining suggests that the coalition approached Piton vulnerability — legitimacy erosion threatened the viability of the coordination cover for extraction. The subsequent partial recovery of the coalition's legitimacy (through transparency initiatives, normalization of drone operations, intelligence-sharing maturation) kept theater below the Piton gate. The constraint is classified Tangled Rope because it requires active enforcement (member states must adopt compatible security policies), exhibits both beneficiaries (U.S., major power security agencies) and victims (developing states, civil liberties, global counterterrorism legitimacy), and sustains itself through the fusion of genuine coordination with asymmetric extraction. No reclassification to Snare is warranted despite severe impacts on developing states, because the coordination benefits are real enough that some signatories would choose to participate even without the material pressure (though fewer would). Conversely, no reclassification to Rope is warranted because the extraction mechanism is not epiphenomenal — it is a primary driver of signatory behavior and policy alignment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_dominance_decomposition,
    'What proportion of the coalition''s extractiveness represents asymmetric alignment of developing states'' policies versus genuine coordination of multilateral counterterrorism operations?',
    'Structural comparison: (a) intelligence-sharing volume and bidirectionality by state pair, (b) targeting authority distribution (who decides target lists and operational theaters?), (c) security policy divergence between signatory states and U.S. doctrine, measured from public statements and disclosed classified reviews',
    'If coordination > extraction: reclassify as Rope from developing state perspectives. If extraction > coordination: reclassify as pure Snare. The constraint''s type is sensitive to this decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_dominance_decomposition, empirical, 'Genuine coordination vs. asymmetric policy extraction decomposition').

omega_variable(
    legitimacy_capture_mechanism,
    'Does the coalition framework legitimize unilateral U.S. military operations that would otherwise violate international law, or does it facilitate genuinely multilateral decision-making?',
    'Analysis of operational decisions: (a) which coalition members have veto authority over military operations in their own territory?, (b) how many military operations occurred without prior consent from affected host nations?, (c) did the coalition framework enable operations that violated the affected nation''s own constitutional constraints on foreign military activity?',
    'If legitimacy is captured: theater_ratio rises to 0.75+, classification shifts toward Piton for international legal institutions. If legitimacy is genuine: theater_ratio drops to 0.45–0.50, coalition functions as Rope rather than Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_capture_mechanism, empirical, 'Whether coalition framework legitimizes unilateral operations or enables multilateral governance').

omega_variable(
    terrorism_definition_asymmetry,
    'Who has authority to define what counts as terrorism within the coalition? Can signatory states designate other signatories'' security operations as terrorism and trigger coalition response?',
    'Examination of coalition doctrine and operational precedent: (a) have any signatory states successfully invooked the coalition to constrain another signatory''s security policy?, (b) does the U.S. retain unilateral designation authority?, (c) what happens when coalition members disagree on targeting (e.g., U.S. vs. Pakistan on Taliban factions)?',
    'If definition authority is symmetric: coalition is genuine multilateral coordination (Rope). If U.S. retains de facto authority: definition becomes an extraction mechanism (suppression mechanism that prevents dissenting states from reframing U.S. operations as counter-terrorism violations). Affects assessment of suppression score.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(terrorism_definition_asymmetry, empirical, 'Whether terrorism definition is symmetrically negotiated or U.S. dominated').

omega_variable(
    civil_liberties_contamination,
    'To what extent does the coalition framework enable participating states to adopt U.S.-model surveillance and detention policies (reduced due process, extended detention, reduced transparency) that they would not have adopted without coalition pressure?',
    'Comparative analysis: (a) did signatory states expand surveillance or detention authority after joining the coalition?, (b) do classified bilateral intelligence agreements require signatory states to adopt U.S.-compatible security policies as condition of participation?, (c) can domestic courts in signatory states enforce human rights constraints against coalition-framed counterterrorism operations?',
    'If civil liberties contamination is high: victims group includes ''civil liberties protections'' across signatory states; extractiveness should rise to 0.60+. If low: constraining extractiveness might drop to 0.40. This directly affects the base metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_liberties_contamination, empirical, 'Coalition framework''s role in civil liberties degradation across signatory states').

omega_variable(
    strategic_interest_alignment,
    'To what degree are signatory states'' genuine terrorism concerns aligned with U.S. strategic interests in a particular region, and where they diverge, which definition wins?',
    'Conflict analysis: (a) identify cases where coalition partners opposed U.S. counterterrorism operations in their region or allied regions, (b) what happened to those disagreements? Did partners accept U.S. framing or force revision?, (c) did the U.S. ever withdraw operations due to partner objection? If never, who has structural authority?',
    'If alignment is high: coalition functions as genuine coordination (lower extractiveness). If divergence is high and U.S. preferences systematically override partners: extractiveness rises, suppression deepens, coalition becomes extraction mechanism masquerading as coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_interest_alignment, empirical, 'Strategic interest alignment and authority distribution in coalition decision-making').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2001_bush_Global_counterterrorism_coalition_alignment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwot_coal_tr_t0, sotu_2001_bush_Global_counterterrorism_coalition_alignment, theater_ratio, 0, 0.5).
narrative_ontology:measurement(gwot_coal_tr_t3, sotu_2001_bush_Global_counterterrorism_coalition_alignment, theater_ratio, 3, 0.61).
narrative_ontology:measurement(gwot_coal_tr_t6, sotu_2001_bush_Global_counterterrorism_coalition_alignment, theater_ratio, 6, 0.68).
narrative_ontology:measurement(gwot_coal_tr_t10, sotu_2001_bush_Global_counterterrorism_coalition_alignment, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(gwot_coal_be_t0, sotu_2001_bush_Global_counterterrorism_coalition_alignment, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gwot_coal_be_t3, sotu_2001_bush_Global_counterterrorism_coalition_alignment, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(gwot_coal_be_t6, sotu_2001_bush_Global_counterterrorism_coalition_alignment, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(gwot_coal_be_t10, sotu_2001_bush_Global_counterterrorism_coalition_alignment, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2001_bush_Global_counterterrorism_coalition_alignment, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2001_bush_Global_counterterrorism_coalition_alignment, us_surveillance_infrastructure_global_integration).
narrative_ontology:affects_constraint(sotu_2001_bush_Global_counterterrorism_coalition_alignment, developing_nation_security_assistance_conditionality).
narrative_ontology:affects_constraint(sotu_2001_bush_Global_counterterrorism_coalition_alignment, international_humanitarian_law_enforcement_degradation).

% DUAL FORMULATION NOTE:
% The global counterterrorism coalition is a constraint family decomposable into at least three structurally distinct constraints: (1) multilateral intelligence-sharing coordination (genuine Rope), (2) U.S. bilateral pressure on developing states to align security policies (Snare from developing state perspective), and (3) erosion of international legal institutions' authority to review security operations (Piton for legal institutions). The single story captures all three simultaneously because they are operationally fused — the coalition framework's legitimacy depends on maintaining the appearance of multilateral coordination while enabling the bilateral extraction and the institutional bypass. Separate stories would artificially decompose what is actually integrated. The network edges reflect downstream constraints that depend on this constraint's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
