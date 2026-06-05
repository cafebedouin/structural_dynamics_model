% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Authority: Magistrate Reading (Ancient Liberty Preservation)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   The magistrate reading of remonstrance authority constructs the
 *   Parlements of France as constitutional guardians of 'ancient liberties'
 *   against arbitrary royal innovation in fiscal, judicial, and
 *   administrative domains. From the magistrate institutional perspective,
 *   remonstrance represents a genuine coordination mechanism: the magistracy
 *   preserves fundamental constitutional order by requiring the crown to
 *   justify new edicts against established law and precedent. This reading
 *   grounds itself in a claimed genealogy connecting Parlement authority to
 *   medieval assemblies and foundational charters, presenting magistrate veto
 *   power as immutable constitutional duty rather than institutional
 *   privilege. However, the structural data reveals a tangled hybrid: the
 *   remonstrance authority does coordinate genuine constitutional constraint
 *   (preventing wholly arbitrary royal action), but it simultaneously
 *   extracts through systematic obstruction of fiscal reforms that would
 *   burden the magistrate and noble tax-exempt classes while constraining
 *   reforms that would expand the common tax base. The magistracy's doctrine
 *   of ancient liberty becomes a mechanism for preserving fiscal privilege
 *   while delegitimizing crown administrative rationalization as 'arbitrary
 *   innovation.' The measurements track the constraint's evolution from
 *   1650–1789: extractiveness rises during the centralizing reigns of Louis
 *   XIV and Louis XV (0.35 → 0.61) as royal fiscal pressure increases and
 *   magistrate obstruction becomes more organized and selective; theater
 *   ratio rises as the constitutional doctrine becomes increasingly
 *   performative (crown overrides via lit de justice, magistracy remonstrates
 *   and delays but ultimately yields, the cycle repeats); suppression peaks
 *   at 1750 (0.68) when magistrate-coordinated resistance is highest (Maupeou
 *   crisis looms), then slightly declines by 1789 (0.62) as Revolutionary
 *   transformation reshapes the entire institutional framework. The
 *   magistrate reading is one reading of the 'remonstrance_authority'
 *   kernel—a contested commitment that the crown reading interprets as
 *   aristocratic obstruction rather than constitutional preservation.
 *
 * KEY AGENTS:
 *   - Parlementary Magistracy: Primary beneficiary (institutional/arbitrage) — extracts through fiscal exemptions and veto leverage; experiences remonstrance as constitutional coordination
 *   - Tax-Exempt Nobility: Aligned beneficiary (powerful/mobile) — benefits from magistrate obstruction of fiscal reforms that would burden noble estates
 *   - Crown Authority: Simultaneously beneficiary and victim (powerful/mobile) — seeks fiscal and administrative reform but constrained by remonstrance delays; can override but at political cost
 *   - Commoner Tax Base: Primary victim (powerless/trapped) — bears fiscal burden while locked out of remonstrance proceedings; extraction rises as magistrates obstruct commoner-focused reforms
 *   - Parlement Collective (Organized): Organized resistance formation (organized/constrained) — when magistrates coordinate across parlements, remonstrance becomes collective extraction mechanism with escalating demands
 *   - Ancient Liberty Doctrine: Institutional practice (institutional/constrained) — the remonstrance ritual persists through inertia; increasingly performative as theatre_ratio rises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.58).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.62).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Authority: Magistrate Reading (Ancient Liberty Preservation)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, 'b21b9422-1780-46fa-98a4-b4d4ab01daa3').
narrative_ontology:cs_kernel_codification('b21b9422-1780-46fa-98a4-b4d4ab01daa3', fixed_text).
narrative_ontology:cs_authority_grounding('b21b9422-1780-46fa-98a4-b4d4ab01daa3', lineage).
narrative_ontology:cs_interpretation_layer_present('b21b9422-1780-46fa-98a4-b4d4ab01daa3').
narrative_ontology:cs_reading_relation('b21b9422-1780-46fa-98a4-b4d4ab01daa3', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('b21b9422-1780-46fa-98a4-b4d4ab01daa3', foundational, remonstrance_preserves_ancient_liberties).
narrative_ontology:cs_axiom_status(remonstrance_preserves_ancient_liberties, holdable).
narrative_ontology:cs_axiom_grounding('b21b9422-1780-46fa-98a4-b4d4ab01daa3', remonstrance_preserves_ancient_liberties, deontological).
narrative_ontology:cs_axiom('b21b9422-1780-46fa-98a4-b4d4ab01daa3', foundational, arbitrary_royal_innovation_threatens_constitutional_order).
narrative_ontology:cs_axiom_status(arbitrary_royal_innovation_threatens_constitutional_order, holdable).
narrative_ontology:cs_axiom_grounding('b21b9422-1780-46fa-98a4-b4d4ab01daa3', arbitrary_royal_innovation_threatens_constitutional_order, deontological).
narrative_ontology:cs_reference_frame('b21b9422-1780-46fa-98a4-b4d4ab01daa3', ancient_constitutional_order_preservation).
narrative_ontology:cs_drift_state('b21b9422-1780-46fa-98a4-b4d4ab01daa3', contemporary_revolutionary_transformation_1789, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('b21b9422-1780-46fa-98a4-b4d4ab01daa3', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlementary_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, tax_exempt_nobility).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, commoner_tax_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The commoner tax base has no exit from the constraint and no voice in remonstrance proceedings. Bears extraction through expanded fiscal authority while magistrates obstruct reform via constitutional doctrine. Trapped between crown demands and magistrate gatekeeping.
constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The magistracy experiences remonstrance primarily as coordination: preserving constitutional balance between crown innovation and ancient liberty. Institutional beneficiary with arbitrage (can exit via accommodation). Extraction runs toward them through tax exemptions and veto power, but also coordinates real constitutional function.
constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% The crown is simultaneously beneficiary (seeks fiscal reform, trade regulation, administrative rationalization) and victim (constrained by remonstrance delays and obstruction). Mobile exit (can override via lit de justice or force) but also coordinated with magistracy on genuine constitutional maintenance. Mixed experience of extraction and coordination.
constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Remonstrance as a constitutional practice is largely performative by the 18th century. The magistracy invokes ancient liberty, the crown overrides via lit de justice, and actual reform proceeds—the remonstrance delays but does not prevent. The doctrine persists through institutional inertia, not functional necessity. Theater ratio reflects the growing gap between constitutional claim and political reality.
constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% When magistrates coordinate across parlements (1770s resistance to Maupeou reforms, 1787-88 pre-Revolution mobilization), remonstrance becomes organized extraction—coordinated obstruction of reform while claiming constitutional duty. Organized power with constrained exit (can be exiled or reformed but cannot abandon institutional role). Genuine coordination function (preventing arbitrary royal action) combined with asymmetric extraction (tax exemptions, veto leverage).
constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From the magistrate reading's own tradition, ancient liberty appears as an immutable constitutional principle—written into France's fundamental law, discovered not invented, constraining all legitimate authority. This perspective risks treating the magistrate reading's doctrinal commitments as natural laws of political order. The structural data reveals this as a false summit: the magistracy constructed the doctrine to preserve fiscal privilege and veto power during a period of declining feudal authority.
constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remonstrance_authority__magistrate_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, TR),
    TR >= 0.70.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The magistrate reading constructs remonstrance as coordination (preserving constitutional order), but the structural outcomes are asymmetric: magistrates extract through veto leverage and fiscal privilege preservation while commoners bear increased taxation. The extractiveness trajectory (0.35 → 0.61 → 0.58) reflects intensifying magistrate obstruction during the 18th century as crown fiscal pressure mounted, peaking when organized parlement resistance was highest (1750, 0.61), then slightly declining by 1789 as institutional collapse approached. This is not maximal Snare extraction (0.72+) because genuine constitutional coordination does occur—the crown cannot act arbitrarily, and remonstrance does constrain some reforms—but the coordination mechanism is asymmetrically distributed: magistrates benefit from the veto mechanism, commoners do not. Suppression (0.62): High. Barriers to circumventing the constraint include the doctrine's deep entrenchment in French legal tradition, magistrate institutional cohesion, and the enormous political cost of overriding the Parlements. However, suppression is not maximal (0.70+) because the crown does possess override mechanisms (lit de justice) and can force compliance through extreme measures (Maupeou reforms of 1768, exile and reformation of magistrates). Theater ratio (0.48): Moderate-low. In the magistrate reading's own framework, remonstrance is substantive constitutional action—the magistracy genuinely believes it is preserving ancient law, and the doctrine has real institutional force. Theater is lower than in a pure Snare because the magistracy's commitment to the doctrine is non-instrumental (they are not cynically performing; they genuinely believe). However, theater rises from 0.32 (1650) to 0.52 (1750) as the institutional reality increasingly diverges from the doctrine: the crown can override via lit de justice, magistrate obstruction delays but does not prevent reform, and the practice becomes increasingly ritualized. The slight decline to 0.48 by 1789 reflects the accelerating institutional collapse—the ritual breaks down as revolutionary pressure overwhelms the old constitutional framework.
 *
 * PERSPECTIVAL GAP:
 *   The magistrate reading generates multiple competing classifications from different structural positions. The commoner tax base sees pure extraction (Snare)—trapped with no voice in remonstrance and bearing increasing fiscal burden. The magistracy sees coordination (Rope)—preserving constitutional order through legitimate veto. The crown sees mixed extraction and coordination (Tangled Rope)—constrained by remonstrance but also coordinated with the magistracy on fundamental order. The organized collective of magistrates sees entrenched extraction (Tangled Rope with rising specificity)—when coordinated across parlements, they exercise organized veto power while maintaining the doctrine's legitimacy claim. The ancient liberty doctrine itself, viewed from the institutional practice perspective, appears degraded and performative (Piton)—the ritual persists through inertia as the crown increasingly overrides. The analytical observer risks seeing remonstrance as an immutable constitutional principle (Mountain)—ancient law constraining all legitimate authority—but the structural data reveals this as a false summit: the magistracy constructed the doctrine to preserve privilege during a period of declining feudal authority and rising fiscal pressure. The perspectival gap reveals that remonstrance's classification depends entirely on whether one accepts the magistrate reading's genealogy of ancient liberty or recognizes it as a doctrinal construction.
 *
 * DIRECTIONALITY LOGIC:
 *   The magistrate reading's directionality computation depends on whether the magistracy is treated as genuine beneficiary (coordination of constitutional order) or as net extraction beneficiary (fiscal privilege + veto leverage). The Tangled Rope classification reflects both roles simultaneously: magistrates do coordinate genuine constitutional function (preventing wholly arbitrary royal action), but they also extract asymmetric benefit (fiscal exemptions + veto power) that falls on commoners. The crown's d value (0.48, moderate target) reflects its mixed position: it benefits from some remonstrance coordination (constraining overmighty magistrates would require greater coercive expenditure without the doctrine's legitimacy) but also bears costs (fiscal reform obstruction, administrative rationalization delays). The commoner tax base d value would be 0.95+ (near-total target)—trapped powerless agent with no exit and bearing full extraction. The magistracy's d value reflects beneficiary status with arbitrage exit: institutional power (can accommodate or resist) and arbitrage options (cooperate with crown or escalate resistance). The piton perspective's d derives from the practice's institutional inertia—the doctrine persists not because it extracts maximum value but because alternatives haven't replaced it.
 *
 * MANDATROPHY ANALYSIS:
 *   The magistrate reading resolves its own mandatrophy by locating the measurement ambiguity at the kernel level. The question is not 'what type is remonstrance?' but 'which reading of the contested kernel (remonstrance_authority) is accurate?' The magistrate reading treats ancient liberty as a genuine constitutional constraint; the crown reading treats it as aristocratic obstruction. These readings coexist because they rest on different historiographical claims about the genealogy of remonstrance doctrine. The Tangled Rope classification (rather than pure Rope or pure Snare) reflects the accurate hybrid: remonstrance does coordinate genuine constitutional function, and it does extract asymmetric benefit. The mandatrophy is resolved by mapping the perspectival gap to the kernel contest: if the magistrate genealogy is correct (ancient liberties are genuine), then remonstrance is primarily coordination with secondary extraction (higher Rope weight). If the crown genealogy is correct (remonstrance is constructed privilege), then remonstrance is primarily extraction disguised as coordination (higher Snare weight). The magistrate reading stands at the Tangled Rope midpoint precisely because both genealogies have some truth: the magistracy did inherit genuine medieval assembly precedents, but they also constructed doctrinal extensions to preserve their privilege during the centralizing period. The constraint cannot collapse to pure Rope or pure Snare without resolving the contested historiography—which the magistrate reading deliberately leaves as omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ancient_liberty_normative_status,
    'Are the magistrates'' invoked ancient liberties genuine constitutional constraints on sovereign power, or post-hoc doctrinal constructions designed to preserve fiscal privilege and institutional veto?',
    'Genealogical analysis of remonstrance doctrine: tracing claimed precedents to their actual historical sources; identifying when practices designated as ''ancient'' were actually formalized or invented; comparing magistrate claims to actual medieval charter language.',
    'If genuine constraints: magistrate reading is justified—remonstrance preserves real constitutional order. If constructions: magistrate reading is extraction mechanism disguised as liberty—the constraint is Snare, not Tangled Rope. The reading relations and axiom status shift from ''holdable'' to ''overridden by historical evidence.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ancient_liberty_normative_status, empirical, 'Genealogy of ancient liberty doctrine: genuine constraint or constructed privilege?').

omega_variable(
    fiscal_extraction_asymmetry_mechanism,
    'Does the magistrate reading''s remonstrance authority systematically protect magistrate tax exemptions and nobility fiscal privilege while constraining crown fiscal reform that would burden the commons?',
    'Comparative analysis of remonstrance outcomes: categorizing by whether edict affected magistrate/noble taxation vs commoner taxation; measuring veto success rates and override delays; analyzing which fiscal reforms were sustained vs which were abandoned after remonstrance.',
    'If systematic asymmetry confirmed: suppression value should rise to 0.70+ (high structural extraction). If asymmetry is negligible: suppression drops to 0.40 (lower structural mechanism). Current assessment (0.62) assumes moderate selectivity; this omega resolves magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_extraction_asymmetry_mechanism, empirical, 'Remonstrance selectivity: does doctrine systematically protect magistrate fiscal privilege?').

omega_variable(
    lit_de_justice_ultimate_authority,
    'Does the crown''s ability to override remonstrance via lit de justice render the magistrate reading''s constitutional claim illusory, or is the override mechanism itself a constitutional safety valve that validates the constraint''s coordination function?',
    'Institutional analysis: does the lit de justice''s availability reduce magistrate obstruction, or does the magistracy''s awareness of the override mechanism actually entrench resistance (making them fight harder when override seems likely)? Compare parlement behavior pre- and post-Maupeou reforms.',
    'If override mechanism validates coordination: suppression and theater_ratio both decrease (magistracy sees it as constitutional negotiation). If override is merely power assertion: theater_ratio rises further, suppression moves toward Snare threshold (0.70+). Current assessment treats lit de justice as coordination safety valve; this omega tests that assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lit_de_justice_ultimate_authority, empirical, 'Lit de justice override: constitutional mechanism or mere power assertion?').

omega_variable(
    committer_frame_kernel_contest,
    'Does the magistrate reading''s core premise (remonstrance as ancient liberty preservation) logically foreclose the crown reading''s premise (remonstrance as aristocratic obstruction of rational reform), or do both readings coexist as live positions held by different institutional actors?',
    'Textual and institutional analysis: mapping which premises are logically contradictory vs which are compatible within a single framework. Can a single institutional order recognize both ''remonstrance preserves ancient constitution'' AND ''remonstrance obstructs necessary fiscal reform'' without internal contradiction?',
    'If foreclosure: reading_relations to crown_reading should be ''forecloses''. If coexistence: ''coexists_with''. If influence: ''influences''. Current authoring assumes ''coexists_with'' (different institutional parties holding incompatible views); this omega tests whether the frameworks actually are mutually exclusive or merely politically opposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_contest, conceptual, 'Kernel contest: do magistrate and crown readings foreclose each other logically?').

omega_variable(
    parlement_coalition_dynamics,
    'To what degree does the magistrate reading''s extraction mechanism depend on coordinated action across multiple parlements, versus extraction that a single parlement could exercise independently?',
    'Historical analysis of resistance episodes: measuring effectiveness of individual parlement remonstrances vs coordinated multi-parlement campaigns (1770s Maupeou crisis, 1787-88 pre-Revolution). Identifying threshold for coalition critical mass.',
    'If coalition-dependent: powerless agents benefit from fragmentation (extraction weakens when magistrates cannot coordinate). Current coal_presence triggers organized power status for the collective perspective; this omega measures actual coordination cost and sustainability. If single-parlement extraction is significant, the ''organized'' perspective should be weighted higher and therapeutic omega added.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parlement_coalition_dynamics, empirical, 'Coalition dependency in magistrate extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1650, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_mag_theater_1650, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(remo_mag_theater_1690, remonstrance_authority__magistrate_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(remo_mag_theater_1750, remonstrance_authority__magistrate_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement(remo_mag_theater_1790, remonstrance_authority__magistrate_reading, theater_ratio, 140, 0.48).

% Extraction over time
narrative_ontology:measurement(remo_mag_extract_1650, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(remo_mag_extract_1690, remonstrance_authority__magistrate_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(remo_mag_extract_1750, remonstrance_authority__magistrate_reading, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(remo_mag_extract_1790, remonstrance_authority__magistrate_reading, base_extractiveness, 140, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(remo_mag_suppression_1650, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(remo_mag_suppression_1750, remonstrance_authority__magistrate_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(remo_mag_suppression_1790, remonstrance_authority__magistrate_reading, suppression_requirement, 140, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, crown_fiscal_authority).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, lit_de_justice_override_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, maupeou_reforms_institutional_dissolution).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel decomposes into two competing readings with significantly different ε values: magistrate_reading (ε=0.58, Tangled Rope) treats remonstrance as genuine constitutional coordination with asymmetric extraction benefiting magistrates; crown_reading (ε=0.72+, Snare) treats remonstrance as constructed aristocratic extraction disguised as constitutional duty. Both readings rest on coherent historiographical claims; neither can be ruled out a priori. The measurements support both readings at different historical moments: magistrate reading is more accurate 1650–1715 (remonstrance genuinely constrains arbitrary royal action); crown reading becomes more accurate 1715–1789 (remonstrance becomes increasingly performative as crown overrides via lit de justice and magistrate obstruction becomes coordinated rent-seeking rather than constitutional constraint). The two constraint files are linked via network.affects_constraints and should be read together as competing interpretations of the same institutional phenomenon. A complete analysis requires understanding both readings and their historical conditions of validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
