% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta's Restraint Authority Absorbed into Parliamentary Sovereignty
 *   domain: constitutional_history/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-sovereignty reading of the
 *   Magna Carta constraint-authority kernel: the charter's substantive
 *   restraints (due process, protection from arbitrary seizure, lawful
 *   judgment) persist not as free-standing higher law but only insofar as
 *   Parliament has absorbed and re-enacted them as ordinary statute (Habeas
 *   Corpus Acts, Bill of Rights 1689, later human-rights legislation). Under
 *   this reading Parliament is the sole successor to the constraint authority
 *   the charter once asserted against King John, and it retains full power to
 *   revise or repeal any charter-descended provision through ordinary
 *   majority legislation. This is a coordination arrangement (a stable,
 *   legitimate, democratically accountable process for defining and updating
 *   fundamental rights) that simultaneously extracts durability from anyone
 *   who lacks majority political power: minorities, unpopular litigants, and
 *   future generations bear the cost of a system where no protection is
 *   permanently entrenched.
 *
 * KEY AGENTS:
 *   - parliamentary_majority: agenda_setter/institutional — inherits and exercises the sole revisory authority over charter-descended rights
 *   - crown_in_parliament: beneficiary/institutional — legitimacy fiction that benefits from foreclosing independent charter authority
 *   - enfranchised_electorate: beneficiary+payer/organized — receives contingent protection, bears the risk of majoritarian reversal
 *   - unrepresented_minorities: payer/powerless — protection only as durable as current majority will
 *   - constitutional_courts: agenda_setter+observer/institutional — apply statute but cannot invoke the charter to override Parliament
 *   - originalist_legal_historians: excluded/moderate — argue for independent charter force, without binding legal effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.42).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta's Restraint Authority Absorbed into Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '2f735303-43c3-450e-8d2f-1316f42af6f9').
narrative_ontology:cs_kernel_codification('2f735303-43c3-450e-8d2f-1316f42af6f9', fixed_text).
narrative_ontology:cs_authority_grounding('2f735303-43c3-450e-8d2f-1316f42af6f9', practice).
narrative_ontology:cs_interpretation_layer_present('2f735303-43c3-450e-8d2f-1316f42af6f9').
narrative_ontology:cs_reading_relation('2f735303-43c3-450e-8d2f-1316f42af6f9', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('2f735303-43c3-450e-8d2f-1316f42af6f9', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('2f735303-43c3-450e-8d2f-1316f42af6f9', foundational, legislature_is_sole_successor_to_charter_authority).
narrative_ontology:cs_axiom_status(legislature_is_sole_successor_to_charter_authority, holdable).
narrative_ontology:cs_axiom_grounding('2f735303-43c3-450e-8d2f-1316f42af6f9', legislature_is_sole_successor_to_charter_authority, conventional).
narrative_ontology:cs_axiom('2f735303-43c3-450e-8d2f-1316f42af6f9', foundational, no_judicially_enforceable_higher_law_above_statute).
narrative_ontology:cs_axiom_status(no_judicially_enforceable_higher_law_above_statute, holdable).
narrative_ontology:cs_axiom_grounding('2f735303-43c3-450e-8d2f-1316f42af6f9', no_judicially_enforceable_higher_law_above_statute, conventional).
narrative_ontology:cs_reference_frame('2f735303-43c3-450e-8d2f-1316f42af6f9', diceyan_parliamentary_supremacy).
narrative_ontology:cs_drift_state('2f735303-43c3-450e-8d2f-1316f42af6f9', post_human_rights_act_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f735303-43c3-450e-8d2f-1316f42af6f9', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_in_parliament).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judicially_unprotected_litigants).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, future_generations_bound_by_repealable_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, rule_of_law_via_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the sovereign legislative power inherited from the historical struggle Magna Carta symbolizes. Can enact, amend, or repeal any statutory restraint that traces its lineage to the charter, including habeas corpus protections and due-process guarantees. Exercises this power through ordinary majority vote, subject to no higher codified constitutional check.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority, beneficiary).

% The formal constitutional fiction uniting monarch and legislature. Benefits from the settled doctrine that no ancient charter binds the current sovereign body — legitimacy flows from parliamentary enactment, not from independent charter authority, which forecloses external claims of higher law against sitting governments.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_in_parliament, beneficiary,
    institutional, civilizational, arbitrage, national).

% Receives whatever protections Parliament currently chooses to maintain as statute — due process, property protections, restraints on arbitrary detention — but holds these only contingently, subject to future majorities. Can vote out a government that erodes protections, but has no separate legal recourse against a Parliament that repeals them through ordinary legislation.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate, payer).

% Groups whose interests do not command a parliamentary majority — historically Catholics, later various immigrant and minority communities, currently groups without electoral leverage. Whatever protection Magna Carta's descendants might have offered as inherent right is, under this reading, only as durable as the current majority's willingness to legislate it, and can be withdrawn by ordinary statute with no independent judicial backstop grounded in the charter itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_minorities, payer,
    powerless, biographical, trapped, national).

% Individuals seeking to invoke Magna Carta clause 39/40-style protections (due process, no punishment save by lawful judgment) against state action find courts bound to apply whatever Parliament has currently enacted, not the charter's original text. Where statute has narrowed or displaced the protection, courts cannot resurrect the charter's authority to override current legislation.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judicially_unprotected_litigants, payer,
    powerless, immediate, trapped, national).

% Inherit a constitutional settlement in which no protection is permanently entrenched — every generation's Parliament can undo what a prior Parliament granted. They bear the structural risk that rights currently understood as fundamental could be legislatively withdrawn without recourse to a higher constitutional text.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, future_generations_bound_by_repealable_rights, payer,
    powerless, civilizational, trapped, national).

% Apply and interpret statutes descended from Magna Carta's principles but must defer to Parliament as the ultimate constitutional authority — under this reading, courts have no power of constitutional review that could strike down an Act of Parliament for violating charter-derived rights. Their interpretive latitude operates strictly within the boundaries statute allows.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_courts, observer).

% Argue that treating Magna Carta as merely absorbed into revisable statute erases its independent normative force as inherited fundamental law binding on all rulers. Their view — closer to the living-constitutionalism reading — is not the operative doctrine in courts or Parliament and functions as scholarly critique rather than binding authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, originalist_legal_historians, excluded,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, and democratically accountable locus for revising and updating inherited restraints on arbitrary power, avoiding the instability of multiple competing claims to constitutional authority (crown prerogative, ancient charter, judicial natural law) that could each veto governance.
% TRANSFER_FUNCTION: Moves ultimate interpretive and revisory authority over 'ancient rights' from any fixed charter text or independent judiciary to whichever coalition commands a parliamentary majority at a given moment; correspondingly moves the durability of protection from constitutionally entrenched to legislatively contingent for anyone without majority political power.
% ABSENT_VOICES: Unrepresented minorities and future generations have no seat in the parliamentary process that determines whether charter-descended protections survive; originalist historians and rights-entrenchment advocates who would argue for judicially enforceable higher law are excluded from binding legal effect, confined to academic and political advocacy.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over charter-descended rights disappeared overnight and were replaced by an entrenched, judicially enforceable charter of rights, courts could strike down legislation for violating fundamental protections, minority groups would gain a check against majoritarian repeal, and the entire architecture of UK-style unwritten constitutionalism would be replaced by something resembling a written, court-enforced constitution — a fundamental rearrangement of where ultimate authority sits.
% FOUNDING_PROBLEM: Determining what body holds final authority to define and revise 'ancient liberties' after the direct baronial-crown compact of 1215 no longer reflected the political structure of the state — someone had to inherit or claim the authority the charter once asserted against an absolute monarch.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereignty doctrine is attested by constitutional scholars in the Diceyan tradition and by the operative practice of UK courts (which decline to strike down Acts of Parliament); however, this attestation comes substantially from within institutions that benefit from the doctrine's authority. Critics outside that tradition — comparative constitutionalists pointing to entrenched-rights systems, and human rights litigators noting cases where statute has narrowed charter-descended protections without independent recourse — corroborate that the founding problem (checking arbitrary power) remains live but contest that parliamentary sovereignty is an adequate or complete solution to it.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.20 to 0.48) as the gap between the charter's symbolic status and its actual legal operation widens: what began as a direct baronial check on a single monarch has become a doctrine under which no statute is immune to a subsequent majority's revision, concentrating real authority in whichever coalition currently governs. Suppression is moderate and largely structural rather than coercive in the crude sense — it operates through doctrine (courts declining judicial review of primary legislation) rather than force, but it functions to suppress alternative theories of entrenched constitutional right. Theater ratio rises steadily (0.10 to 0.40) as invocations of 'Magna Carta principles' in political rhetoric increasingly outpace their operative legal content, which is now determined almost entirely by current statute rather than the charter text.
 *
 * PERSPECTIVAL GAP:
 *   From the parliamentary-majority seat, this is a functioning, democratically legitimate constitutional order in which no provision is held hostage to unelected judicial or antiquarian authority. From the unrepresented-minority or judicially-unprotected-litigant seat, the identical structure appears as a standing risk that any currently-recognized right can be legislatively withdrawn with no independent recourse — the engine's per-seat computation should reflect this divergence directly from the power/exit data rather than from any single narrated verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary majorities and the electorate that can currently command a majority sit near the beneficiary end: they hold or can access the revisory power itself. Unrepresented minorities, litigants outside majoritarian favor, and future generations sit near the target end: they experience the same sovereignty doctrine as a ceiling on how durable any protection can be, since it always remains subject to repeal by a body they cannot reliably influence. Crown-in-Parliament as an institutional fiction benefits by having its ultimate authority go unchallenged by rival claims to higher law.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — checking arbitrary exercise of power — remains structurally live (state power still needs restraint), but the specific mechanism this reading endorses (parliamentary absorption with full revisory power) has drifted from 'restraint on the crown' toward 'restraint administered entirely at the discretion of whichever body currently holds the crown-in-parliament fiction.' This is tangled_rope rather than snare because the coordination function is genuine and load-bearing (a stable, legitimate amendment process avoiding constitutional paralysis) even as it structurally disadvantages groups without majoritarian leverage — mislabeling it a pure snare would miss the real coordination value; mislabeling it a pure rope would erase the victims who bear the cost of non-entrenchment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_entrenchment_framing,
    'Is Parliament''s unlimited revisory power over charter-descended rights better understood as the charter''s continuation-by-inheritance (this reading), or as evidence that Magna Carta was never truly a supra-legislative constraint and the ''inheritance'' framing is a legitimating fiction constructed after the fact?',
    'Comparative constitutional analysis of jurisdictions that trace similar founding compacts (e.g., other Commonwealth constitutions with entrenched bills of rights) alongside close historical study of whether contemporaries at any point between 1215 and 1689 treated the charter as binding independent of legislative re-enactment.',
    'If the inheritance framing is a retrospective legitimating construction, the coordination function claimed for parliamentary sovereignty is weaker than authored here, and the constraint would sit closer to snare (extraction dressed as continuity) than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_entrenchment_framing, conceptual, 'Whether parliamentary sovereignty is a genuine inheritance of charter authority or a legitimating narrative for legislative supremacy.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (parliamentary_sovereignty, living_constitutionalism, feudal_obsolescence) locate their disagreement — is it about WHO holds ultimate interpretive authority (Parliament vs. courts vs. no one), or about WHETHER the charter has any binding force at all today?',
    'Structural decomposition of each reading''s core premise against actual case law: examine whether courts in practice ever treat charter-descended principles as a check on primary legislation (would support living_constitutionalism), always defer to statute (supports parliamentary_sovereignty), or ignore the charter''s textual authority entirely (supports feudal_obsolescence).',
    'Clarifies which relations in reading_relations should be forecloses vs. coexists_with — if the disagreement is purely about the locus of authority (Parliament vs. courts) the readings can coexist across different legal systems; if it is about binding force per se, parliamentary_sovereignty and feudal_obsolescence converge on ''no independent charter authority'' while diverging on why.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise axis of disagreement among the three kernel readings of Magna Carta''s constraint authority.').

omega_variable(
    minority_protection_ceiling,
    'Does the absence of judicial power to strike down primary legislation for violating charter-descended rights represent an acceptable cost of democratic legitimacy, or a structural failure that leaves minorities without meaningful recourse?',
    'Empirical tracking of legislative episodes where charter-descended protections (due process, habeas corpus, protection from arbitrary detention) were narrowed by ordinary statute affecting minority groups, compared against outcomes in entrenched-rights jurisdictions facing comparable pressures.',
    'If narrowing episodes disproportionately affect groups without electoral leverage and entrenched-rights jurisdictions show materially better protection outcomes, this weighs toward classifying the victim-bearing cost as a structural extraction rather than an acceptable democratic tradeoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_ceiling, preference, 'Whether majoritarian revisability of fundamental rights is an acceptable democratic cost or a structural extraction from minorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1689, 0.2).
narrative_ontology:measurement_basis(magn_tr_t1689, observed).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1832, 0.28).
narrative_ontology:measurement_basis(magn_tr_t1832, observed).
narrative_ontology:measurement(magn_tr_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1911, 0.33).
narrative_ontology:measurement_basis(magn_tr_t1911, observed).
narrative_ontology:measurement(magn_tr_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1972, 0.37).
narrative_ontology:measurement_basis(magn_tr_t1972, observed).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(magn_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1689, 0.28).
narrative_ontology:measurement_basis(magn_be_t1689, observed).
narrative_ontology:measurement(magn_be_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1832, 0.35).
narrative_ontology:measurement_basis(magn_be_t1832, observed).
narrative_ontology:measurement(magn_be_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1911, 0.4).
narrative_ontology:measurement_basis(magn_be_t1911, observed).
narrative_ontology:measurement(magn_be_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1972, 0.44).
narrative_ontology:measurement_basis(magn_be_t1972, observed).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.48).
narrative_ontology:measurement_basis(magn_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1215, 0.35).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1689, 0.3).
narrative_ontology:measurement_basis(magn_su_t1689, observed).
narrative_ontology:measurement(magn_su_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1832, 0.32).
narrative_ontology:measurement_basis(magn_su_t1832, observed).
narrative_ontology:measurement(magn_su_t1911, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1911, 0.36).
narrative_ontology:measurement_basis(magn_su_t1911, observed).
narrative_ontology:measurement(magn_su_t1972, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1972, 0.4).
narrative_ontology:measurement_basis(magn_su_t1972, observed).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(magn_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% Sibling reading in the magna_carta_constraint_authority kernel family. This story (parliamentary_sovereignty_reading) authors ε=0.48 for the standing arrangement of parliamentary supremacy over charter-descended rights, structured as tangled_rope. The living_constitutionalism_reading sibling authors a different ε and structure for the same underlying kernel, treating the charter as independently binding through judicial precedent (likely rope or mountain-leaning, lower extraction, different victim set). The feudal_obsolescence_reading sibling treats the charter as having no binding force at all (likely low-extraction, near-mountain-of-history or simply inert). Each story is ε-invariant on its own terms; the three are linked via affects_constraints rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
