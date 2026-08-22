% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: constitutional/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the CROWN READING of remonstrance authority:
 *   the claim that magistrate remonstrance rights, invoked to preserve
 *   'ancient constitutional liberties,' function as a veto protecting
 *   particularist fiscal privileges against rational imperial consolidation.
 *   Under this reading, remonstrance is a snare mechanism—extractive
 *   gatekeeping dressed in constitutional language. The Crown (fiscal
 *   authority) bears costs when remonstrance blocks necessary reforms;
 *   provincial magistratures and gentry networks benefit by maintaining tax
 *   exemptions and jurisdictional monopolies. The founding coordination
 *   problem (preventing arbitrary imperial action) is dead; what persists is
 *   the extracted veto power. The sibling reading (magistrate_reading) frames
 *   remonstrance as legitimate constitutional defense of ancient liberty.
 *   These are not two measurements of the same constraint; they are two
 *   distinct constraints instantiated by incompatible readings of the same
 *   kernel text.
 *
 * KEY AGENTS:
 *   - crown_fiscal_authority: Victim (d=0.95) — bears costs of gridlock, blocked reforms, cannot rationalize taxation
 *   - provincial_magistratures: Agenda-setter + beneficiary (d=0.10) — sets veto agenda, protected from audit, collects jurisdictional rents
 *   - landed_gentry_networks: Beneficiary (d=0.15) — properties shielded from centralization, exemptions protected through magistrate patronage
 *   - centralizing_reformers: Payer (d=0.88) — edicts blocked, authority undermined, credited for nothing completed
 *   - ordinary_taxpayers: Excluded (d=0.92) — not in remonstrance bodies, bear fiscal chaos when gridlock starves state capacity
 *   - imperial_military_apparatus: Payer (d=0.85) — starved of funds, blamed for strategic failure caused by fiscal delays beyond its control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.71).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional/political_economy").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'bca589a4-89ee-4d16-84a0-eaee22cdcaa3').
narrative_ontology:cs_kernel_codification('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', fixed_text).
narrative_ontology:cs_authority_grounding('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', lineage).
narrative_ontology:cs_interpretation_layer_present('bca589a4-89ee-4d16-84a0-eaee22cdcaa3').
narrative_ontology:cs_reading_relation('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', foundational, remonstrance_as_gentry_veto).
narrative_ontology:cs_axiom_status(remonstrance_as_gentry_veto, holdable).
narrative_ontology:cs_axiom_grounding('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', remonstrance_as_gentry_veto, empirically_contingent).
narrative_ontology:cs_axiom('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', foundational, ancient_liberty_doctrine_illegitimate_cover).
narrative_ontology:cs_axiom_status(ancient_liberty_doctrine_illegitimate_cover, holdable).
narrative_ontology:cs_axiom_grounding('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', ancient_liberty_doctrine_illegitimate_cover, empirically_contingent).
narrative_ontology:cs_reference_frame('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', centralized_rational_fiscal_authority).
narrative_ontology:cs_drift_state('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', contemporary_magistrate_entrenchment, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('bca589a4-89ee-4d16-84a0-eaee22cdcaa3', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, provincial_magistratures).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, landed_gentry_networks).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, centralizing_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, imperial_military_apparatus).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, ancient_constitutional_limits_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, magistrate_custodianship_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Crown seeks to levy new taxes, reorganize fiscal administration, or implement centralizing reforms necessary for state capacity and military readiness. When magistrate bodies exercise remonstrance rights, they block or delay these measures indefinitely by claiming constitutional violation. The Crown cannot override without appearing to abandon ancient legal forms and inviting escalation. It bears the cost of gridlock, diplomatic isolation, and fiscal strain.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_fiscal_authority, payer,
    institutional, generational, trapped, national).

% Control the local apparatus of justice, recruitment, and property registration. They exercise remonstrance right to block imperial edicts that would subordinate their authority to centralized organs, redistribute their jurisdictional privileges, or impose taxes they cannot collect without consent. Their veto is protected by invoking ancient constitutional custom; they extract rents from their monopoly over local governance and shield themselves from competitive oversight.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_magistratures, agenda_setter,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, provincial_magistratures, beneficiary).

% Benefit from the magistrate veto indirectly: their properties are shielded from centralizing land taxes, their exemptions are protected from audit, and their local political dominance rests on magistrate cooperation. They sponsor magistrate resistance through patronage networks and constitute the core of remonstrating blocs. Remonstrance prevents imperial rationalization that would expose their holdings to standardized assessment.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, landed_gentry_networks, beneficiary,
    powerful, generational, arbitrage, regional).

% Imperial officials tasked with administrative modernization, fiscal consolidation, or military reform. They author edicts that magistrates remonstrate against, absorbing years of political cost and institutional delay. Their authority is undermined by the invocation of ancient constitutional limits that block them even when their reforms are empirically justified. They bear the cost of stagflation and cannot be promoted or credited for work that cannot be completed.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, centralizing_reformers, payer,
    institutional, biographical, constrained, national).

% Are not represented in magistrate bodies and cannot remonstrate themselves. They bear the consequence of gridlock: fiscal chaos, inability to raise funds for public defense, and arbitrary collection efforts when magistrate vetoes prevent systematic taxation. Their exclusion from the remonstrance mechanism means their interests are never heard when magistrates claim ancient constitutional privilege.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, ordinary_taxpayers, excluded,
    powerless, biographical, trapped, national).

% Depends on reliable fiscal revenue to maintain readiness. Remonstrance-induced delays and blockades starve military budgets, degrade force posture, and invite external challengers. Leadership absorbs the blame for strategic failures caused by fiscal delays they cannot control. They bear the structural cost of the constraint without the ability to remonstrate.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, imperial_military_apparatus, payer,
    institutional, biographical, constrained, national).

% Scholars and jurists who defend remonstrance as an authentic recovery of ancient constitutional wisdom. They argue magistrates are custodians of immemorial liberty, not obstructionists. This reading is not themselves a beneficiary or payer but provides the theoretical apparatus that legitimates the constraint. They are the interpretive authority grounding the constraint's claimed naturality.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, constitutional_traditionalists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, provincial_magistratures).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for institutional bodies possessing local knowledge and legitimacy (magistratures) to register objection to imperial edicts before implementation, ensuring that reforms do not blindly override settled jurisdictional arrangements and property protections.
% TRANSFER_FUNCTION: Transfers veto power from the Crown to magistrate bodies, allowing them to extract compliance costs from centralization efforts and protect particularist privileges from standardizing audit. The constraint moves authority from the fiscal center to regional gatekeepers.
% ABSENT_VOICES: Ordinary taxpayers, non-gentry peasantry, merchants excluded from magistrate networks, and centralizing reformers outside the magistrate bloc have no seat at the remonstrance table. They would argue for rational fiscal administration, open competition, and reduction of magistrate rent extraction—but remonstrance exists precisely to exclude such arguments from formal consideration.
% DISAPPEARANCE_RATIONALE: If remonstrance right vanished, the Crown could implement fiscal consolidation, standardized taxation, and administrative rationalization within months. Military readiness would improve, gentry tax-farming would become exposed to audit, and magistrate monopolies on local authority would face centralized competition. The entire structure of elite coordination through magistrate privilege would collapse, forcing reorganization around either imperial efficiency or open competitive politics.
% FOUNDING_PROBLEM: Early imperial expansion created overlapping jurisdictions and competing authority claims between Crown and magistratures. Remonstrance emerged as a mechanism to prevent arbitrary imperial action that could dissolve established local arrangements without procedural notice.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and centralizing reformers attest the founding problem is solved: jurisdictions are now clearly defined, magistrate authority is fixed by statute, and the risk of arbitrary imperial action has been institutionalized away. Constitutional traditionalists attest the founding problem remains live as a perpetual risk of centralizing overreach. Independent political economists and historians outside both benefiting parties note that remonstrance has mutated from procedural safeguard to veto mechanism protecting fiscal evasion; the original problem (arbitrary action) is solved, but the solution has become the obstruction.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.45 to plateau at 0.68 over the interval: as the Crown's reform agenda becomes more ambitious and magistrate resistance hardens, the veto power's extractive margin widens. Early period (0–10): magistrates still accommodate some reforms; middle period (10–25): magistrates entrench around fiscal privilege and remonstrance invocations multiply; late period (25+): the constraint plateaus at maximum extraction—magistrates have neutralized most Crown initiatives without losing legitimacy. Theater ratio climbs from 0.25 to 0.42 and plateaus: magistrates increasingly perform 'constitutional custodianship' ritualism (elaborate remonstrance documents citing ancient texts) while the actual function is rent protection. Suppression requirement rises from 0.55 to 0.71: the Crown must suppress internal reformer pressure, suppress ordinary taxpayer demands for fiscal order, and suppress military officers' complaints about budgets—suppression is high because the constraint persists despite creating measurable harm to multiple stakeholders. The plateau at t=25+ indicates the constraint has reached equilibrium: magistrates have extracted maximum veto power without triggering Crown dissolution of the remonstrance right (which would cost the Crown legitimacy). This is the classic snare signature: extraction peaks and stabilizes when the target has been fully constrained and has exhausted exit options.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's seat (institutional, trapped): remonstrance is extractive obstruction—a veto protecting gentry privilege dressed as constitutional recovery. From the magistrate seat (organized, arbitrage): remonstrance is legitimate custodianship of ancient law against centralizing tyranny—the veto is proper exercise of constitutional authority. The engine computes radically different type classifications from these same structural facts: Crown seat computes snare (target of extraction, suppressed alternatives); magistrate seat computes rope (genuine coordination, no measured extraction). The claim (snare) matches the Crown's perspective; the magistrate perspective would instantiate a different constraint (magistrate_reading). The architecture's innovation is that BOTH readings produce valid constraint stories with DIFFERENT ε values, DIFFERENT beneficiary/victim sets, DIFFERENT types—because a reading is not a perspective or an opinion, it is a DISTINCT CONSTRAINT instantiated by incompatible interpretations of a shared kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown fiscal authority: full target (d=0.95). Bears the extraction directly—remonstrance blocks its edicts, forces compliance costs, starves capacity. Trapped exit (cannot withdraw from fiscal authority or become another power atom). Provincial magistratures: near-full beneficiary (d=0.10). Sets the agenda (remonstrance right), collects the rents (fiscal exemption, jurisdictional monopoly), has arbitrage exit (could comply with Crown edits but choose not to). Landed gentry: beneficiary (d=0.15). No direct extraction; indirect beneficiary through magistrate patronage networks. Centralizing reformers: near-full target (d=0.88). Edicts blocked, authority denied, constrained exit (career trapped in service, cannot leave imperial apparatus). Military: near-full target (d=0.85). Budgets starved, cannot exit or override. Ordinary taxpayers: near-full target (d=0.92). Excluded from remonstrance, bear fiscal chaos, trapped exit. The directionality profile is highly asymmetric: two beneficiary seats with low d, four payer/target seats with d > 0.85. This asymmetry is the snare signature. No directionality override needed; the derived values match the structural reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is DEAD: the founding problem (arbitrary imperial action, jurisdictional chaos) has been institutionally solved—magistrate authority is now fixed by statute, jurisdictional boundaries are defined, and the risk of wholesale imperial override has been normalized away. Yet remonstrance persists at high extractiveness (0.68). This is mandatrophy: the mandate that created remonstrance (protecting against arbitrary action) has outlived its function, but the mechanism persists because it now serves a different function (protecting fiscal privileges). The constraint itself prevents the Crown from formally repealing remonstrance (which would require appearing to abandon ancient constitutional form), creating a locked equilibrium. The rising extraction curve (0.45→0.68) as the founding problem was progressively solved is the signature: the constraint's function shifted from coordination (needed when jurisdictions were unclear) to extraction (protecting gentry monopolies now that jurisdictions are clear). An agenda-setter seat (magistratures) benefits from this shift; the Crown cannot correct it without institutional cost. This is exactly the mandatrophy-resolved case: the arrangement's justification is dead, but the structural beneficiary has captured the apparatus and prevents dissolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence_contest,
    'Is the founding coordination problem (preventing arbitrary imperial action without procedure) genuinely dead, or do magistrates correctly identify an ongoing risk of centralized overreach that remonstrance still addresses?',
    'Historical analysis of post-remonstrance reforms: if Crown edicts after magistrate remonstrance are implemented smoothly without the arbitrary revisions remonstrance was meant to prevent, the founding problem is dead. If Crown edicts are revised mid-course or contradict prior commitments, the problem persists.',
    'If the problem is dead, the constraint is mandatrophy-resolved (snare persisting by inertia); if the problem persists, remonstrance is legitimate constitutional defense and the type classification should shift toward rope or mountain. This mismatch is the core of the reading contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_contest, empirical, 'Whether the founding coordination problem is obsolete or still live.').

omega_variable(
    ancient_liberty_construction,
    'Is magistrate invocation of ''ancient constitutional liberty'' a genuine recovery of immemorial practice, or is it a contemporary invention backdated to classical sources to legitimize gatekeeping?',
    'Textual and archaeological analysis of the kernel: does actual pre-imperial practice show magistrate remonstrance rights, or are modern remonstrance claims a 16th–17th century innovation attributed retrospectively to ancient tradition?',
    'If ''ancient liberty'' is a construction, the constraint''s legitimacy claim dissolves and it becomes obviously extractive gatekeeping. If ancient practice did instantiate remonstrance, the legitimacy framework shifts and the magistrate_reading gains coherence. The two readings hang on opposite conclusions from this same evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ancient_liberty_construction, conceptual, 'Whether remonstrance is ancient practice or modern invention.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the Crown''s suppression of internal reformer pressure and military dissent structural (external barriers to removing remonstrance right) or internalized (Crown elites have absorbed the constitutional mythology and genuinely believe remonstrance is ancient law)?',
    'Post-constraint analysis: if Crown authority can act decisively and reform taxation after remonstrance is repealed (by another power transition or constitutional amendment), the suppression was structural and external. If Crown elites continue to treat remonstrance as binding even after formal repeal, the suppression has been internalized.',
    'If structural, the constraint''s effective suppression is lower than authored (0.71) once the structural barrier is removed. If internalized, the suppression persists in elite cognition even after institutional repeal—the target carries the constraint with them. This affects post-remedy trajectory analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of reform is structural or internalized elite belief.').

omega_variable(
    reading_distinctness_boundary,
    'Do the crown_reading and magistrate_reading represent genuinely incommensurable framings of a single kernel, or are they points on a continuous spectrum of legitimacy assessment where an intermediate reading is coherent?',
    'Formal logical analysis of the core axioms: can a framework coherently hold that remonstrance is both ancient constitutional law AND serving contemporary gentry fiscal privilege? Or do the two axioms contradict such that only one can be true in a single framework?',
    'If genuinely incommensurable (forecloses relation), the two readings cannot coexist in one authority structure and the constraint is unstable—one reading must prevail. If a middle position is coherent (coexists_with or influences relation), the readings are distinct but not mutually destructive and the constraint can persist as long-running institutional tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinctness_boundary, conceptual, 'Whether the two readings are logically incommensurable or admit intermediate frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(remo_tr_t0, observed).
narrative_ontology:measurement(remo_tr_t5, remonstrance_authority__crown_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(remo_tr_t5, observed).
narrative_ontology:measurement(remo_tr_t10, remonstrance_authority__crown_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(remo_tr_t10, observed).
narrative_ontology:measurement(remo_tr_t15, remonstrance_authority__crown_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(remo_tr_t15, observed).
narrative_ontology:measurement(remo_tr_t20, remonstrance_authority__crown_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(remo_tr_t20, observed).
narrative_ontology:measurement(remo_tr_t25, remonstrance_authority__crown_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(remo_tr_t25, observed).
narrative_ontology:measurement(remo_tr_t30, remonstrance_authority__crown_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(remo_tr_t30, observed).
narrative_ontology:measurement(remo_tr_t35, remonstrance_authority__crown_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(remo_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(remo_be_t0, observed).
narrative_ontology:measurement(remo_be_t5, remonstrance_authority__crown_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(remo_be_t5, observed).
narrative_ontology:measurement(remo_be_t10, remonstrance_authority__crown_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(remo_be_t10, observed).
narrative_ontology:measurement(remo_be_t15, remonstrance_authority__crown_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(remo_be_t15, observed).
narrative_ontology:measurement(remo_be_t20, remonstrance_authority__crown_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(remo_be_t20, observed).
narrative_ontology:measurement(remo_be_t25, remonstrance_authority__crown_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(remo_be_t25, observed).
narrative_ontology:measurement(remo_be_t30, remonstrance_authority__crown_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(remo_be_t30, observed).
narrative_ontology:measurement(remo_be_t35, remonstrance_authority__crown_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(remo_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(remo_su_t0, observed).
narrative_ontology:measurement(remo_su_t5, remonstrance_authority__crown_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(remo_su_t5, observed).
narrative_ontology:measurement(remo_su_t10, remonstrance_authority__crown_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(remo_su_t10, observed).
narrative_ontology:measurement(remo_su_t15, remonstrance_authority__crown_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(remo_su_t15, observed).
narrative_ontology:measurement(remo_su_t20, remonstrance_authority__crown_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(remo_su_t20, observed).
narrative_ontology:measurement(remo_su_t25, remonstrance_authority__crown_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(remo_su_t25, observed).
narrative_ontology:measurement(remo_su_t30, remonstrance_authority__crown_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(remo_su_t30, observed).
narrative_ontology:measurement(remo_su_t35, remonstrance_authority__crown_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(remo_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel admits two structurally incompatible readings. The crown_reading (this file) frames remonstrance as extractive veto protecting gentry privilege; the magistrate_reading frames it as legitimate constitutional defense of ancient liberty. Each reading instantiates a DIFFERENT constraint with different ε, beneficiary/victim sets, and types. They are linked via network.affects_constraints because they compete for legitimacy in the same institutional arena—dominance of one reading forecloses adoption of the other by the same authority structure. The two readings cannot both be true in a single institutional framework; one must prevail. This is not two measurements of the same constraint; it is two distinct constraints from incompatible readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
