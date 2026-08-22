% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: Secular Democratic Subordination Constraint (Charter Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-revolutionary Charter mandates secular democratic institutions and
 *   requires military subordination to civilian authority. This story
 *   instantiates ONE reading of the contested kernel
 *   'july_charter_sovereign_legitimacy': the secular democratic reading. The
 *   Charter is presented by its drafters and international advocates as the
 *   neutral, democratic, institutional answer to the founding legitimacy
 *   question; this reading treats it as such. However, the same Charter is
 *   read by military actors as constraining legitimate guardianship, and by
 *   political Islam as foreclosing religious sovereignty. This constraint
 *   story captures ONLY the secular democratic reading — it is not neutral,
 *   it is one commitment. The sibling readings are separate constraint files.
 *   The claim/metric gap is deliberate and structural to kernel readings: the
 *   reading claims the constraint coordinates secular democratic legitimacy;
 *   the metrics describe substantially extractive, heavily suppressed
 *   operation because the reading's enforcement depends on excluding
 *   competing framings. The engine measures that asymmetry; the gap is not an
 *   error.
 *
 * KEY AGENTS:
 *   - secular_democratic_parties: beneficiaries (gain constitutional legitimacy and institutional access)
 *   - civil_state_institutions: beneficiaries (derive authority from Charter's secular democratic mandate)
 *   - political_islam_movements: victims (excluded from religious sovereignty framing, constrained entry to formal institutions)
 *   - military_autonomous_authority: victims (subordinated to civilian control, identity-locked exit)
 *   - charter_drafting_coalition: agenda_setter (writes and enforces secular democratic frame)
 *   - international_secular_democratic_observers: observers (amplify the reading's institutional weight)
 *   - political_islam_international_networks: excluded (barred from Charter co-interpretation)
 *   - military_guardian_ideology_network: excluded (subordinated framings foreclosed)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.76).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "Secular Democratic Subordination Constraint (Charter Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '95b65dcd-987b-46ad-b053-34931d58d11c').
narrative_ontology:cs_kernel_codification('95b65dcd-987b-46ad-b053-34931d58d11c', fixed_text).
narrative_ontology:cs_authority_grounding('95b65dcd-987b-46ad-b053-34931d58d11c', lineage).
narrative_ontology:cs_interpretation_layer_present('95b65dcd-987b-46ad-b053-34931d58d11c').
narrative_ontology:cs_reading_relation('95b65dcd-987b-46ad-b053-34931d58d11c', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('95b65dcd-987b-46ad-b053-34931d58d11c', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('95b65dcd-987b-46ad-b053-34931d58d11c', foundational, secular_democratic_legitimacy_sole_source).
narrative_ontology:cs_axiom_status(secular_democratic_legitimacy_sole_source, holdable).
narrative_ontology:cs_axiom_grounding('95b65dcd-987b-46ad-b053-34931d58d11c', secular_democratic_legitimacy_sole_source, conventional).
narrative_ontology:cs_axiom('95b65dcd-987b-46ad-b053-34931d58d11c', foundational, military_subordination_to_civilian_authority).
narrative_ontology:cs_axiom_status(military_subordination_to_civilian_authority, holdable).
narrative_ontology:cs_axiom_grounding('95b65dcd-987b-46ad-b053-34931d58d11c', military_subordination_to_civilian_authority, conventional).
narrative_ontology:cs_reference_frame('95b65dcd-987b-46ad-b053-34931d58d11c', post_revolutionary_secular_democratic_founding).
narrative_ontology:cs_drift_state('95b65dcd-987b-46ad-b053-34931d58d11c', contemporary_enforcement_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('95b65dcd-987b-46ad-b053-34931d58d11c', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_state_institutions).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_movements).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain constitutional legitimacy and institutional access through secular democratic framing. Benefit from Charter provisions that exclude religious-nationalist competitors and subordinate military veto power. Their institutional survival depends on Charter enforcement; exit would mean abandoning the constitutional ground they inhabit.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties, beneficiary,
    organized, generational, constrained, national).

% Derive constitutional legitimacy from Charter's secular democratic mandate. Parliament, judiciary, and civilian administration gain authority to act as primary decision-makers rather than subordinate to military preference. The Charter frames their role as the normal state structure, not provisional.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_state_institutions, beneficiary,
    institutional, generational, analytical, national).

% Excluded or marginalized by Charter provisions that enshrine secular democratic legitimacy as the sole source of legal authority and impose electoral/institutional barriers on religious-nationalist organization. They carry the cost of constitutional subordination: inability to advance Islam as a sovereign legitimacy ground within the legal system, forced integration into secular democratic frames, or operating outside formal institutions. Exit means renouncing the Charter's authority entirely — a costly break from the state order.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_movements, payer,
    organized, generational, constrained, national).

% Subordinated to civilian control through Charter provisions that bar autonomous military governance, strip the military of independent legislative authority, and require military command to answer to civilian political authority. The military's institutional identity has historically fused with guardian-of-state function; Charter subordination requires reconstructing that identity as professional service rather than autonomous custodian. Cost includes loss of veto power over civilian decisions, inability to claim special constitutional standing, and structural pressure to depoliticize military hierarchy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, biographical, identity_locked, national).

% Writes and enforces the Charter; represents the coalition that triumphed in the founding moment. Sets the secular democratic framing and the subordination requirements through Charter text and interpretation of that text. Their power to set the frame is real but depends on continuous enforcement — if military or Islamist movements reassert autonomous authority, the frame weakens.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, charter_drafting_coalition, agenda_setter,
    organized, biographical, constrained, national).

% Monitor and publicly affirm Charter compliance. Use recognition, aid, and diplomatic support as incentives for secular democratic enforcement. Frame compliance as the gate to international legitimacy and material support; Charter violations are treated as backsliding. Their observational role amplifies the secular democratic reading's institutional weight.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_secular_democratic_observers, observer,
    powerful, generational, analytical, global).

% Excluded from Charter co-drafting; their reading of legitimate statehood (religious identity as sovereign ground) is actively foreclosed by secular democratic framing. Would argue for constitutionalizing Islamic law or religious legitimacy if given standing. Their structural exclusion is maintained by international alliance patterns that reinforce the secular democratic reading.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_international_networks, excluded,
    powerful, generational, constrained, global).

% Excluded from Charter authority by subordination provisions. Represents the military-as-permanent-guardian reading (the sibling kernel reading). Their exclusion from formal decision-making authority is structural to the secular democratic reading; if they reasserted autonomous guardian claims, the entire secular democratic frame would face challenge.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_guardian_ideology_network, excluded,
    institutional, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, stable framework for legitimate governance: secular democratic institutions (parliament, courts, elected executive) as the only constitutionally valid locus of sovereignty. Solves the coordination problem of founding after rupture: what is the source of legitimate authority now? The secular democratic reading answers: the demos through democratic institutions, not religious law, not military hierarchy.
% TRANSFER_FUNCTION: Transfers sovereign authority FROM military autonomous decision-making and FROM religious-nationalist legitimacy claims TO secular democratic institutions and secular constitutional law. Transfers veto power FROM military guardianship TO civilian elected representatives. Transfers constitutional immunity FROM military autonomy TO Charter enforcement machinery and civilian courts.
% ABSENT_VOICES: Political Islam movements and military-as-guardian ideologists are structurally excluded from Charter co-interpretation: they would argue for religious legitimacy and military autonomous authority respectively, but the secular democratic reading pre-forecloses those framings at the constitutional level. Alternative readings exist (the sibling kernels documented in omegas) but are not parties to Charter enforcement.
% DISAPPEARANCE_RATIONALE: If this secular democratic constraint vanished — if the Charter were repealed or its enforcement collapsed — the military could reassert autonomous governance claims, political Islam could pursue constitutionalization of religious law, and the entire sovereignty structure would reorganize around contested foundations. The state would face immediate questions about the source of legitimate authority that the Charter currently answers; institutions built on Charter legitimacy would lose their foundation.
% FOUNDING_PROBLEM: Post-revolutionary rupture: the old regime's legitimacy is discredited; a new one must be established. What is the source of legitimate governance authority? Competing answers: religious tradition and Islamic law, military institutional hierarchy as permanent guardian, or secular democratic will-of-the-people.
% FOUNDING_PROBLEM_CORROBORATION: The secular democratic reading's advocates attest the founding problem is live and unresolved — the Charter's continued enforcement is necessary because challenges from military and Islamist actors persist. Military officers and political Islam organizations attest the problem is NOT solved by the secular democratic answer — they hold the founding problem remains open and their alternative readings are equally valid. International observers attest the founding problem is institutionally managed by the Charter, though they acknowledge contentious interpretation. The problem status divergence IS the constraint: the reading works only as long as the secular democratic coalition can enforce its answer against competing framings.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects the substantial transfer: Charter provisions strip military autonomous authority and exclude Islamic-nationalist legitimacy claims. These are real structural transfers, not theater. Suppression is higher (0.76) because the secular democratic reading's persistence depends on actively enforcing subordination — military and Islamist actors retain institutional capacity and legitimacy narratives that compete with the Charter frame; the constraint holds only as long as enforcement machinery works. Theater ratio (0.42) is moderate because the secular democratic framing is genuinely institutionalized (courts, parliament, constitution), but a growing share of enforcement energy goes to suppressing competing framings rather than operating normal democratic processes. The measurement series models enforcement intensification (suppression rising from 0.68 to 0.76 over 40 time points) as the coalition faces mounting pressure from excluded framings. Extractiveness rises more slowly (0.52 to 0.68) because the fundamental transfer is established early; the rise models accumulating performative work to maintain subordination. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The charter_drafting_coalition seat and the political_islam_movements seat should compute dramatically differently. From the drafting coalition's position, the constraint is genuine coordination — it stabilizes legitimate authority on a shared secular democratic basis, enabling normal democratic process. From the political_islam seat, the same constraint operates as pure extraction: it forecloses their reading and subordinates their constituencies through constitutional exclusion. From the military_autonomous_authority seat, the constraint is extraction + identity disruption: it transfers veto power, forces reconstitution of institutional identity from guardian to subordinate, and creates structural pressure toward depoliticization. The engine computes this divergence from the structural data alone — the authored claim (tangled_rope: real coordination + asymmetric extraction) preserves the divergence as a feature of the constraint, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (secular_democratic_parties, civil_state_institutions) gain constitutional legitimacy and institutional access; they derive d near 0.0 (beneficiary end). Victims (political_islam_movements, military_autonomous_authority) lose autonomous authority; they derive d near 1.0 (target end). The military_autonomous_authority seat is particularly high-d because exit is identity_locked — the military's institutional identity has fused with guardian-of-state function; subordination requires not just accepting civilian control but reconstructing professional military identity itself. Political_islam_movements carry high d due to constrained exit: they can exit by renouncing the Charter (revolutionary exit at high cost) or work within the secular frame (accepting subordination). Neither is mobile arbitrage; both are extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   This is NOT a case of a dead mandate persisting as theater. The secular democratic reading's founding mandate — to establish legitimate post-revolutionary authority through democratic institutions — remains structurally live as long as the excluded framings contest it. The measurement series shows suppression_requirement slightly rising (0.68 to 0.76) because the coalition faces increasing pressure from military and Islamist actors reasserting their readings. The theater_ratio rise (0.25 to 0.42) indicates growing performative work — increasing share of enforcement energy devoted to suppressing competing framings rather than operating routine democracy. This is NOT mandatrophy (function-atrophied-but-enforced); it is mandate-contested (function-alive-but-under-pressure). Mandatrophy would appear as theater_ratio accelerating past 0.6+ while extractiveness fell, indicating the constraint persists mainly through performance while the original function withers. That is not this reading's trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the secular democratic reading the correct instantiation of the Charter''s text, or do the guided_nationalism and military_custodian readings capture equally valid textual readings that simply serve different constituencies?',
    'Comparative constitutional law analysis of similar post-revolutionary charters; historical record of the Charter''s drafting process and competing interpretations presented in founding debates; empirical record of which reading''s interpretation mechanisms (courts, executive, parliament) actually control state action over time.',
    'If the secular democratic reading is the sole coherent interpretation, it approaches a mountain-like constraint — a reading that is structurally true. If competing readings are textually defensible, the constraint is more deeply tangled_rope: multiple readings coexist, each instantiating different extraction patterns. The empirical record of which interpretation controls will determine which reading actually governs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the secular democratic reading is the Charter''s true content or one among multiple textually defensible readings.').

omega_variable(
    enforcement_asymmetry_stability,
    'How stable is the enforcement asymmetry that subordinates military and political Islam? Does the secular democratic coalition have the institutional capacity to maintain subordination indefinitely, or will enforcement decay, military pressure accumulate, and the Charter''s reading shift?',
    'Long-term institutional trajectory: whether secular democratic courts and parliament retain decision-control over military appointment and removal, whether political Islam gains institutional entry through electoral evolution, whether international support for the secular democratic reading persists, whether military institutional capacity and morale remain subordinated.',
    'High enforcement stability would mean the secular democratic reading stabilizes as the dominant framing, theater_ratio plateaus, and the constraint moves toward rope (coordination succeeds). Low stability would mean suppression_requirement rises sharply, theater_ratio accelerates (performing subordination while it weakens), and the constraint drifts toward snare (pure extraction of competing framings before they reorganize) or toward contested mandatrophy (the mandate to establish secular democratic authority becomes unachievable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_stability, empirical, 'Whether the enforcement machinery maintaining secular democratic subordination remains stable or erodes.').

omega_variable(
    military_identity_lock_permanence,
    'Is military identity_locked status permanent, or can military institutional culture depoliticize and accept subordination as a normal professional role?',
    'Post-subordination military generational replacement; tracking of military organizational identity claims in public discourse, recruitment narratives, institutional training; surveys of military personnel on professional versus guardian identity; institutional succession choices (do military leaders accept or resist civilian appointment authority).',
    'If identity-lock persists, the military remains a high-d target: subordination costs remain substantial (requiring continuous enforcement). If military depoliticization succeeds, d-value for military_autonomous_authority approaches moderate (the institutional transfer normalizes as routine civilian-military relations). This directly affects the long-term sustainability of suppression_requirement: if identity-lock persists, suppression must remain high; if it relaxes, the constraint can shift toward rope-like operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_identity_lock_permanence, empirical, 'Whether military institutional identity can depoliticize or remains fused with guardian-of-state claims.').

omega_variable(
    political_islam_electoral_integration,
    'Can political Islam movements integrate into the secular democratic framework through electoral and institutional channels, or do Charter secular provisions foreclosure Islamic political participation entirely?',
    'Empirical record of political Islam electoral participation, institutional legitimacy, and Charter-compliant political organization; degree to which Islamic parties can operate within secular democratic rules or face structural bars; international jurisprudence on religious-political party participation in secular democratic systems.',
    'If integration succeeds, political_islam_movements d-value decreases (from high-target toward moderate): they become participants in secular democratic competition rather than excluded victims. If foreclosure persists, d remains high and the constraint sustains asymmetric extraction. This affects classification: a secular democratic system that fully integrates religious political movements approaches rope; one that maintains systematic political Islam exclusion remains tangled_rope or approaches snare (pure extraction of a competing legitimacy narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_islam_electoral_integration, empirical, 'Whether political Islam can institutionally participate in secular democratic governance or remains structurally excluded.').

omega_variable(
    sibling_reading_foreclosure_ambiguity,
    'Does the secular democratic reading logically foreclose the military_custodian and guided_nationalism readings, or do these readings coexist as different parties'' competing framings of the same Charter?',
    'Logical analysis of the axioms (see cs_structure.axioms): if secular democratic legitimacy is foundational and military autonomy logically contradicts it, foreclosure applies; if the readings are axiomatically distinct but textually coexistent, they coexist. Empirical observation: if military officers and Islamist actors explicitly reject the secular democratic reading yet accept the Charter''s formal authority, coexistence is empirically real.',
    'If foreclosure is true, the constraint approaches mountain-like permanence: the military and Islamic readings are logically false within the Charter framework, and teaching that truth is the secular democratic reading''s work. If coexistence is true, the constraint is constitutively contested: it persists through enforcement of one reading against competitors who remain institutionally present and could reorganize. This affects omega classification: foreclosure is an empirical/conceptual question; coexistence is a structural fact about the reading''s claim to logical uniqueness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_ambiguity, conceptual, 'Whether this reading logically eliminates competitors or coexists with them as a contested framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel july_charter_sovereign_legitimacy. Three separate constraint files model the three readings (secular_democratic_reading, guided_nationalism_reading, military_custodian_reading). Each reading instantiates a different ε, different beneficiary/victim structure, and different classification. The readings coexist as competing parties' framings of the same Charter text. This secular_democratic_reading affects both sibling readings because its enforcement (secular democratic courts, subordination of military, exclusion of religious-nationalist law) creates structural pressure on the alternative readings' legitimacy claims. Each sibling reading is both a competitor for Charter interpretation and structurally downstream from this reading's enforcement machinery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
