% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Near-Categorical Speech Protection (Absolutist Reading)
 *   domain: constitutional/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the speech protection
 *   kernel: expression is protected near-categorically, and the harm a
 *   listener suffers from protected speech is assigned no restrictive force
 *   unless the speech crosses one of a narrow, closed set of exclusions
 *   (imminent incitement, true threats). Speaker autonomy is maximized; the
 *   protection boundary is the widest available among the kernel's readings.
 *   The arrangement has a genuine coordination core — it disables the
 *   discretionary harm-weighing machinery that majorities have historically
 *   used to persecute dissent — and it simultaneously shifts the costs of
 *   harmful protected expression onto identifiable, largely immobile listener
 *   populations. Claim and metrics are authored independently: I claim
 *   tangled_rope because both the coordination function and the asymmetric
 *   cost-shifting are structurally real and the arrangement requires
 *   continuous judicial enforcement to hold; the metric values record what I
 *   take to be descriptively true of its operation, without tuning either to
 *   the other or to a predicted engine output. Family relationship: sibling
 *   readings of the same kernel (harm_threshold_reading, marketplace_reading,
 *   dignity_reading, democratic_participation_reading) are separate
 *   constraints with different cognizable-victim sets and different epsilon
 *   values; see network.dual_formulation_note and the
 *   kernel_reading_indexicality omega.
 *
 * KEY AGENTS:
 *   - - political_dissidents: Intended primary beneficiary (powerless/constrained) — relies on the bright line against state suppression machinery
 *   - - extremist_and_provocateur_speakers: Unintended beneficiary (moderate/mobile) — protected while attacking identifiable groups
 *   - - high_reach_media_corporations: Concentrated beneficiary (institutional/arbitrage) — monetizes immunity at scale, funds its defense
 *   - - civil_liberties_advocates: Secondary beneficiary (organized/identity_locked) — professional identity fused with boundary maintenance
 *   - - targeted_identity_group_members: Primary bearer of costs (powerless/trapped) — absorbs protected harmful expression without recourse
 *   - - individually_harassed_listeners: Secondary bearer of costs (moderate/constrained) — remedy available only past a rarely reached threshold
 *   - - constitutional_courts: Agenda setter (institutional/identity_locked) — administers and polices the boundary through doctrine
 *   - - restriction_majorities_legislatures: Excluded seat (organized/constrained) — harm findings carry no adjudicative weight
 *   - - comparative_law_scholars: Analytical observer (analytical/analytical) — documents cross-jurisdictional variation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.48).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Near-Categorical Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'fa68404c-a5c2-4e87-9d98-e7cdde02478f').
narrative_ontology:cs_kernel_codification('fa68404c-a5c2-4e87-9d98-e7cdde02478f', fixed_text).
narrative_ontology:cs_authority_grounding('fa68404c-a5c2-4e87-9d98-e7cdde02478f', lineage).
narrative_ontology:cs_interpretation_layer_present('fa68404c-a5c2-4e87-9d98-e7cdde02478f').
narrative_ontology:cs_reading_relation('fa68404c-a5c2-4e87-9d98-e7cdde02478f', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('fa68404c-a5c2-4e87-9d98-e7cdde02478f', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('fa68404c-a5c2-4e87-9d98-e7cdde02478f', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa68404c-a5c2-4e87-9d98-e7cdde02478f', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('fa68404c-a5c2-4e87-9d98-e7cdde02478f', foundational, listener_harm_confers_no_restrictive_force).
narrative_ontology:cs_axiom_status(listener_harm_confers_no_restrictive_force, holdable).
narrative_ontology:cs_axiom_grounding('fa68404c-a5c2-4e87-9d98-e7cdde02478f', listener_harm_confers_no_restrictive_force, deontological).
narrative_ontology:cs_axiom('fa68404c-a5c2-4e87-9d98-e7cdde02478f', secondary, categorical_exclusions_closed_set).
narrative_ontology:cs_axiom_status(categorical_exclusions_closed_set, holdable).
narrative_ontology:cs_axiom_grounding('fa68404c-a5c2-4e87-9d98-e7cdde02478f', categorical_exclusions_closed_set, conventional).
narrative_ontology:cs_reference_frame('fa68404c-a5c2-4e87-9d98-e7cdde02478f', literal_nonabridgement_baseline).
narrative_ontology:cs_drift_state('fa68404c-a5c2-4e87-9d98-e7cdde02478f', contemporary_balancing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa68404c-a5c2-4e87-9d98-e7cdde02478f', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, extremist_and_provocateur_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, high_reach_media_corporations).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targeted_identity_group_members).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, individually_harassed_listeners).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_primacy).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, viewpoint_neutrality_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, counter_speech_remedy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish criticism of governing majorities under threat of prosecution. The bright-line protection is the principal barrier between their expression and state suppression machinery, and it operates automatically — no tribunal weighs whether their cause is worthy. The same rule that shields them shields the speakers who attack them, and they cannot modify the boundary they depend on.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, political_dissidents, beneficiary,
    powerless, biographical, constrained, national).

% Advocate programs that attack identifiable groups and deliberately shock public sentiment. Protection attaches wherever they operate within the jurisdiction, and they can move their messaging across cities, platforms, and formats at will. They bear no reciprocal burden under the arrangement and lose nothing if it stands.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, extremist_and_provocateur_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Monetize attention at national and international scale, including inflammatory and false material, with minimal content liability. They fund litigation that defends the protection boundary, organize through trade associations, and can shift incorporation, hosting, and distribution across borders while domestic protection continues to hold.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, high_reach_media_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Litigate and organize in defense of the protection boundary, routinely representing clients whose views they find repellent as a demonstration of principle. Organizational missions, professional reputations, and funding streams formed around the boundary's maintenance; erosion of the boundary would dissolve the role they occupy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, civil_liberties_advocates, beneficiary,
    organized, biographical, identity_locked, national).

% Belong to groups that recurring protected expression marks as dangerous, criminal, or unfit for equal citizenship. Membership is not renounceable, and exposure follows them across neighborhoods, workplaces, and platforms. Their available responses — counter-expression, avoidance, private association — are costly and unevenly resourced, and formal complaints are dismissed at the threshold because the expression falls on the protected side of the line.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targeted_identity_group_members, payer,
    powerless, generational, trapped, national).

% Endure sustained, targeted hostile communication from speakers the arrangement protects. Some can change addresses, employers, or accounts, at substantial personal and financial cost; others cannot. The conduct they report qualifies for restriction only if it crosses the narrow threat category, which few patterns reach.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, individually_harassed_listeners, payer,
    moderate, biographical, constrained, national).

% Draw and police the protection boundary through doctrine, striking down legislative restrictions and refining the categorical exceptions. Generations of precedent constitute the institution's self-understanding; reopening the baseline would unsettle the court's accumulated identity, its legitimacy claims, and the settled expectations of every actor that litigates before it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Pass statutes answering constituent demand for relief from harmful expression. Their factual findings about harm receive essentially no weight when the statutes reach adjudication. They remain formally central to the political process yet are structurally shut out of boundary-setting, able to legislate only inside lines drawn elsewhere and defended by other institutions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, restriction_majorities_legislatures, excluded,
    organized, biographical, constrained, national).

% Document how jurisdictions with narrower and broader protection regimes handle the same expressive conflicts, tracking downstream effects on dissent, harassment, and public discourse. They hold no position in the domestic settlement and bear none of its costs.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, comparative_law_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, high_reach_media_corporations).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes case-by-case harm weighing from the state's expressive toolkit: by refusing to adjudicate listener-harm claims, the arrangement denies every temporary majority the discretionary instrument that expressive persecution campaigns have historically run on, and gives dissenters a predictable bright line instead of a tribunal's judgment call.
% TRANSFER_FUNCTION: Moves the costs of harmful protected expression — fear, exclusion, reputational damage, defensive effort — from the speakers who produce it to the listeners and groups who absorb it, without compensation; simultaneously moves boundary-setting power over expression from legislatures to courts, and effective immunity to speakers in proportion to their reach.
% ABSENT_VOICES: Targets of protected harmful expression were absent from the conversation in which the boundary was theorized: the doctrine was articulated by and for speakers and states, and the people who absorb the costs of protected speech enter the story only as hypothetical counterspeakers. Restriction-seeking legislatures are formally present in politics but their harm findings carry no adjudicative weight in the forum that decides.
% DISAPPEARANCE_RATIONALE: If the near-categorical boundary vanished overnight, legislatures would enact restriction statutes within a session, prosecution of dissent would resume under newly written harm rationales, and publishers, broadcasters, and platforms would reorganize around liability avoidance — the expressive economy would rearrange itself around permission structures within months.
% FOUNDING_PROBLEM: State persecution of dissent: seditious libel prosecutions, prior restraint and licensing of press and pulpit, and majoritarian punishment of religious and political minorities. The arrangement was built to strip government of the discretionary instrument that had silenced opposition in the founding era.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative human-rights monitors corroborate that the founding problem was real and remains live in jurisdictions lacking the arrangement. Critics outside the beneficiary set — dignity theorists, targeted-community organizations, and comparative scholars documenting narrower regimes that nonetheless protect dissent — attest that the anti-persecution rationale no longer covers most of what the present boundary shields. No single source outside the dispute adjudicates the status; the corroboration itself splits, which is the signal recorded here.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48: the arrangement shifts real, uncompensated costs (fear, exclusion, reputational injury, defensive expenditure) onto identifiable groups, and the shift concentrates as speaker reach grows — but the same structure confers broad autonomy benefits enjoyed diffusely by every speaker, including the vulnerable. Suppression 0.55: the arrangement actively forecloses the legislative alternative through judicial review and closes the targets' remedial path at the threshold, while counter-speech, private moderation, and social sanction remain open, so foreclosure is substantial but incomplete. Theater 0.22: the doctrine performs real adjudicative work; ceremonial reaffirmation is a minor share. Accessibility_collapse 0.52: alternative regulatory designs (harm-based tort expansion, anti-subordination ordinances) collapse on contact with the doctrine, but private-ordering and counter-speech substitutes persist. Resistance 0.66: recurring restrictionist movements, sustained scholarly opposition, and comparative pressure contest the boundary continuously. Temporal series (one shared grid, interval mapped to roughly 1919–2019): base_extractiveness climbs from 0.28 to 0.48 as amplification technology multiplies speaker reach faster than the boundary narrows; theater_ratio starts high (rhetorical celebration coexisting with active suppression in the early era), bottoms out mid-interval as the doctrine matures into working machinery, then ticks up modestly as symbolic reaffirmation grows alongside the functional core; suppression_requirement ratchets upward as restriction attempts grow more legally sophisticated and the enforcement apparatus must strike down ever more elaborately wrapped statutes. The rising extraction trajectory is the T17-relevant signal: accumulation, not decay.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from identical structure. From the dissident seat, the boundary is the wall that stands between their expression and the state — the arrangement is the precondition of their participation. From the targeted-listener seat, the same wall is the thing that leaves them exposed: the rule that protects their tormentor is experienced as their abandonment. From the court's seat, the boundary is fidelity — the institution's identity consists in holding the line its predecessors drew. From the media corporation's seat, the boundary is license — an operating condition priced into every editorial decision. The engine computes these divergences from power, exit, and directional data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: political_dissidents (victim-adjacent power, constrained exit) sit near the beneficiary pole but less extremely than the others because they also absorb hostile protected speech aimed at them; extremist_and_provocateur_speakers (mobile) derive near-full beneficiary position; high_reach_media_corporations (arbitrage-grade exit, global scope) derive the lowest d in the story; civil_liberties_advocates derive low d despite identity_lock, since identity lock modulates exit, not benefit. Declared victims derive high directionality: targeted_identity_group_members (trapped, generational exposure) sit nearest the full-target pole; individually_harassed_listeners (constrained) sit slightly inside them. Constitutional_courts are undeclared on the beneficiary/victim axis; as agenda_setters they collect administrative and legitimacy returns from the arrangement's persistence, placing them toward the beneficiary side of symmetric. Restriction_majorities_legislatures hold the excluded seat with no declared structural position — the canonical fallback governs, and the exclusion itself is documented in absent_voices rather than forced into a directionality number.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — disabling state persecution of dissent — is genuinely still live at the margins, but the arrangement's scope has outrun its genealogy: the boundary now chiefly shields commercial amplification, group-directed hostility, and falsehood at scale, activities the founding problem never contemplated. Because the founding_problem_status is contested rather than dead, the mismatch consumer should not fire the zombie flag; the honest reading is partial obsolescence with a live residual core. Mandatrophy discipline prevents two symmetrical errors here: labeling the whole arrangement pure coordination erases the identifiable populations bearing its costs; labeling it pure extraction erases the dissent protection that vulnerable speakers measurably depend on. The tangled_rope claim preserves both halves and locates the dispute where it structurally belongs — in the width of the boundary, which is precisely what the sibling readings vary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (absolutist_reading) of the speech_protection_kernel; what structurally changes if a sibling reading is adopted instead?',
    'Adoption of a sibling reading is observable in doctrine: a harm_threshold or dignity reading enlarges the cognizable-victim set (targeted groups gain standing), a marketplace or democratic_participation reading keeps the wide boundary but re-grounds it. Track doctrinal admissions of harm evidence at the restriction stage.',
    'Under harm_threshold_reading or dignity_reading, the victim arrays enlarge and epsilon recomputes over a wider cost-bearing population, likely raising measured extraction; under marketplace_reading or democratic_participation_reading, the boundary width holds and this story''s numbers approximately carry over with a changed vindication set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: classification is indexed to the absolutist reading; sibling readings are different constraints, not measurement settings.').

omega_variable(
    naturality_vs_construction,
    'Is near-categorical speech protection a discovered feature of political morality (speaker autonomy as a pre-political limit on state power) or a constructed constitutional settlement that serves identifiable interests?',
    'Cross-jurisdictional comparison: if societies with narrower boundaries show no systematic loss of dissent protection or speaker welfare attributable to the difference, the categorical width is revealed as construction serving particular beneficiary coalitions rather than a structural necessity.',
    'If constructed-and-interest-serving, the arrangement''s persistence requires the enforcement machinery the metrics already register, and the beneficiary asymmetry weighs heavier in classification; if discovered, the boundary approximates a structural limit and the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_construction, conceptual, 'Whether the wide boundary is natural limit or interested construct.').

omega_variable(
    amplification_scale_drift,
    'Does the arrangement''s cost-shifting scale with communication technology, such that a boundary calibrated for pamphleteering extracts differently at broadcast and platform scale?',
    'Compare harm incidence per protected utterance across technology eras using the temporal series extended past the current interval; test whether target-population costs track reach concentration rather than utterance count.',
    'If extraction scales with reach, the rising base_extractiveness trajectory steepens under platform-scale amplification and the arrangement drifts toward the extraction-dominated end of its hybrid range; if not, the current trajectory plateaus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amplification_scale_drift, empirical, 'Technology-mediated scaling of the boundary''s cost-shifting.').

omega_variable(
    counter_speech_sufficiency,
    'Is counter-speech a genuine remedy that renders listener costs non-shifted, or is it unequally available in a way that makes the costs extracted from those least able to answer?',
    'Measure reach asymmetry between originators of group-directed protected expression and the targeted populations'' responsive capacity; audit whether counter-speech outcomes correlate with resources rather than merit.',
    'If counter-speech systematically fails for low-resource targets, the uncompensated cost-shifting is properly counted as extraction from a trapped population and effective extraction for the victim seats rises; if it succeeds broadly, the arrangement''s hybrid classification softens toward coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_sufficiency, empirical, 'Adequacy of the counter-speech substitute for bearing the boundary''s costs.').

omega_variable(
    categorical_boundary_panic_cycles,
    'Are the narrow categorical exclusions stable and principled, or do they expand and contract with moral panic cycles (sedition scares, terrorism panics, disinformation crises)?',
    'Code the exclusion set''s width across panic and calm periods; test whether threat-category admissions cluster temporally with security crises and revert afterward.',
    'Panic-driven cycling would mean the boundary''s width — the variable that determines who pays — is set by episodic fear rather than principle, injecting oscillation into epsilon and complicating any steady-state classification; a stable exclusion set supports the current flat-to-rising trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_boundary_panic_cycles, empirical, 'Stability of the closed exclusion set under crisis pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_absolutist_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.34).
narrative_ontology:measurement(spk_absolutist_tr_t15, speech_protection_kernel__absolutist_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(spk_absolutist_tr_t30, speech_protection_kernel__absolutist_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(spk_absolutist_tr_t45, speech_protection_kernel__absolutist_reading, theater_ratio, 45, 0.16).
narrative_ontology:measurement(spk_absolutist_tr_t60, speech_protection_kernel__absolutist_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(spk_absolutist_tr_t75, speech_protection_kernel__absolutist_reading, theater_ratio, 75, 0.17).
narrative_ontology:measurement(spk_absolutist_tr_t90, speech_protection_kernel__absolutist_reading, theater_ratio, 90, 0.2).
narrative_ontology:measurement(spk_absolutist_tr_t100, speech_protection_kernel__absolutist_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(spk_absolutist_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(spk_absolutist_be_t15, speech_protection_kernel__absolutist_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(spk_absolutist_be_t30, speech_protection_kernel__absolutist_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(spk_absolutist_be_t45, speech_protection_kernel__absolutist_reading, base_extractiveness, 45, 0.4).
narrative_ontology:measurement(spk_absolutist_be_t60, speech_protection_kernel__absolutist_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(spk_absolutist_be_t75, speech_protection_kernel__absolutist_reading, base_extractiveness, 75, 0.46).
narrative_ontology:measurement(spk_absolutist_be_t90, speech_protection_kernel__absolutist_reading, base_extractiveness, 90, 0.47).
narrative_ontology:measurement(spk_absolutist_be_t100, speech_protection_kernel__absolutist_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(spk_absolutist_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(spk_absolutist_su_t15, speech_protection_kernel__absolutist_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(spk_absolutist_su_t30, speech_protection_kernel__absolutist_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(spk_absolutist_su_t45, speech_protection_kernel__absolutist_reading, suppression_requirement, 45, 0.44).
narrative_ontology:measurement(spk_absolutist_su_t60, speech_protection_kernel__absolutist_reading, suppression_requirement, 60, 0.49).
narrative_ontology:measurement(spk_absolutist_su_t75, speech_protection_kernel__absolutist_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement(spk_absolutist_su_t90, speech_protection_kernel__absolutist_reading, suppression_requirement, 90, 0.54).
narrative_ontology:measurement(spk_absolutist_su_t100, speech_protection_kernel__absolutist_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'free speech' conflates five structurally distinct commitments sharing one constitutional kernel. This file authors the absolutist reading only: widest boundary, listener harm non-cognizable, smallest victim set. The harm_threshold_reading and dignity_reading instantiate different constraints with enlarged cognizable-victim sets and correspondingly different epsilon; the marketplace_reading and democratic_participation_reading share this reading's rejection of harm-based restriction but ground it in truth-discovery and self-governance respectively. Influence runs outward from this reading: its categorical formulation supplies the baseline that the conditional readings amend, so degradation or consolidation here propagates to every sibling through the family edges declared above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
