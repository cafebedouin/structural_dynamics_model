% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Constitutional Positivist Reading: Text Plus Amendment
 *   domain: political/legal
 *
 * SUMMARY:
 *   The U.S. Constitution as interpreted under the positivist reading is a
 *   constraint that anchors constitutional meaning to the text's face value
 *   and channels legitimate constitutional change through formal amendment.
 *   The reading claims that justices should interpret the Constitution within
 *   textual bounds and that social change requiring new constitutional
 *   protections must pursue Article V amendment rather than judicial
 *   innovation. This constraint exists in contest with originalist and
 *   living-constitutionalist readings of the same constitutional text. The
 *   positivist reading sits between originalism (which locks meaning at
 *   ratification) and living constitutionalism (which permits meaning to
 *   evolve with society). It permits textual evolution—later amendments
 *   revise the standing text—but not judicial rewriting without amendment.
 *   The constraint is substantially extractive for groups seeking
 *   constitutional protection for interests not yet textualized; it benefits
 *   legislatures and amendment coalitions who retain the exclusive legitimacy
 *   to develop constitutional meaning.
 *
 * KEY AGENTS:
 *   - Supreme Court justices: enforce the textual boundary; constrained from expanding meaning unilaterally
 *   - Congress and state legislatures: hold exclusive formal amendment power; primary locus of constitutional development
 *   - Amendment coalitions: benefit from the amendment mechanism; must build supermajority consensus
 *   - Excluded political minorities: bear the cost of textual limitation; cannot access courts for novel claims
 *   - Temporal losers: groups needing constitutional protection in immediate timescales; face multi-generational amendment barrier
 *   - Originalist and living-constitutionalist seats: external critics; contested the reading's framing of constitutional meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.42).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.38).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Constitutional Positivist Reading: Text Plus Amendment").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "political/legal").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'ccd0cc27-c113-4681-b0c9-62082770567d').
narrative_ontology:cs_kernel_codification('ccd0cc27-c113-4681-b0c9-62082770567d', fixed_text).
narrative_ontology:cs_authority_grounding('ccd0cc27-c113-4681-b0c9-62082770567d', extraction).
narrative_ontology:cs_interpretation_layer_present('ccd0cc27-c113-4681-b0c9-62082770567d').
narrative_ontology:cs_reading_relation('ccd0cc27-c113-4681-b0c9-62082770567d', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccd0cc27-c113-4681-b0c9-62082770567d', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('ccd0cc27-c113-4681-b0c9-62082770567d', foundational, amendment_exclusive_legitimacy).
narrative_ontology:cs_axiom_status(amendment_exclusive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ccd0cc27-c113-4681-b0c9-62082770567d', amendment_exclusive_legitimacy, deontological).
narrative_ontology:cs_axiom('ccd0cc27-c113-4681-b0c9-62082770567d', foundational, textual_constraint_on_courts).
narrative_ontology:cs_axiom_status(textual_constraint_on_courts, holdable).
narrative_ontology:cs_axiom_grounding('ccd0cc27-c113-4681-b0c9-62082770567d', textual_constraint_on_courts, conventional).
narrative_ontology:cs_reference_frame('ccd0cc27-c113-4681-b0c9-62082770567d', constitutional_meaning_text_plus_amendment).
narrative_ontology:cs_drift_state('ccd0cc27-c113-4681-b0c9-62082770567d', contemporary_amendment_stall, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ccd0cc27-c113-4681-b0c9-62082770567d', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, representative_institutions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, amendment_coalitions).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, excluded_political_minorities).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, temporal_losers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, congress_and_state_legislatures).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, state_governments).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the constitutional text; declare meaning; refuse to expand beyond textual bounds. Under the positivist reading, their power is simultaneously exercised and limited: they set the terms of constitutional interpretation but cannot unilaterally revise the Constitution. They must continually defend the textual boundary against political pressure to reach socially necessary outcomes without amendment. The constraint requires them to explain why courts should not recognize rights not yet textualized—a politically costly position.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).

% Hold exclusive formal power to amend the Constitution. Under the positivist reading, they are the primary agents of legitimate constitutional change. They benefit because this arrangement preserves legislative supremacy and prevents courts from unilaterally expanding constitutional rights without electoral accountability. They also benefit from the high amendment bar: it prevents temporary electoral majorities from rewriting the Constitution, protecting their legislative achievements.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, congress_and_state_legislatures, beneficiary,
    institutional, generational, mobile, national).

% Command the supermajority consensus required for amendment. When successful, they have exclusive legitimate authority to reshape foundational law. The constraint aligns their interests with durable constitutional commitment: achieving amendment requires building lasting consensus, not merely electoral victory. Failed amendments (those that cannot achieve supermajority support) are correctly interpreted as lacking sufficient durable backing for constitutional change.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, amendment_coalitions, beneficiary,
    organized, generational, mobile, national).

% Lack the electoral power to command an amendment coalition. Under the positivist reading, their constitutional claims are routed exclusively through courts when textual hooks exist, and routed to amendment when they do not. For claims lacking textual basis, the route is impossible: they cannot realistically build the supermajority coalition for amendment, and courts are barred from recognizing their claims. They are identity-locked to the status of 'groups the Constitution does not protect,' which shapes their political identity and available strategies. The constraint extracts from them in the form of permanently foreclosed constitutional protection absent a multi-generational political realignment.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, excluded_political_minorities, payer,
    powerless, biographical, identity_locked, national).

% Groups whose interests conflict with standing constitutional text and who cannot command an amendment coalition. They face simultaneous barriers: courts are structurally barred from recognizing novel claims (textual boundary), and amendment is realistically impossible in their immediate timescale. Unlike excluded minorities with generational patience, they need constitutional protection or legal redress in years or decades, not centuries. The constraint extracts from them by denying access to both judicial development and amendment—they are trapped in the constitutional status quo regardless of changing social needs.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, temporal_losers, payer,
    moderate, immediate, trapped, national).

% Hold the reading that constitutional meaning is fixed at ratification and amendment-driven change erodes the Constitution's binding force. They are excluded from shaping the positivist constraint because they dispute its foundational claim: that text can legitimately evolve through amendment. They critique the reading from outside, arguing it is insufficiently anchored to original meaning. They would offer an alternative constraint with different extraction profiles if their reading were operative instead.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_justices_and_scholars, excluded,
    institutional, generational, constrained, national).

% Hold the reading that constitutional meaning legitimately evolves through judicial interpretation in light of evolving social understanding. They are excluded from shaping the positivist constraint because they dispute its core limit: that courts should not evolve meaning beyond the text. They critique the reading from outside, arguing it is insufficiently responsive to social change and leaves vulnerable groups unprotected. They would offer an alternative constraint with very different extraction profiles (courts as primary constitutional developers, minorities with court access, no amendment requirement).
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, excluded,
    institutional, generational, constrained, national).

% Hold veto power over amendments (ratification by three-fourths of states) and benefit from that structural position; they can block constitutional changes even with supermajority congressional support. They also bear the cost of constitutional constraint to text: their police powers are limited to what the text permits, and they cannot unilaterally invoke evolving constitutional meaning to expand jurisdiction. The constraint requires them to respect textual boundaries on their authority (e.g., Commerce Clause limits on state regulation) while giving them powerful voice in amendment.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, state_governments, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, state_governments, beneficiary).

% Analyze and critique the constraint by arguing that text-bound interpretation leaves protected groups vulnerable to majoritarian exclusion and that temporal losers lack realistic avenues for constitutional change. They operate outside the constraint as institutional critics; under the positivist reading, their political strategy is routed toward electoral coalition-building for amendment rather than toward litigation-based constitutional development. They testify before Congress, publish academic work, and mobilize public opinion to support amendment, but they cannot persuade courts to recognize claims absent textual basis.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, civil_rights_advocates, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of constitutional stability in a large republic: how to preserve foundational law against casual amendment while retaining legitimate mechanisms for necessary change. The positivist reading coordinates by anchoring meaning to text (which is fixed and public) and channeling constitutional change through amendment (which requires supermajority consensus across diverse jurisdictions).
% TRANSFER_FUNCTION: Transfers the primary locus of constitutional development from courts to legislatures and amendment coalitions. Justices relinquish the power to expand meaning beyond text; legislatures and state governments gain the exclusive formal mechanism for constitutional change. The cost is borne by groups seeking constitutional protection for interests not yet textually recognized.
% ABSENT_VOICES: Excluded political minorities and temporal losers would object that the constraint entrenches majoritarian constitutional meaning and forecloses rapid redress for novel claims. They are excluded because the positivist reading is authored from within the institutional perspective (courts, legislatures) rather than from the perspective of those seeking access to courts for constitutional claims. International human-rights advocates and future generations (who will inherit the consequences of today's textual boundaries) are also absent.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished—if courts reverted to reading constitutional meaning beyond the text in light of evolving social values—the amendment process would become a secondary, ornamental mechanism. Constitutional development would accelerate dramatically, and the political balance would shift: courts would become the primary site of constitutional change rather than legislatures and amendment coalitions. State governments' veto power over amendment would atrophy. The institutional settlement would reorganize around judicial supremacy over constitutional meaning.
% FOUNDING_PROBLEM: The Framers drafted a Constitution understood to be the supreme law binding all branches. The founding problem was how to establish a written constitution that would remain authoritative across centuries of social change without becoming either obsolete or subject to casual revision. The positivist reading claims the solution is text-based meaning (clear, public, stable) plus democratic amendment (legitimate, durable, supermajority-protective).
% FOUNDING_PROBLEM_CORROBORATION: The positivist reading's account of the founding problem is attested by the Framers' debates (primary sources showing concern for stability and amendment procedure). However, the reading's claim that TEXT PLUS AMENDMENT solves the founding problem is contested: originalists argue that textual meaning fixed at ratification is the solution; living constitutionalists argue that evolving judicial interpretation is the solution. No reading commands unanimous external corroboration. The reading's legitimacy rests on its coherence with the Framers' structural choice (written text, Article V amendment), not on unanimous agreement about whether that choice succeeded.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint extracts from groups seeking judicial recognition of novel constitutional claims, but it does not extract from the winning amendment coalitions or from legislatures—they benefit from the institutional arrangement. Suppression is moderate (0.38): the constraint suppresses judicial creativity through textual boundary enforcement, but it does not suppress legislative action—legislatures can draft amendments freely (the suppression is structural: high amendment bar prevents easy change, not active coercion of legislators). Theater ratio is low (0.22): the textual-interpretation function is genuine, and the amendment process is real, though courts do perform some theatrical constraint-defense when explaining why they cannot recognize unarticulated rights. The accessibility-collapse measurement (0.61) reflects that groups locked out of current text have genuinely reduced alternative pathways—amendment is the only legitimate route, but it is extremely high-friction. Resistance is high (0.71): courts regularly face pressure to expand meaning, civil-rights advocates argue for living constitutionalism, and the constraint is continually contested. The measurement series run on one shared time grid so every metric is authored at every examined time point. Slight elevation of extractiveness from t=0 to t=30, then plateauing, reflects growing diversity of excluded claims as society changes, followed by stabilization as legal culture acknowledges the constraint's persistence.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court seat should compute differently from the amendment-coalition seats. From the Court's perspective, the constraint is a boundary it must defend—they are constrained by it. From the amendment-coalition perspective, the constraint benefits them: they retain exclusive legitimate power to develop constitutional meaning. From the excluded-minorities' perspective, the constraint is extractive: it prevents judicial access even when textual claims are strong. From the originalist perspective, the positivist reading is too permissive (it permits amendment to alter foundational meaning). From the living-constitutionalist perspective, the reading is too restrictive (it forecloses necessary judicial evolution). The engine computes these divergences per-seat from the structural data—the multiple perspectives are not author-reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Representative institutions (Congress, state governments, amendment coalitions) are near the beneficiary end of the directionality spectrum: they retain exclusive legitimate power and benefit from the high amendment bar that prevents rivals from unilaterally revising their victories. Excluded minorities and temporal losers are near the target end: they bear the cost of textual limitation without having the political power to command amendment. Supreme Court justices sit near the symmetric point: they benefit from the clarity of textual boundaries (which simplifies their interpretive task) but also bear the constraint of being structurally limited from addressing novel constitutional claims. The directionality derives from whether the agent benefits from the amendment monopoly (beneficiary → low d) or suffers from textual exclusion (victim → high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading occupies a contentious middle ground. It claims to solve the founding problem (stable, legitimate constitutional development) through text plus amendment. However, the reading faces mandatrophy pressure from both flanks: originalists argue the positivist allowance for amendment-driven textual change erodes originalism's anchoring principle; living constitutionalists argue the positivist restriction on judicial evolution abandons the practical solution (courts) in favor of a theoretically elegant but institutionally unrealistic mechanism (amendment—only 27 amendments in 235 years). The claim is that amendment IS the legitimate mechanism; the metric shows that amendment is extraordinarily high-friction and that courts face enormous pressure to evolve meaning. If the amendment process permanently stalls (a real possibility in a polarized polity), the positivist reading's founding-problem claim becomes obsolete: it will have proven unable to deliver constitutional change even when supermajorities support it. That obsolescence is not mandatrophy in the snare sense (a cover story for pure extraction); it is mandatrophy in the constructive sense: the reading's justification (amendment delivers legitimate change) collides with empirical failure (amendment delivers almost nothing). The reading survives by theatrical invocation of amendment possibility, even as the actual mechanism atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_efficacy_empirical,
    'Does the amendment mechanism actually deliver constitutional change when supermajorities support it, or has the mechanism permanently atrophied?',
    'Long-term empirical observation: if no new amendments pass for 50+ years despite persistent supermajority support for particular changes (measurable via polling), the mechanism is atrophied; if amendments continue at historical rates or accelerate, the mechanism is viable.',
    'If amendment is atrophied, the positivist reading''s founding-problem claim becomes obsolete (it promises legitimate change through amendment but cannot deliver it), and the reading collapses toward piton (theatrical invocation of amendment while courts in fact redevelop meaning, or fundamental rights go unprotected). If amendment remains viable, the positivist reading retains its coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_efficacy_empirical, empirical, 'Whether the amendment mechanism is a functional or theatrical path to constitutional change.').

omega_variable(
    judicial_textual_boundary_observance,
    'Do courts genuinely limit themselves to textual bounds under the positivist reading, or do they covertly innovate through interpretation while claiming textual fidelity?',
    'Comparative study of judicial opinions: count instances where courts interpret language in ways inconsistent with contemporary textual meaning (e.g., Fourth Amendment ''searches'' applied to electronic surveillance not imagined in 1791, yet claimed as textual interpretation). If covert innovation dominates, the boundary is theatrical; if courts genuinely refuse unarticulated claims, the boundary is real.',
    'If courts covertly innovate, the positivist reading''s constraint on judicial power is theater, and the actual structure is living constitutionalism disguised as textualism. The victims (excluded minorities) may not actually be excluded if courts find textual hooks for novel claims. If courts genuinely refuse unarticulated claims, the constraint is real, and excluded minorities are actually trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_textual_boundary_observance, empirical, 'Whether textual constraint on judicial power is real or theatrical.').

omega_variable(
    reading_contestation_foreclosure,
    'Can the positivist reading coexist with originalism and living constitutionalism in a single Supreme Court, or do the readings logically foreclose one another?',
    'Structural analysis of reading axioms: if the positivist insistence on amendment-exclusive change logically contradicts the originalist insistence on text-fixed meaning, they foreclose; if different justices can hold different readings without contradiction (one votes positivist, another originalist, both consistent within their own frames), they coexist.',
    'If readings foreclose, Supreme Court composition becomes determinative—the majority reading controls constitutional interpretation, and the reading election is high-stakes. If readings coexist, multiple readings can operate simultaneously as competing frameworks within the Court''s overall practice (some opinions positivist, some originalist, some living—the Court holds all three readings implicitly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_foreclosure, conceptual, 'Whether the positivist reading logically forecloses its siblings or permits coexistence.').

omega_variable(
    supermajority_majoritarianism,
    'Does the positivist reading''s reliance on supermajority amendment protection actually protect minorities, or does it entrench majoritarian constitutional meaning by making change so difficult that majorities can ignore minority claims?',
    'Empirical observation over multi-generational timescales: if historically disadvantaged groups secure constitutional protections via amendment at rates comparable to historically advantaged groups, the supermajority requirement is neutral; if amendment is asymmetrically harder for minority-benefiting changes, the requirement is majoritarian in effect.',
    'If the supermajority requirement is genuinely neutral, the positivist claim that amendment is the democratic path to constitutional change is coherent. If the requirement is asymmetrically difficult for minority claims, the reading entrenches majoritarian constitutional meaning under the cover of democratic legitimacy—the constraint becomes more extractive than authored, approaching snare-like operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_majoritarianism, empirical, 'Whether the amendment requirement''s supermajority threshold protects minorities or entrenches majorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__positivist_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__positivist_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__positivist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__positivist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__positivist_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__positivist_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__positivist_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__positivist_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__positivist_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__positivist_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__positivist_reading, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% The constraint 'us_constitution_1787__positivist_reading' is one reading of the contested kernel 'us_constitution_1787'. Three constraint stories decompose the constitutional meaning dispute: (1) originalist_reading fixes meaning at ratification; (2) positivist_reading anchors meaning to text plus amendment; (3) living_reading permits evolutionary judicial interpretation. Each reading produces a different ε value and a different stakeholder extraction profile, though all three address the same kernel. The three stories are linked via network.affects_constraints to model their interpretive competition and mutual influence. The positivist reading influences both siblings: it competes with originalism for textual legitimacy and with living constitutionalism for the seat of constitutional development authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
