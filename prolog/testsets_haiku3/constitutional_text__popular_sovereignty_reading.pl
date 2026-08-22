% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text Authority via Popular Sovereignty (Reading)
 *   domain: political/constitutional/philosophical
 *
 * SUMMARY:
 *   This constraint instantiates the popular sovereignty reading of
 *   constitutional authority: the claim that a written constitution derives
 *   legitimacy from constituent power (the sovereign will of the people) and
 *   that neither courts nor legislatures can claim final interpretive
 *   authority. Popular mobilization through amendment, convention, or
 *   revolutionary action is treated as the meta-authority that can override
 *   institutional interpretations. This reading sits in contest with judicial
 *   supremacy (courts are final) and legislative sovereignty (parliaments are
 *   final) readings of the same constitutional kernel. The constraint's
 *   operation extracts authority from institutional actors and transfers it
 *   to extra-institutional democratic expression; it benefits those who
 *   mobilize popular power and the abstract value of participatory democracy,
 *   while imposing costs on institutional stability and expert authority. The
 *   high theater ratio (0.67) reflects the gap between the reading's formal
 *   assertion of popular authority and the practical difficulty of
 *   actualizing that authority except during episodic amendment or convention
 *   moments—most of the time, courts and legislatures operate as if they have
 *   greater authority than the reading nominally grants them.
 *
 * KEY AGENTS:
 *   - popular_mobilization (organized actors leading amendment campaigns, conventions, revolutionary bodies) — allocates authority and organizes mass political will
 *   - institutional_stability (courts, legislatures, administrative apparatus) — bears the cost of subordination to popular override
 *   - specialized_expertise (constitutional scholars, senior judges, expert advisors) — pays the cost of de-centered epistemological authority
 *   - ordinary_citizens (the demos in aggregate) — formal beneficiaries but episodically empowered
 *   - amendment_framers/convention leaders (shapers of popular will expression) — agenda-setters under this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.58).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.42).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text Authority via Popular Sovereignty (Reading)").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "political/constitutional/philosophical").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '5d9f3842-9056-46c1-a06f-4e1a8c8a8d69').
narrative_ontology:cs_kernel_codification('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', formalized).
narrative_ontology:cs_authority_grounding('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', extraction).
narrative_ontology:cs_interpretation_layer_present('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69').
narrative_ontology:cs_reading_relation('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', foundational, constituent_power_supreme).
narrative_ontology:cs_axiom_status(constituent_power_supreme, holdable).
narrative_ontology:cs_axiom_grounding('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', constituent_power_supreme, deontological).
narrative_ontology:cs_axiom('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', foundational, institutional_subordination_to_demos).
narrative_ontology:cs_axiom_status(institutional_subordination_to_demos, holdable).
narrative_ontology:cs_axiom_grounding('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', institutional_subordination_to_demos, deontological).
narrative_ontology:cs_reference_frame('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', constituent_power_of_demos).
narrative_ontology:cs_drift_state('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', contemporary_institutional_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d9f3842-9056-46c1-a06f-4e1a8c8a8d69', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_mobilization).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, specialized_expertise).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, courts).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, ordinary_citizens).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized movements, amendment campaigns, and constitutional convention organizers collect authority and standing under this reading. Popular mobilization is treated as the legitimate source of constitutional meaning revision. Parties that can organize the demos effectively gain the power to reshape constitutional understanding. Their role is to aggregate diffuse citizen will into actionable constitutional claims.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, popular_mobilization, beneficiary,
    organized, generational, analytical, national).

% Courts, legislatures, and administrative agencies bear the cost of this reading's operation. They lose the ability to settle constitutional questions definitively; their interpretations remain subject to popular override through amendment or convention. They must continuously justify their constitutional reasoning to public opinion and face delegitimation if their interpretations drift too far from perceived popular will. The constraint forces institutional actors into defensive postures, perpetually accounting for their authority to external forces.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_stability, payer,
    institutional, generational, constrained, national).

% Constitutional scholars, senior judges with hermeneutic authority, and legal experts who claim specialized insight into constitutional meaning are de-centered by this reading. Their expertise is reduced to advisory status; the ultimate judge of constitutional meaning is treated as the people, not the expert. This removes the epistemic privilege that expertise-based authority normally commands. Experts remain relevant but subordinated, their conclusions subject to popular revision.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, specialized_expertise, payer,
    powerful, biographical, constrained, national).

% Courts pay the cost of subordination to popular authority and lose interpretive finality. However, they also benefit insofar as this reading protects them from legislative supremacy—the reading declares no institutional actor is supreme, which preserves courts' standing as independent interpreters. Courts are caught between cost (accountability to popular will) and benefit (parity with legislatures, protected from parliamentary override).
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, courts, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, courts, beneficiary).

% Legislatures are equally subordinated under this reading: they cannot claim constitutional supremacy, their statutes remain constrained by the constitution, and constitutional interpretation can be overridden by the people through amendment. They lose the ability to settle constitutional questions through parliamentary procedure. Like courts, they benefit from being protected against the other institution's supremacy claim, but they pay the cost of accountability to extra-institutional forces.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, legislature, beneficiary).

% Those who lead amendment campaigns and constitutional conventions are the shapers of popular will expression under this reading. They set the agenda for which constitutional questions are revisited, frame the alternatives presented to the public, and mobilize consent for particular revisions. Their power is conditional—they must genuinely mobilize the demos—but when successful, they exercise decisive authority over constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, amendment_framers, agenda_setter,
    organized, generational, analytical, national).

% Citizens hold formal authority as the demos in the aggregate but episodic power to exercise it. They benefit from the reading's assertion that constitutional authority flows from them, but their individual power to affect constitutional interpretation is diffuse. They can exercise influence only during amendment or convention moments, or through sustained mobilization campaigns. Between these episodes, they are governed by constitutional interpretation made by institutions.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, ordinary_citizens, beneficiary,
    powerless, biographical, mobile, national).

% Those who hold the judicial supremacy reading would argue that courts, not the people, have final say on constitutional meaning, and that popular amendment is limited to formal constitutional change while interpretation is judicial prerogative. They are excluded from the framework this reading instantiates because it denies their core premise. They form the basis of competing constitutional interpretation and active resistance to the popular sovereignty reading.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_supremacy_adherents, excluded,
    institutional, generational, analytical, national).

% Those who hold the legislative sovereignty reading argue that parliaments, not the people, have supreme constitutional authority through mechanisms like notwithstanding clauses or simple override. They are excluded from the framework this reading instantiates. They represent an alternative authority allocation that, if institutionalized, would foreclose or severely constrain the practical operation of the popular sovereignty reading.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislative_supremacy_adherents, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, popular_mobilization).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a meta-framework for resolving constitutional disputes when institutional actors diverge: by treating popular sovereignty as the ultimate arbiter, the reading offers a closure mechanism (amendment, convention, or mass mobilization) when courts and legislatures deadlock or drift. The coordination problem is: 'When institutions disagree on constitutional meaning, what principle determines whose interpretation prevails?' The reading answers: the people, through formal amendment or extra-institutional pressure.
% TRANSFER_FUNCTION: Transfers interpretive authority from institutional actors (courts and legislatures) to the demos as a mobilized political force. The costs borne by institutions and expertise are reframed as accountability burdens they must shoulder; the benefits accrue to those who mobilize popular power and to the abstract value of democratic participation itself. When amendment or convention succeeds, constitutional meaning shifts as the people direct, regardless of institutional preference.
% ABSENT_VOICES: Legal and constitutional elites not mobilized into the amendment or convention process are largely excluded from this reading's framework for authority—their expertise is de-centered. Institutional actors (judges, legislators) are nominally included as interpreters, but the reading subordinates them, so their normative authority is discounted. Those lacking the organizational capacity to mobilize the public (smaller factions, diffuse interests) find their constitutional claims harder to vindicate through the popular sovereignty mechanism, even though the mechanism formally treats all citizens as equals.
% DISAPPEARANCE_RATIONALE: If this reading of constitutional authority disappeared—i.e., if popular sovereignty were no longer treated as the ultimate source of constitutional meaning—institutions (courts and legislatures) would consolidate hermeneutic authority and constitutional change would depend on their decisions, not on popular mobilization. The constraint's absence would shift the entire framework for constitutional interpretation away from participatory oversight. Different institutional actors (or whichever gained supremacy in its place) would set the terms, and amendment/convention would lose its theoretical primacy as the source of legitimate constitutional change.
% FOUNDING_PROBLEM: The founding problem for this reading is the question of constitutional legitimacy under popular government: where does a written constitution's authority come from? This reading answers: from the constituent power of the people—the sovereign will that preceded and authored the constitution itself. The problem is: how can a fixed text bind a living people, and what happens when institutions diverge from popular will? The reading solves this by treating popular amendment and mobilization as the authoritative update mechanism, ensuring the constitution remains subordinate to the people's sovereign will rather than the people becoming subordinate to the constitution.
% FOUNDING_PROBLEM_CORROBORATION: This reading is attested by democratic theorists and popular sovereignty advocates (Ackerman, Tushnet, social contract tradition), who argue that constitutional legitimacy flows from constituent power and that institutions must remain accountable to the people. However, institutional actors (senior judges, legislatures) tend to contest the status: they argue constitutional stability and expertise-based interpretation require institutional finality, not perpetual popular override. Comparative constitutional law shows mixed corroboration: some jurisdictions treat amendment as the highest expression of constitutional authority (Switzerland, Denmark); others vest final authority in courts (US Supreme Court doctrine post-1803) or legislatures (UK parliamentary supremacy model). The founding problem remains live and contested across systems.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as tangled_rope because it combines genuine coordination (resolving constitutional disputes via a meta-authority principle that can break institutional deadlock) with asymmetric extraction (institutional actors bear the cost of perpetual accountability to popular forces they cannot fully control). Extractiveness is moderate (0.58 at t=20) rather than high because the extraction is ideologically justifiable (democratic accountability) even if costly to institutions; suppression is lower (0.42) because this reading does not require intensive coercive enforcement—it succeeds through normative appeal and periodic mobilization rather than constant suppression of alternatives. Theater is notably high (0.67) because a wide gap exists between the reading's formal claim (the people hold ultimate authority) and its actual operation (amendment is rare; most constitutional interpretation proceeds institutionally, and the people rarely assert override authority). The theater trajectory rises over time as institutional actors increasingly perform deference to popular sovereignty while maintaining de facto interpretive control, creating a gap between stated and actual authority allocation. Resistance is high (0.73) because institutional actors actively resist this reading through practice (ignoring amendment threat signals, consolidating interpretive power) and because competing readings (judicial and legislative supremacy) mount real counterclaims.
 *
 * PERSPECTIVAL GAP:
 *   The popular mobilization seat experiences this constraint as empowering: the reading vindicates their authority and provides theoretical justification for amendment campaigns and convention calls. The institutional stability seat experiences it as subordinating and costly: courts and legislatures lose closure and remain perpetually accountable. Specialized expertise experiences it as delegitimizing: their professional authority is reduced to advisory status, subordinate to popular judgment. The divergence is structural: the reading allocates authority differentially across seats, so the computed type per seat will vary—the engine should compute tangled_rope or snare from the institutional seats' perspective (they are targets of extraction) and rope or beneficiary-side assessment from the mobilization seat's perspective (they collect authority). The claim/metric independence is deliberate: this constraint is CLAIMED as tangled_rope (coordination + extraction) while the metrics describe moderate extractiveness and high theater, which might support piton or snare classification—the engine's divergence detection marks this as a theoretically justified but operationally contested constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural relationship to constitutional authority: popular mobilization has low d (beneficiaries of authority-vesting, no exit penalty), institutional stability has high d (targets of subordination, constrained exit—institutions cannot simply exit constitutional systems), specialized expertise has high d (authority-challenged, constrained exit), and ordinary citizens have moderate d (formal authority but diffuse and episodic power). The amendment_framers seat has low d (they gain agenda-setting authority under this reading). This creates a highly asymmetric directionality profile, which the engine will register as extractive despite the claim of tangled_rope—the asymmetry is the point.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading faces the mandatrophy question: is the founding problem (legitimate constitutional change under popular sovereignty) still live, or has it atrophied into theatrical invocation? The measurement series shows theater rising from 0.58 to 0.68 over time, which suggests the constraint increasingly operates through performance rather than function—the reading is invoked and cited, but actual constitutional change proceeds through institutional channels, and popular mobilization remains episodic. The status is contested: proponents argue the founding problem remains live (citizens can and do mobilize amendments, courts do cite popular sovereignty rhetoric, the threat of convention constrains institutions); critics argue the founding problem is dead (amendment is practically impossible in many systems, institutions ignore popular sentiment, the constraint persists only as theater). The high resistance measurement (0.73) supports the live-problem reading: if the constraint were truly atrophied, resistance would decay toward zero; sustained resistance suggests genuine contestation over whether popular authority or institutional authority prevails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_stability,
    'Is popular sovereignty a stable reading of the constitutional kernel, or is it a transient ideology that institutional actors routinely displace through practice?',
    'Longitudinal study of constitutional amendment rates, court invocations of popular sovereignty language, and correlation between mass mobilization and constitutional change. Tracking whether courts cite constituent power language when constraining their own power (vs. only when validating their decisions).',
    'If the reading is empirically stable (amendment and convention remain viable, courts genuinely constrain themselves by popular sentiment), the constraint persists as tangled_rope or rope. If institutional displacement is systematic (courts redefine popular sovereignty to preserve their authority, amendments become practically impossible), the constraint degrades to piton or theater. The empirical question determines whether mandatrophy is present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_stability, empirical, 'Whether popular sovereignty reading is substantively actionable or increasingly theater.').

omega_variable(
    coordination_extraction_boundary,
    'Does this reading genuinely solve a coordination problem (how to resolve constitutional disputes when institutions deadlock), or is the ''coordination'' function merely post-hoc rationalization for extracting authority from institutions?',
    'Examine historical moments when amendment or convention actually resolved an institutional deadlock (vs. when they failed or were never attempted). Test whether popular mobilization mechanism is invoked symmetrically for all constitutional disputes or only selectively when it advantages certain factions.',
    'If genuine coordination problem exists and is solved (deadlock resolution is measurably harder without the popular sovereignty mechanism), the tangled_rope classification holds—asymmetric extraction rides on real coordination. If the mechanism is rarely invoked and institutions routinely settle disputes without it, the constraint may be pure snare (extracting authority without solving coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether popular sovereignty mechanism solves a real coordination problem or merely extracts authority.').

omega_variable(
    forecloses_vs_coexists_question,
    'Does the popular sovereignty reading logically foreclose the judicial supremacy and legislative sovereignty readings (ruling them out within any single framework), or do all three remain simultaneously live positions held by different factions?',
    'Examine whether proponents of any single reading can coherently acknowledge the truth of a sibling reading without abandoning their own core commitments. Test whether a principled constitutional jurist can hold both popular sovereignty and judicial supremacy, or whether they necessarily conflict.',
    'If forecloses relation is correct, the kernel represents a genuinely contested choice between incompatible readings, and only one can ultimately be instantiated. If coexists_with is correct, all three remain live and the constraint''s authority allocation is perpetually contested—the kernel is under-determined across readings. This determines whether the cs_structure.reading_relations should use forecloses or coexists_with for sibling relationships.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forecloses_vs_coexists_question, conceptual, 'Whether popular sovereignty reading logically excludes or merely conflicts with institutional supremacy readings.').

omega_variable(
    identity_fusion_expertise,
    'Do institutional actors and specialists fused their identities with their interpretive authority such that loss of final interpretive power feels existentially threatening, or is the suppression of this reading caused primarily by structural interests (e.g., courts lose power, lose resources)?',
    'Study how courts and legal elites respond to popular sovereignty rhetoric—do they argue substantively against it or dismiss it as impossible to operationalize? Post-defeat institutional behavior: if subordination to popular will is imposed, do institutions gradually adapt their identity or resist with high intensity?',
    'If identity fusion is present, suppression of the reading is partly internalized (experts believe popular sovereignty is wrong, not merely costly)—the constraint persists partly through cognitive capture, not just structural power. If resistance is purely structural-interest based, suppression can be lifted by changing power relations or exit incentives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_expertise, conceptual, 'Whether institutional resistance to popular sovereignty reading is ideological or interest-based.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__popular_sovereignty_reading, theater_ratio, 10, 0.61).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__popular_sovereignty_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement_basis(cons_tr_t30, projected).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.69).
narrative_ontology:measurement_basis(cons_tr_t40, projected).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__popular_sovereignty_reading, theater_ratio, 50, 0.67).
narrative_ontology:measurement_basis(cons_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_text__popular_sovereignty_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text__popular_sovereignty_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(cons_be_t30, projected).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement_basis(cons_be_t40, projected).
narrative_ontology:measurement(cons_be_t50, constitutional_text__popular_sovereignty_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(cons_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_text__popular_sovereignty_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text__popular_sovereignty_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(cons_su_t30, projected).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.43).
narrative_ontology:measurement_basis(cons_su_t40, projected).
narrative_ontology:measurement(cons_su_t50, constitutional_text__popular_sovereignty_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(cons_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested constitutional kernel (the authority and meaning of written constitutional text). All three readings—popular_sovereignty_reading, judicial_supremacy_reading, and legislative_sovereignty_reading—share the same subject (constitutional authority) but instantiate different constraints through different epistemic frameworks and authority allocations. They form a constraint family linked via network.affects_constraints. Each reading has its own ε (extractiveness measured differently depending on what is treated as the standing arrangement under contest), its own beneficiary/victim structure (who collects from the authority allocation), and its own claimed type. Sibling readings affect this one: if judicial supremacy becomes institutionally dominant, it forecloses the practical operation of popular sovereignty (courts claim final authority, amendment becomes theoretically meaningless). If legislative sovereignty dominates, it competes with popular sovereignty for authority allocation. The three readings are not the same constraint viewed from different angles; they are genuinely different constraints grounded in the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
