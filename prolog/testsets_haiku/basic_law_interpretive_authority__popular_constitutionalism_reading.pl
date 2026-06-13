% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Distributed Interpretive Authority
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Popular constitutionalism is a reading of basic constitutional law that
 *   locates interpretive authority not in courts or legislatures but in the
 *   people themselves, understood as distributed democratic actors.
 *   Constitutional meaning emerges through ongoing contestation among courts,
 *   legislatures, and popular movements rather than reaching terminal closure
 *   in any single institution. This reading claims that neither the judiciary
 *   (through supremacy) nor the legislature (through sovereignty) should
 *   monopolize interpretation; instead, constitutional legitimacy derives
 *   from sustained popular engagement with constitutional meaning. The
 *   constraint is claimed as tangled_rope because it coordinates
 *   constitutional interpretation across multiple institutional sites
 *   (genuine coordination function) while extracting costs (gridlock,
 *   institutional uncertainty) and requiring active enforcement (suppression
 *   of institutional attempts to monopolize meaning). The reading is one of
 *   three interpretations of the contested kernel
 *   'basic_law_interpretive_authority'; it coexists with
 *   judicial_supremacy_reading and parliamentary_sovereignty_reading as
 *   competing live positions in constitutional discourse.
 *
 * KEY AGENTS:
 *   - popular_movements: organized groups mobilizing constitutional interpretation outside court/legislative channels — beneficiary, high resistance to institutional closure
 *   - civil_society_coalitions: advocacy organizations articulating competing constitutional readings — beneficiary, mobile exit
 *   - judiciary: courts constrained from claiming terminal authority, forced to justify decisions as embedded in ongoing public contestation — payer, institutional power
 *   - legislature: legislative majorities limited by perpetual constitutional contestation from below, unable to settle constitutional meaning through statute or amendment alone — payer, institutional power
 *   - academic_constitutional_scholars: gain platform when their interpretive work becomes legitimacy-grounding for institutional decisions — beneficiary, arbitrage exit
 *   - institutional_efficiency and jurisdictional_clarity: non-agent systemic goods sacrificed by distributing decision-making authority across multiple sites — victims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism: Distributed Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "political/constitutional").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, '239aa058-b358-4a59-ac89-10521fcda985').
narrative_ontology:cs_kernel_codification('239aa058-b358-4a59-ac89-10521fcda985', distributed).
narrative_ontology:cs_authority_grounding('239aa058-b358-4a59-ac89-10521fcda985', diffuse_epistemic).
narrative_ontology:cs_reading_relation('239aa058-b358-4a59-ac89-10521fcda985', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('239aa058-b358-4a59-ac89-10521fcda985', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('239aa058-b358-4a59-ac89-10521fcda985', foundational, popular_sovereign_interpretation).
narrative_ontology:cs_axiom_status(popular_sovereign_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('239aa058-b358-4a59-ac89-10521fcda985', popular_sovereign_interpretation, deontological).
narrative_ontology:cs_axiom('239aa058-b358-4a59-ac89-10521fcda985', foundational, perpetual_constitutional_contestation).
narrative_ontology:cs_axiom_status(perpetual_constitutional_contestation, holdable).
narrative_ontology:cs_axiom_grounding('239aa058-b358-4a59-ac89-10521fcda985', perpetual_constitutional_contestation, conventional).
narrative_ontology:cs_reference_frame('239aa058-b358-4a59-ac89-10521fcda985', popular_democratic_constitutional_sovereignty).
narrative_ontology:cs_drift_state('239aa058-b358-4a59-ac89-10521fcda985', contemporary_institutional_attempts_at_closure, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('239aa058-b358-4a59-ac89-10521fcda985', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, civil_society_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional_efficiency).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, jurisdictional_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, academic_constitutional_scholars).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, continuous_democratic_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can mobilize constitutional contestation outside courts and legislatures; their interpretations shape political salience and veto power. They benefit from a framework that legitimates popular voice as constitutional authority. They also bear the costs of perpetual contestation — no settlement, no rest, continuous mobilization required to prevent judicial or legislative takeover of meaning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_movements, payer).

% Organize around competing constitutional interpretations; their advocacy shifts the threshold for what counts as legitimate constitutional argument in public discourse. They benefit from a framework that validates constitutional meaning-making outside state institutions. They pay by engaging in perpetual contestation with no guaranteed settlement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, civil_society_coalitions, beneficiary,
    organized, generational, mobile, national).

% Must justify its interpretations as embedded in ongoing democratic deliberation rather than claiming terminal authority. This strips the legitimacy of finality. Courts benefit from the constraint's protection against legislative override of constitutional protections, but lose authority over meaning's closure. They operate under perpetual threat of popular constitutional override and legislative constitutional amendment.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, beneficiary).

% Holds constitutional amendment power but operates under the shadow of popular constitutional interpretation that can delegitimize its statutory choices before amendment even occurs. The constraint constrains legislative sovereignty by requiring that enacted law pass scrutiny not just through court review but through sustained popular constitutional contestation. Legislature benefits from the mechanism when courts overreach, but pays the cost when popular movements successfully reinterpret the constitution outside legislative channels.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, beneficiary).

% Gain interpretive authority and public platform when courts and legislators must justify their positions as embedded in scholarly constitutional discourse. They benefit from the constraint's framing that constitutional meaning is contestable and best understood through academic debate. They pay minimal cost and can exit through professionalization if the academy becomes too politicized.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, academic_constitutional_scholars, beneficiary,
    organized, generational, arbitrage, national).

% Institutional and scholarly actors who believe constitutional meaning should be settled, stable, and not perpetually subject to reinterpretation through popular mobilization. They argue for either judicial supremacy or parliamentary sovereignty to close the hermeneutical circle. They are excluded from this reading's legitimacy framework because their core claim — that interpretation must reach terminal closure — contradicts popular constitutionalism's foundational premise.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_minimalists, excluded,
    powerful, generational, trapped, national).

% Scholars and practitioners comparing constitutional systems; they observe how popular constitutionalism distributes interpretive authority differently than Westminster parliamentary sovereignty or US judicial supremacy models. They document variation and constitutive effects but do not participate in the contestation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, observers_comparative_constitutionalism, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels constitutional disputes into public democratic deliberation rather than concentrating them in a single institution (court or legislature). Prevents any single institutional interpreter from claiming finality and insulates constitutional meaning from rapid institutional capture by ensuring that shifts in constitutional interpretation require sustained popular support, not just a change in judicial composition or legislative majority.
% TRANSFER_FUNCTION: Redistributes interpretive authority from terminal institutional gatekeepers to distributed popular actors; moves the locus of constitutional legitimacy from institutional expertise and legal reasoning to popular sovereignty and democratic contestation. Externalizes the costs of perpetual reinterpretation (gridlock, institutional uncertainty, mobilization burden) onto all institutional sites and the polity as a whole.
% ABSENT_VOICES: Constitutional minimalists and efficiency-focused institutional designers would argue that perpetual contestation erodes the stability and predictability constitutional systems require. Judiciary and legislature, when speaking from an institutional self-interest perspective, would argue that popular constitutional interpretation undermines their ability to perform their distinctive functions. These voices are structurally absent from popular constitutionalism's legitimacy framework — the reading forecloses their core claim that interpretation must reach closure.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism disappeared and either judicial supremacy or parliamentary sovereignty solidified as the exclusive interpretive authority, the distribution of interpretive power would collapse into a single institution. Constitutional disputes would move into courts or legislatures exclusively; popular movements would lose their claim to constitute legitimate constitutional meaning outside those institutions; the constraint that distributes gridlock costs across multiple sites would be replaced by the constraint that concentrates authority in one. The institutional landscape and the political practice of constitutionalism would reorganize entirely.
% FOUNDING_PROBLEM: The founding problem is the risk of institutional capture of constitutional meaning: if courts monopolize interpretation, they can entrench themselves against legislative correction; if legislatures monopolize interpretation, they can use constitutional amendment to expand their power without principled constraint; if neither is kept accountable to popular understanding of the constitution, constitutional meaning becomes the property of institutional elites rather than the people. Popular constitutionalism was developed as a reading to protect democratic equality in constitutional interpretation itself.
% FOUNDING_PROBLEM_CORROBORATION: Popular constitutionalism scholars (Tushnet, Kramer, Siegel) attest that institutional capture remains a live danger and that sustained popular contestation is the remedy. Institutional actors (judges and legislators) often contest this diagnosis, arguing that their interpretive role requires stability and that popular contestation introduces dangerous populism. Separation-of-powers scholars outside the benefiting parties have documented historical cases of institutional overreach and confirmed the mechanism's diagnosis. No neutral external corroborator exists; the dispute is among scholars and institutions with different stakes.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint imposes real costs (gridlock, institutional drag, perpetual contestation) but also produces genuine coordination (prevents institutional monopoly, maintains popular constitutional input). The costs are unevenly distributed: popular movements benefit while institutions pay. Suppression is moderate (0.42) because the constraint does not require coercive force to maintain — it is sustained by scholarly legitimacy, popular mobilization, and the institutional fear of judicial/legislative overreach. Theater rises slightly over the interval (0.38 to 0.51) as institutional actors increasingly perform deference to popular constitutional interpretation while often resisting it substantively — the ratio approaches 0.5 because the performative element (courts citing popular movements, legislatures claiming popular mandate) grows without fully replacing the functional extraction. The measurements form one shared time grid: every metric is authored at every sampled time point (0, 5, 10, 15, 20, 25, 30, 40). The constraint reaches steady state around t=25; thereafter extraction and theater plateau, indicating the reading has achieved institutional equilibrium without further intensification or decay.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and legislature experience this constraint as extractive (they lose authority, face perpetual contestation, cannot settle meaning) while popular movements experience it as enabling (they gain interpretive voice, can contest institutional overreach). The engine computes per-seat directionality from the structural data: judicial and legislative seats should compute near the target end of directionality (high d, high χ), while popular-movement seats compute near the beneficiary end (low d, negative or low χ). The agenda-setter role is distributed across multiple sites rather than concentrated in one institution, which creates the perspectival asymmetry: no single institutional seat controls the agenda; the reading distributes agenda-setting power to those who can mobilize popular constitutional interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements are the beneficiary set (d near 0.0): they gain interpretive authority under this reading and have mobile exit options (can exit via demobilization if the reading is superseded). They pay the cost of perpetual contestation, which modulates their d upward to ~0.35 (symmetric). Judiciary and legislature are the payer set (d near 1.0): they lose authority over meaning's closure and have constrained exit (cannot simply revert to supremacy or sovereignty without overturning the reading's legitimacy claim). Academic scholars are secondary beneficiaries (d near 0.2): they gain platform without bearing major costs. The non-agent victims (efficiency, jurisdictional clarity) are excluded from directionality computation per schema (agent=false) but are named to document what the constraint extracts from the system's functional capacity. The constraint should trigger tangled_rope classification: it has beneficiaries (popular movements, civil society, scholars), victims (institutional efficiency, clarity), requires active enforcement (suppression of institutional closure attempts), and produces asymmetric extraction (institutions lose authority while movements gain it).
 *
 * MANDATROPHY ANALYSIS:
 *   Popular constitutionalism prevents false-summit (natural law) classification through its requirement for active enforcement and its beneficiary structure. The constraint is not a natural law or inevitable feature of constitutional systems — it is a contestable reading, maintained through scholarly legitimacy and popular mobilization, perpetually threatened by institutional attempts to monopolize meaning. The foundational axiom (popular_sovereign_interpretation) is holdable (live claim) but not yet overridden in constitutional practice, though it meets substantial resistance from institutional minimalists. The mandatrophy test applies: did the founding problem (institutional capture of constitutional meaning) produce the constraint, or has the constraint persisted beyond the problem's solution? The constraint remains live as long as institutional capture remains a credible threat — which it does, given the stability of extractiveness and theater over the measurement interval. The constraint should not be classified as piton because popular movements continue to defend it actively and institutions continue to resist it rather than simply performing compliance while ignoring it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_will_identification,
    'How is ''popular constitutional interpretation'' identified and measured? What distinguishes authentic popular understanding from elite capture of popular rhetoric?',
    'Comparative analysis of constitutional movements: track how courts/legislatures justify their deference to ''popular will'' and what fraction of named ''popular'' interpretation actually traces to grassroots versus elite-led movements. Examine cases where multiple contradictory ''popular'' interpretations exist simultaneously.',
    'If popular will is identifiable and non-elite, the constraint enables genuine democratic constraint on institutions. If ''popular interpretation'' is systematically elite-curated or rhetorical cover for judicial/legislative decisions, the constraint becomes a facade and should reclassify as snare (judicial/legislative extraction dressed in populist language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_will_identification, empirical, 'Whether popular constitutional interpretation can be distinguished from elite rhetorical capture.').

omega_variable(
    gridlock_vs_accountability_tradeoff,
    'Does the constraint''s gridlock function primarily as a check on institutional overreach, or does it primarily paralyze institutional effectiveness at the cost of accountability?',
    'Case studies of constitutional stalemates: analyze whether gridlock prevented institutional capture or whether it prevented timely response to pressing needs. Compare outcomes in jurisdictions with higher and lower popular constitutionalism enforcement.',
    'If gridlock primarily prevents capture, the constraint is well-classified as tangled_rope (coordination + extraction in service of accountability). If gridlock primarily prevents timely institutional response, the constraint may be reclassified as snare (extraction from institutional efficiency in service of preventing capture that may not be the live danger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_vs_accountability_tradeoff, empirical, 'Whether the constraint''s gridlock function is instrumentally justified.').

omega_variable(
    institutional_inertia_vs_principled_enforcement,
    'Is the constraint maintained by principled commitment to popular sovereignty, or is it maintained by institutional actors'' incentive to cite ''popular will'' as cover for their own policy preferences?',
    'Track institutional invocations of ''popular constitutional interpretation'' when it aligns with their preferences versus when it contradicts them. Measure consistency of institutional deference across ideological divides.',
    'If institutional actors cite popular constitutionalism selectively (deferring when it suits them, ignoring when it doesn''t), the constraint is theater-heavy and borders on snare (extraction of institutional authority dressed in popular language). If deference is consistent across ideological lines, the constraint operates as intended.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_inertia_vs_principled_enforcement, empirical, 'Whether institutional actors genuinely enforce the constraint or selectively invoke it.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do judicial_supremacy_reading and parliamentary_sovereignty_reading remain live institutional positions that popular_constitutionalism_reading must continually defeat, or have they been progressively delegitimized such that this reading faces no real institutional alternative?',
    'Track institutional authority claims over time: do courts or legislatures still explicitly claim terminal interpretive authority, or do they preface such claims with acknowledgment of popular constitutional contestation? Monitor scholarly and judicial discourse for explicit advocacy of the sibling readings.',
    'If the sibling readings remain live and institutional actors periodically attempt to reclaim supremacy/sovereignty, the reading is under active contestation and this constraint classification is accurate. If the sibling readings have been progressively delegitimized and no institutional actor explicitly claims them anymore, the constraint may have succeeded in its delegitimation function and should be reclassified as a rope (coordination achieved, extraction resolved).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, empirical, 'Whether popular constitutionalism faces real institutional alternatives or operates in a delegitimized landscape.').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is ONE instantiation of a contested kernel with THREE possible readings. What analysis reveals which reading is structurally true versus which are cover stories for institutional capture?',
    'Comparative case analysis: examine constitutional systems with strong judicial supremacy, strong parliamentary sovereignty, and strong popular constitutionalism; compare outcomes on institutional capture, stability, responsiveness, and protection of minorities. Assess whether the constraint''s structure matches its claimed function in each jurisdiction.',
    'If popular constitutionalism empirically prevents institutional capture better than the sibling readings, the reading is warranted and the classification holds. If empirical comparison shows institutional capture under any of the readings equally, or if popular constitutionalism enables different forms of capture (populist majoritarian capture of constitutional meaning), the committer analysis should revise which reading best captures the kernel''s true structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Which reading of the basic_law_interpretive_authority kernel most accurately describes the constraint''s true institutional role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement_basis(basi_tr_t30, observed).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement_basis(basi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(basi_be_t30, observed).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(basi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(basi_su_t30, observed).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(basi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.18).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'basic_law_interpretive_authority.' Three structurally distinct constraints decompose this kernel based on which institutional seat is claimed to hold terminal interpretive authority. The reading_relations (coexists_with for both siblings) and axioms (popular_sovereign_interpretation as the foundational distinguishing claim) document the committer structure. The epsilon values differ across readings: judicial_supremacy assumes high clarity and low contestation (low extraction), parliamentary_sovereignty assumes legislative finality (moderate extraction), and this reading (popular_constitutionalism) assumes perpetual contestation (moderate extraction from institutional gridlock, coordination function from preventing monopoly). All three are live positions in constitutional scholarship and practice; they are not sequential readings of a resolved kernel but rather competing claims about where constitutional authority is legitimately lodged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
