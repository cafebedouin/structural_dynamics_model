% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Article 4 NDC Sovereigntist Reading: Voluntary National Climate Pledges
 *   domain: international_governance/climate_policy/treaty_law
 *
 * SUMMARY:
 *   Under the sovereigntist reading of Paris Article 4, Nationally Determined
 *   Contributions (NDCs) are framed as voluntary, self-determined climate
 *   pledges that preserve national energy sovereignty and the right of states
 *   to determine their own development pathways. This reading emphasizes
 *   state authority, CBDR protection for developing nations, and the
 *   principle of permanent sovereignty over natural resources. It contrasts
 *   sharply with the supranational reading (which treats NDCs as binding
 *   commitments on a mandated ratcheting trajectory toward net-zero) and the
 *   equity reading (which interprets CBDR as requiring structural
 *   distinctions in target-setting based on historical responsibility). The
 *   sovereigntist reading is instantiated here as a single constraint with
 *   its own ε-invariant structure, independent of the sibling readings.
 *
 * KEY AGENTS:
 *   - fossil_dependent_economies — moderate power, mobile exit; retain energy policy autonomy under this reading
 *   - emerging_industrializers — moderate power, mobile exit; protected from acceleration pressure
 *   - developed_high_emitter_economies — powerful but facing reputational pressure to pledge aggressively despite voluntary nature
 *   - climate_vulnerable_small_states — powerless, trapped exit; must pledge without enforcement guarantee on others
 *   - state_sovereignty_doctrine — vindicated non-agent beneficiary; the constraint's core legitimacy claim
 *   - international_climate_secretariat — agenda-setter with constrained authority (facilitates, cannot enforce)
 *   - non_state_advocates — excluded from formal obligation structure; voice remains advisory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.32).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.18).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Article 4 NDC Sovereigntist Reading: Voluntary National Climate Pledges").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_governance/climate_policy/treaty_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '05bd1e5e-8028-4cb4-ad08-cdffd42b915b').
narrative_ontology:cs_kernel_codification('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', fixed_text).
narrative_ontology:cs_authority_grounding('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', lineage).
narrative_ontology:cs_interpretation_layer_present('05bd1e5e-8028-4cb4-ad08-cdffd42b915b').
narrative_ontology:cs_reading_relation('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', foundational, permanent_sovereignty_over_natural_resources).
narrative_ontology:cs_axiom_status(permanent_sovereignty_over_natural_resources, holdable).
narrative_ontology:cs_axiom_grounding('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', permanent_sovereignty_over_natural_resources, deontological).
narrative_ontology:cs_axiom('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', foundational, state_voluntary_pledge_autonomy).
narrative_ontology:cs_axiom_status(state_voluntary_pledge_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', state_voluntary_pledge_autonomy, conventional).
narrative_ontology:cs_reference_frame('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', sovereign_state_energy_self_determination).
narrative_ontology:cs_drift_state('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', contemporary_supranational_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('05bd1e5e-8028-4cb4-ad08-cdffd42b915b', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, emerging_industrializers).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, state_sovereignty_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_small_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, developed_economies_high_emitters).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_small_states).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, national_energy_self_determination).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, permanent_sovereignty_over_natural_resources).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, differentiated_responsibility_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States whose economies rely on coal, oil, or gas production and export. Under the sovereigntist reading, they retain the authority to set their own NDC targets at whatever level preserves their development pathways and export revenues. They can revise pledges upward or downward without external penalty. The voluntary framing permits them to avoid acceleration pressures and maintain fossil infrastructure as a sovereign choice.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies, beneficiary,
    moderate, generational, mobile, global).

% Middle-income and lower-income states industrializing via coal and hydrocarbon-powered development. The sovereigntist reading protects their right to choose energy pathways unconstrained by binding international targets. They can pledge modest reductions and claim Common But Differentiated Responsibilities (CBDR) protection without committing to supranational accountability or ratcheting timelines.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, emerging_industrializers, beneficiary,
    moderate, generational, mobile, global).

% Wealthy, industrialized states with high per-capita emissions and established economies. They face pressure to commit to aggressive NDC targets to demonstrate climate leadership. The sovereigntist reading allows them to pledge ambitiously, but also permits developing states to pledge less aggressively without sanction, creating asymmetry in effective burden. Their arbitrage exit consists of delaying implementation, revising pledges downward, purchasing carbon credits, or relying on disputed offset mechanisms.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, developed_economies_high_emitters, payer,
    powerful, generational, arbitrage, global).

% Island nations and low-lying coastal states facing existential threats from warming. Under the sovereigntist reading, they can pledge high targets (which they often do, for moral visibility), but they have no enforcement mechanism to compel higher pledges from major emitters. Their sovereignty is respected; their survival is not guaranteed. They bear the costs of climate change unmitigated while maintaining identity as climate plaintiffs within the Paris system. Their exit is identity-locked: withdrawal from Paris would mean losing their only institutional voice, however weak.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_small_states, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_small_states, beneficiary).

% The UNFCCC and subsidiary bodies administer the NDC review process. Under the sovereigntist reading, they lack authority to reject pledges, impose penalties, or mandate ratcheting. They facilitate transparency, track progress toward self-set targets, and maintain procedural legitimacy, but enforcement is purely reputational and voluntary. Their constrained exit consists of the threat to resign, which would undermine the entire Paris system but is not credible because the secretariat is constituted to maintain it.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, international_climate_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% NGOs, scientific bodies, indigenous communities, and youth movements demanding binding targets and enforcement. The sovereigntist reading structurally excludes them from the formal obligation structure. They can advocate, testify, and pressure states, but the framework guarantees their voice remains advisory, not structural. States can ignore their demands as a matter of sovereign choice. Their constrained exit consists of continued advocacy pressure or withdrawal from participation, both of which leave the constraint unchanged.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, non_state_climate_advocates, excluded,
    organized, biographical, constrained, global).

% Oil, gas, coal, and cement sectors. The sovereigntist reading permits states to accommodate their interests by setting modest pledges, delaying transition timelines, or grandfathering existing infrastructure. No supranational body can force accelerated phase-outs. They benefit from the constraint's low enforcement gradient and the state's retained authority to weight their interests against climate goals. Their exit consists of relocation to jurisdictions with lower climate ambition or greater policy flexibility.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, carbon_intensive_industries, beneficiary,
    powerful, biographical, mobile, global).

% Climate scientists, economists, policy analysts, and international legal scholars analyzing NDC architecture. They observe the constraint's structural properties: voluntary nature, lack of enforcement, divergence between pledges and emissions pathways, and the persistence of unilateral exit options for all parties. They remain outside the obligation structure and carry no stakes except professional interest.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, analytical_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a procedural framework for states to declare and track their self-determined climate commitments, creating transparency about national energy policies and enabling peer review and technical exchange without imposing binding targets or supranational governance.
% TRANSFER_FUNCTION: Transfers legitimacy and narrative authority from binding international environmental governance to national climate policy autonomy. High-emission states gain the framing of climate action (through pledges) without surrendering control over energy policy. Developing states preserve the right to prioritize development and energy access. The constraint moves discretionary power back toward the nation-state from international institutions.
% ABSENT_VOICES: Climate-vulnerable populations in fossil-dependent economies (coal miners facing potential job loss but not represented in the pledge-setting process); future generations bearing the costs of delayed transition; ecosystems and non-human life affected by climate change but holding no voice in NDC deliberation. These absences are structural to the sovereigntist reading: if they were seated as stakeholders with veto power, the reading would collapse into supranational governance.
% DISAPPEARANCE_RATIONALE: If the NDC framework and the sovereigntist reading vanished, states would lose the procedural legitimacy conferred by Paris Article 4. Climate negotiations would either revert to non-binding aspirational statements (less structured than NDCs) or shift toward binding enforcement mechanisms administered by supranational bodies (the supranational reading). The vacuum would be filled by one of the alternative governance structures, not by spontaneous state-led decarbonization.
% FOUNDING_PROBLEM: The challenge of designing a climate treaty that respects state sovereignty while creating accountability structures — balancing the imperative of global emissions reductions against the principle that states retain authority over their own natural resources and development pathways. The sovereigntist reading resolves this by prioritizing sovereignty: states set their own targets and remain free to revise or abandon them.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem and its sovereigntist resolution are attested by state representatives (particularly from fossil-dependent and emerging economies) and by scholars of international law emphasizing permanent sovereignty over natural resources. The alternative reading (that the founding problem requires binding supranational enforcement) is attested by climate scientists, vulnerable-state representatives, and scholars of international environmental governance. The two readings cite overlapping founding facts but resolve them toward opposite poles — this is the kernel contest.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.32 at interval end) because the constraint imposes no binding obligation, no penalty for non-compliance or revision, and no surcharge or cost transfer from high-emitters to vulnerable states. Fossil-dependent economies and industrial developers extract benefit (they retain energy autonomy) without bearing costs. The constraint coordinates transparency and procedural legitimacy but does not extract resources or restrict exit. Suppression is correspondingly LOW (0.18) because the constraint has minimal enforcement machinery — there is no external body that coerces state compliance, no penalty system, no surveillance apparatus with teeth. Theater is MODERATE (0.41) because much of what the constraint does is performative: states make pledges for narrative credibility while the framework guarantees no accountability. The measurement series remain essentially flat because the sovereigntist reading does not posit a ratcheting dynamic; the constraint's structure is stable and its low extractiveness does not accumulate over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (climate-vulnerable states) and the beneficiary seats (fossil-dependent economies, sovereignty doctrine) should compute radically different types from this same constraint. A vulnerable state sees the NDC framework as producing coordination of transparency without binding reciprocal obligation — a rope that coordinates their own visibility while leaving high-emitters unconstrained. A fossil-dependent economy sees it as pure coordination: they gain the legitimacy of climate participation (entry fee to global climate narrative) while retaining full energy autonomy (exit option). The developed high-emitter seats sit in between: they pay reputational pressure to pledge aggressively but retain the sovereign right to delay implementation or revise downward. The engine computes this divergence from the power/exit/beneficiary structure. The authored claim (rope) reflects the sovereigntist reading's framing; the metrics reflect what the constraint actually does under that reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil-dependent and emerging economies are net beneficiaries (d toward 0.0): they gain sovereignty preservation and energy autonomy without constraint. The state sovereignty doctrine is a vindicated proposition (not an agent, not a seat) that lends legitimacy to the whole reading. Developed high-emitters face mild extraction (reputational pressure to pledge aggressively, d near 0.4–0.5) but retain arbitrage exit (they can delay, revise, greenwash). Climate-vulnerable small states face the highest effective extraction (d near 0.8): they are trapped into the system (must participate for visibility and moral voice), they carry the costs of climate change unmitigated, and the framework provides no enforcement lever against high-emitters. Their exit is identity-locked (international identity as climate plaintiff, dependent on the system's legitimacy) or trapped (small economies with no alternative climate venue). The UNFCCC secretariat is the agenda-setter (constrained, institutional, powerful enough to administer but not to enforce) at d near 0.5 (neither collecting nor bearing the primary costs; administering a structure others shaped).
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading does not fall into mandatrophy by its own terms. The founding problem — how to design a climate treaty respecting sovereignty — remains live under this reading. The mandate (establish voluntary pledges and transparency) is still being executed (NDCs are filed, reviewed, reported). However, the alternative readings (supranational, equity) argue that the founding problem HAS morphed: the original intent was to reduce global emissions at a specified rate, which the sovereigntist reading now fails to deliver. From that perspective, the sovereigntist reading exhibits mandatrophy (the original emission-reduction mandate is lost, replaced by sovereignty-protection procedures). The binary is contested precisely because the kernel itself is contested. An external analysis would flag this as a LIVE CONTENTION constraint: the reading's mandate is coherent within its own frame but incompatible with the mandates of sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_versus_binding_boundary,
    'Is Article 4 of the Paris Agreement best interpreted as guaranteeing state sovereignty over target-setting (sovereigntist reading) or as requiring binding commitments on a ratcheting path (supranational reading)?',
    'Legal analysis of negotiating history, subsequent state practice and opinio juris (customary international law formation), and jurisprudence from any international climate court if established; differential evolution of state behavior if enforcement mechanisms are experimentally introduced in subset of parties.',
    'If sovereigntist interpretation prevails, the constraint remains a low-epsilon voluntary framework; if supranational interpretation gains institutional dominance, reclassification to tangled_rope or snare becomes likely as enforcement mechanisms accumulate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereign_versus_binding_boundary, conceptual, 'The core contested interpretation of the Paris kernel: sovereignty-preserving versus binding-commitment framings.').

omega_variable(
    cbdr_scope_ambiguity,
    'Does Common But Differentiated Responsibilities justify structural target differences (lower targets for developing states, higher for developed), or does it apply only to the transition timeline and financial support?',
    'Examination of subsequent conference decisions and state negotiating positions; empirical measurement of whether NDC targets actually reflect differentiated responsibility or converge toward uniform stringency regardless of development status.',
    'If CBDR justifies structural target differences, the sovereigntist reading is strengthened (developing states retain autonomy to pledge less aggressively); if CBDR is reinterpreted to apply only to support and timelines, the supranational and equity readings gain ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_scope_ambiguity, conceptual, 'Whether differentiated responsibility translates to differentiated target obligations.').

omega_variable(
    enforcement_atrophy_mechanism,
    'Why do global enforcement mechanisms under the NDC system remain atrophied, and is the atrophy structural (states collectively prefer it) or contingent (enforcement infrastructure has not yet been built)?',
    'Analysis of COP decisions, state proposals for enhanced transparency and accountability, and coalitional dynamics in NDC review processes; empirical test: if enforcement proposals arise and states accept them, atrophy was contingent; if enforcement proposals are consistently blocked or water-downed, atrophy is structural.',
    'Structural atrophy supports the sovereigntist claim that states prefer and defend voluntary frameworks; contingent atrophy suggests the reading is one phase of an evolving system that will eventually enforce (supporting supranational reclassification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_atrophy_mechanism, empirical, 'Whether weak enforcement reflects state preference or institutional incompleteness.').

omega_variable(
    fossil_fuel_trajectory_coupling,
    'To what extent does the low extractiveness and high theater of the sovereigntist reading enable fossil-fuel-dependent states to maintain coal, oil, and gas infrastructure under the appearance of climate participation?',
    'Empirical tracking of state NDC pledges versus actual fossil fuel production and infrastructure investment decisions; comparison of pledges to baseline emissions scenarios; correlation of sovereigntist rhetoric adoption with fossil fuel industry lobbying patterns.',
    'High coupling would establish the sovereigntist reading as instrumentally valuable to fossil-fuel interests and to the states they influence, supporting a snare-class alternative analysis (the ''voluntary sovereignty'' framing is cover for extractive lock-in). Lower coupling would support the rope framing (genuine coordination with divergent interests but no concentrated extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_fuel_trajectory_coupling, empirical, 'Whether the sovereigntist reading materially enables fossil fuel persistence.').

omega_variable(
    vulnerable_state_exit_mechanism,
    'What are the actual exit options for climate-vulnerable small states trapped in the NDC system? Are they truly trapped, or do they retain mobile or arbitrage exits not captured by the authored exit_options?',
    'Interview-based research with small island and low-lying-coast state representatives; analysis of coalition-building behavior in UNFCCC processes; empirical test of whether threatened states can credibly exit (withdraw from Paris, form counter-coalition, pursue alternative climate venues).',
    'If truly trapped (identity-locked into Paris system, no exit to alternative legitimacy), their effective extraction is higher than measured and the constraint edges toward snare classification from their seat; if arbitrage exits exist (alternative climate coalitions, bilateral climate finance arrangements), their directionality is less extreme and the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_state_exit_mechanism, empirical, 'The actual exit availability for powerless climate-vulnerable states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(pari_tr_t0, observed).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement_basis(pari_tr_t5, observed).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(pari_tr_t10, observed).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(pari_tr_t15, observed).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(pari_tr_t20, observed).
narrative_ontology:measurement(pari_tr_t30, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(pari_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(pari_be_t0, observed).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement_basis(pari_be_t5, observed).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(pari_be_t10, observed).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement_basis(pari_be_t15, observed).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(pari_be_t20, observed).
narrative_ontology:measurement(pari_be_t30, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(pari_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement_basis(pari_su_t0, observed).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 5, 0.16).
narrative_ontology:measurement_basis(pari_su_t5, observed).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement_basis(pari_su_t10, observed).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 15, 0.17).
narrative_ontology:measurement_basis(pari_su_t15, observed).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(pari_su_t20, observed).
narrative_ontology:measurement(pari_su_t30, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(pari_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.1).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, fossil_fuel_production_sovereignty).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, international_climate_accountability).

% DUAL FORMULATION NOTE:
% This constraint is part of the paris_article_4_ndc constraint family (three competing readings of the same kernel). The sovereigntist_reading instantiates the voluntary-sovereignty pole; supranational_reading instantiates the binding-enforcement pole; equity_reading instantiates the structural-differentiation pole. Each reading has a distinct epsilon, beneficiary structure, and classification, reflecting the kernel's irreducible interpretive contest. All three are mutually linked via network.affects_constraints. The readings do not reduce to measurement disagreement — they are structural disagreements about what Article 4 requires and permits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__sovereigntist_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
