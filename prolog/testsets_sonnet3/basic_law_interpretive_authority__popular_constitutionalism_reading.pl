% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Constitutional Meaning as Perpetual Democratic Contestation (Popular Constitutionalism Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates one reading of the
 *   basic_law_interpretive_authority kernel: constitutional meaning is not
 *   settled terminally by any institution but emerges from ongoing
 *   contestation among courts, legislatures, social movements, and electoral
 *   coalitions. This is structurally distinct from the
 *   judicial_supremacy_reading (courts hold terminal authority) and the
 *   parliamentary_sovereignty_reading (the elected legislature holds terminal
 *   authority) — those are separate constraint stories with their own ε
 *   values, beneficiaries, and victims, linked via
 *   network.affects_constraints. Under this reading, the coordination
 *   function (avoiding concentration of fallible terminal authority) is real,
 *   but it distributes real costs onto parties needing settled doctrine and
 *   onto claimants who lack sustained mobilization capacity to keep winning a
 *   contest that never closes.
 *
 * KEY AGENTS:
 *   - social_movements: primary beneficiary of perpetual openness (organized/mobile)
 *   - electoral_majorities: beneficiary via electoral leverage over time (organized/mobile)
 *   - regulatory_certainty_seekers: bear the cost of doctrinal instability (powerful/constrained)
 *   - minority_rights_claimants: bear the cost of revisable protections without matching mobilization capacity (moderate/trapped)
 *   - courts and legislature: demoted from terminal authority to participant-voices under this reading
 *   - constitutional_theorists: analytical observers comparing the reading against historical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.31).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Constitutional Meaning as Perpetual Democratic Contestation (Popular Constitutionalism Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, '11104529-2d5d-4cf6-a933-9150444506a5').
narrative_ontology:cs_kernel_codification('11104529-2d5d-4cf6-a933-9150444506a5', distributed).
narrative_ontology:cs_authority_grounding('11104529-2d5d-4cf6-a933-9150444506a5', distributed).
narrative_ontology:cs_reading_relation('11104529-2d5d-4cf6-a933-9150444506a5', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('11104529-2d5d-4cf6-a933-9150444506a5', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('11104529-2d5d-4cf6-a933-9150444506a5', foundational, no_institution_holds_terminal_interpretive_authority).
narrative_ontology:cs_axiom_status(no_institution_holds_terminal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('11104529-2d5d-4cf6-a933-9150444506a5', no_institution_holds_terminal_interpretive_authority, conventional).
narrative_ontology:cs_axiom('11104529-2d5d-4cf6-a933-9150444506a5', foundational, constitutional_meaning_tracks_sustained_popular_mobilization).
narrative_ontology:cs_axiom_status(constitutional_meaning_tracks_sustained_popular_mobilization, holdable).
narrative_ontology:cs_axiom_grounding('11104529-2d5d-4cf6-a933-9150444506a5', constitutional_meaning_tracks_sustained_popular_mobilization, empirically_contingent).
narrative_ontology:cs_reference_frame('11104529-2d5d-4cf6-a933-9150444506a5', contested_multi_site_interpretive_practice).
narrative_ontology:cs_drift_state('11104529-2d5d-4cf6-a933-9150444506a5', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11104529-2d5d-4cf6-a933-9150444506a5', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, electoral_majorities).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, civic_organizations).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, regulatory_certainty_seekers).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, minority_rights_claimants).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_meaning_is_practice_dependent).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, no_institution_holds_terminal_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilize outside courts and legislatures to press constitutional claims through protest, litigation-adjacent advocacy, and electoral pressure. The absence of a terminal adjudicator means their claims remain live indefinitely rather than being foreclosed by a single ruling; they can keep contesting settled doctrine through changed political conditions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movements, beneficiary,
    organized, generational, mobile, national).

% Shift constitutional understanding through elections, legislative action, and appointments over time, rather than being bound permanently by a single court's reading. Their leverage is real but requires sustained electoral coordination across multiple cycles to durably move meaning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, electoral_majorities, beneficiary,
    organized, generational, mobile, national).

% Advocacy groups, bar associations, and civic institutions participate in an ongoing interpretive contest across courts, legislatures, and public discourse, helping set which questions stay live and which are treated as settled. They benefit from the absence of a single terminal forum that could close down their access points.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, civic_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, civic_organizations, agenda_setter).

% Businesses, administrators, and lower courts that need stable, predictable constitutional rules to plan investment, draft regulation, or resolve disputes bear the cost of perpetual contestability: doctrine that could shift again through the next electoral or mobilization cycle undermines settled reliance, and they have no forum that can hand down a truly final answer.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, regulatory_certainty_seekers, payer,
    powerful, biographical, constrained, national).

% Groups whose constitutional protections depend on a favorable reading prevailing bear the risk that gains achieved through litigation or legislation can be reopened by a subsequent electoral majority or mobilized counter-movement, since no institutional victory is treated as terminal. Unlike organized social movements with sustained coalition capacity, individually rights-dependent claimants often lack the numbers to keep winning the perpetual contest.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, minority_rights_claimants, payer,
    moderate, generational, trapped, national).

% Under this reading, courts remain one voice among several rather than the final word; their rulings are treated as provisional inputs into an ongoing contest rather than binding resolutions. They participate in setting doctrine but cannot close the question, which is a demotion from the authority they claim under the sibling judicial-supremacy reading.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, courts, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, courts, agenda_setter).

% Elected representatives shape constitutional meaning through statute and constitutional amendment processes, but under this reading their enactments too remain contestable by subsequent movements and majorities rather than final — a demotion from the terminal authority claimed under the parliamentary-sovereignty reading.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, agenda_setter).

% Scholars who document and adjudicate among the competing readings of interpretive authority, tracing how meaning has actually moved historically across courts, legislatures, and popular mobilization to assess which reading better describes observed practice.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes the burden of constitutional interpretation across multiple institutional sites and civic actors over time, avoiding the concentration of terminal authority in any single fallible institution and allowing constitutional meaning to track evolving social consensus without requiring formal amendment for every shift.
% TRANSFER_FUNCTION: Moves the cost of interpretive finality away from any single institution (which would bear the legitimacy risk of terminal rulings) and onto claimants and regulated parties, who must operate under permanently revisable doctrine; moves interpretive leverage toward organized, sustained-mobilization-capable actors and away from individually-situated claimants who cannot keep contesting indefinitely.
% ABSENT_VOICES: Individually-situated minority rights claimants without durable coalition infrastructure are structurally disadvantaged relative to well-organized social movements and electoral coalitions, since perpetual contestability rewards sustained collective capacity over one-time legal victories; they would object that 'meaning is always contestable' functions differently for a group that can out-mobilize opponents across decades versus one that cannot.
% DISAPPEARANCE_RATIONALE: If this reading's premise vanished and a terminal adjudicator were installed (judicial or legislative), the political process would reorganize around capturing that single forum rather than sustaining distributed, ongoing contestation; social movements would shift strategy from long-horizon mobilization toward one-time institutional capture, and settled doctrine would acquire a finality it currently lacks.
% FOUNDING_PROBLEM: The problem this reading addresses is the risk of concentrating terminal interpretive authority in any single institution — whether courts (subject to counter-majoritarian difficulty and elite capture) or legislatures (subject to majoritarian tyranny and electoral volatility) — and the observed historical reality that constitutional meaning has, in practice, shifted through social movements, changed electoral coalitions, and evolving public understanding rather than remaining fixed by any one ruling or statute.
% FOUNDING_PROBLEM_CORROBORATION: Historians of constitutional development and comparative political scientists outside any interested institution attest that meaning has empirically moved through popular mobilization independent of formal doctrinal channels (e.g., civil rights era doctrinal shifts preceding and following, not solely produced by, judicial rulings). Courts and legislatures themselves largely reject this reading's premise, each asserting its own terminal authority; the corroboration for the founding problem's continued liveness comes primarily from constitutional historians and social movement scholars, not from either institutional claimant.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rises modestly over the interval: this reading imposes real costs on parties needing settled doctrine, but those costs are diffused across many institutional sites rather than concentrated through a single enforced extraction channel, consistent with the story's own premise that no institution holds terminal power to enforce a stable rent. Suppression is comparatively low (0.31) because the reading's defining feature is precisely the ABSENCE of a mechanism that could suppress ongoing contestation — no single actor can foreclose the debate, which is the coordination benefit but also means less coercive apparatus exists to measure as suppression. Theater ratio is modest and slowly rising (0.28 at T=40), reflecting some performative invocation of 'ongoing democratic dialogue' by institutions that in practice still try to entrench their own readings. Accessibility collapse is moderate-low (0.35): alternatives (terminal judicial or legislative authority) remain live, contested political projects, not fully closed off. Resistance is comparatively high (0.58) because every institutional actor with a stake in a terminal-authority reading (courts, legislatures) actively contests this reading's premise.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an organized social movement, this arrangement is a rope: genuine coordination avoiding concentrated fallible authority, with real participatory benefit. From the seat of a minority rights claimant without comparable mobilization infrastructure, the same absence of terminal adjudication looks closer to a tangled rope or worse: hard-won protections remain permanently reopenable by better-organized opposition. The engine computes these divergent seat classifications from the declared structural data (power, exit_options, time_horizon) rather than from any single narrative frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are actors with sustained mobilization capacity — social movements, electoral coalitions, civic organizations — who benefit from meaning staying open because they can keep contesting it across long time horizons (d near beneficiary end, generational time_horizon, mobile exit). Payers are actors needing settled doctrine (regulatory certainty seekers: powerful but constrained by the absence of finality) and minority rights claimants whose gains are perpetually revisable and who, unlike organized movements, often lack the coalition durability to keep re-winning the contest (trapped exit, moderate power). Courts and legislature are structurally demoted under this reading — excluded from terminal authority despite institutional power — which is why they are marked excluded/agenda_setter dual role: they still shape the contest but cannot close it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's classification prevents mislabeling perpetual contestability as either pure extraction (a snare imposed by whichever institution currently wins the contest) or pure coordination (a rope with no real losers). The founding problem — avoiding concentrated fallible terminal authority — remains genuinely contested as live or resolved depending on who is asked; corroboration from constitutional historians outside any institutional claimant supports treating the founding problem as still-live rather than dead-but-persisting (which would indicate mandatrophy). The distributed-cost structure (regulatory certainty seekers and minority claimants both pay, but through different mechanisms) is what keeps this from being a clean rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_describes_actual_practice,
    'Of the three kernel readings (judicial supremacy, parliamentary sovereignty, popular constitutionalism), which one actually describes how constitutional meaning has moved historically in this jurisdiction — or does the answer vary by era and issue area?',
    'Comparative constitutional-historical analysis tracing specific doctrinal shifts (e.g., civil rights, economic regulation, criminal procedure) to identify whether courts, legislatures, or extra-institutional mobilization was the proximate driver of change, across multiple episodes and periods.',
    'If judicial rulings are consistently the proximate and durable driver, the judicial_supremacy_reading better describes practice and this reading''s beneficiary structure (social movements/electoral majorities as primary winners) would be overstated. If legislative action is consistently decisive, the parliamentary_sovereignty_reading fits better. Only if the diffusion is genuine and consistent does this reading hold as the accurate structural description rather than as normative aspiration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_describes_actual_practice, conceptual, 'Which kernel reading best fits observed constitutional-historical practice.').

omega_variable(
    gridlock_cost_distribution_fairness,
    'Is the distribution of gridlock/uncertainty costs across institutional sites under this reading actually more equitable than concentrating costs in a single terminal authority, or does it simply shift costs onto the least-organized claimants who cannot sustain perpetual contestation?',
    'Empirical study comparing outcomes for minority rights claimants under jurisdictions with strong judicial finality versus jurisdictions with weaker/more contestable constitutional settlements, controlling for underlying social conditions.',
    'If minority claimants fare systematically worse under perpetual contestability (because they cannot out-mobilize opposing coalitions over decades), this reading''s coordination benefit is substantially offset by a redistribution of vulnerability onto exactly the parties constitutional protection is meant to shield — which would push the classification toward tangled_rope rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gridlock_cost_distribution_fairness, empirical, 'Whether distributed contestability costs fall disproportionately on under-organized rights claimants.').

omega_variable(
    committer_structure_kernel_disagreement_location,
    'This story is one reading of the basic_law_interpretive_authority kernel. The disagreement with the judicial_supremacy_reading and parliamentary_sovereignty_reading siblings is located specifically in whether interpretive authority terminates anywhere at all — this reading denies that any institution''s ruling or enactment is ever final, while both siblings locate finality in a specific institution (courts or legislature respectively). A sibling reading would change: the beneficiary set (from organized/mobile movements to whichever institution holds finality), the victim set (from under-organized claimants to whoever loses under the terminal institution''s ruling), and the classification logic entirely (a terminal-authority reading could plausibly compute as tangled_rope or snare if the terminal institution is captured, whereas this reading''s distributed structure resists that concentration).',
    'This is a conceptual/framing question, not empirically resolvable by further data alone — it depends on which normative theory of constitutional legitimacy (elite expertise, democratic mandate, or ongoing popular sovereignty) is adopted as the baseline for evaluating the arrangement.',
    'Adopting a different reading changes which agents are structurally positioned as beneficiaries versus victims and changes the classification the engine computes, without changing the underlying institutional facts — this is precisely the kind of framing under-determination the ε-invariance principle requires decomposing into separate stories rather than averaging within one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_disagreement_location, conceptual, 'Location of the kernel disagreement: whether interpretive authority terminates anywhere, and in which institution if so.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(basic_law_interpretive_authority__popular_constitutionalism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'who holds final constitutional interpretive authority' per the ε-invariance principle. judicial_supremacy_reading locates terminal authority in courts; parliamentary_sovereignty_reading locates it in the elected legislature; this story (popular_constitutionalism_reading) denies terminal authority exists anywhere, treating meaning as perpetually contested across institutional sites. Each sibling has a distinct ε, beneficiary/victim structure, and classification logic, reflecting genuinely different structural claims about where (or whether) interpretive finality resides — not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
