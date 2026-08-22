% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/institutional_design/political_theory
 *
 * SUMMARY:
 *   The judicial supremacy reading of basic law interpretive authority holds
 *   that courts possess final, binding interpretive power over constitutional
 *   meaning through institutional independence and specialized legal
 *   expertise. This reading is one of three structurally distinct claims
 *   about constitutional authority (the kernel). The competing
 *   readings—parliamentary sovereignty and popular constitutionalism—occupy
 *   different institutional positions and deploy different legitimacy
 *   narratives. Under judicial supremacy, the judiciary benefits from
 *   centralized authority and professional monopoly over constitutional
 *   language; legislatures and electoral majorities bear the cost of gridlock
 *   when courts block legislation. The reading claims the constraint is a
 *   tangled rope (genuine coordination function: stable constitutional
 *   meaning; asymmetric extraction: judicial power concentration). The
 *   measured metrics show extractiveness rising through the interval to 0.68
 *   as judicial reach expands, suppression peaking at 0.72 as courts enforce
 *   their interpretive monopoly, and theater ratio rising moderately as
 *   legitimating rhetoric (fidelity to law, neutral expertise) replaces
 *   functional necessity. The claim/metric gap is intentional per the schema:
 *   the reading itself is authored independently of what the metrics measure.
 *
 * KEY AGENTS:
 *   - Judicial Institution: Chief beneficiary; holds and expands final interpretive authority; gains legitimacy and institutional power from the reading
 *   - Legislative Majority: Primary payer; elected representatives enact policy within judicially-defined bounds; bears gridlock cost when courts invalidate legislation
 *   - Electoral Majorities: Secondary payer; voters cannot override judicial interpretation through normal democratic process; faces supermajority amendment requirement to overturn judicial doctrine
 *   - Legal Profession: Secondary beneficiary; professional authority and status rise when law is treated as specialized expertise domain
 *   - Counter-Majoritarian Interests: Tertiary beneficiary; minorities and dissenters gain veto power over majoritarian legislation through judicial review
 *   - Parliamentary Sovereignty Doctrine: Excluded voice; claims legislatures possess co-equal interpretive authority; structurally incompatible with judicial supremacy framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/institutional_design/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, 'a99ec41b-3ac2-4541-8b89-1cdf9e8f7406').
narrative_ontology:cs_kernel_codification('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', fixed_text).
narrative_ontology:cs_authority_grounding('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', extraction).
narrative_ontology:cs_interpretation_layer_present('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406').
narrative_ontology:cs_reading_relation('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', foundational, judicial_independence_necessary_for_neutral_interpretation).
narrative_ontology:cs_axiom_status(judicial_independence_necessary_for_neutral_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', judicial_independence_necessary_for_neutral_interpretation, instrumental).
narrative_ontology:cs_axiom('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', foundational, expertise_basis_legitimate_authority).
narrative_ontology:cs_axiom_status(expertise_basis_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', expertise_basis_legitimate_authority, deontological).
narrative_ontology:cs_reference_frame('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', judicial_independence_and_expertise).
narrative_ontology:cs_drift_state('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', contemporary_institutional_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a99ec41b-3ac2-4541-8b89-1cdf9e8f7406', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_institution).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts hold final interpretive authority over the constitution's meaning through specialized legal expertise, institutional independence (life tenure, salary protection), and insulation from electoral pressure. Judges elaborate constitutional doctrine, set precedent, and invalidate legislation deemed unconstitutional. The judicial monopoly on final interpretation consolidates institutional power and derives legitimacy from claims of neutral expertise and fidelity to foundational law.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_institution, agenda_setter,
    institutional, generational, analytical, national).

% Elected representatives enact legislation within the interpreted boundaries the courts establish. When courts invalidate legislation as unconstitutional, the legislative process bears the cost of revisiting, reframing, or abandoning policy goals. Supermajority requirements to override judicial interpretation (e.g., constitutional amendment) create a structural asymmetry where legislative will requires higher consensus to override judicial judgment.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_majority, payer,
    institutional, biographical, constrained, national).

% Voters elect representatives and expect their majorities to translate into enacted policy. When courts block legislation as unconstitutional, electoral majorities bear the frustration cost of gridlock. They cannot directly reverse judicial decisions through the legislative process alone; they must secure a supermajority or wait for generational shifts in court composition.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% The doctrine that judges must be insulated from electoral and political pressure to decide cases impartially. This reading vindicates the independence principle by treating it as foundational to legitimate constitutional interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_independence_doctrine, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_independence_doctrine).

% Attorneys and legal scholars gain professional authority and career advancement from the belief that law is a specialized domain requiring expert interpretation. Judicial supremacy elevates the status of legal expertise as the legitimate language of constitutional meaning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    powerful, generational, mobile, national).

% Individuals and groups whose interests would be defeated by electoral majorities (religious minorities, unpopular dissidents, property holders in redistributive contexts) benefit from a court system that can override majority will through constitutional interpretation. They gain veto power over certain majoritarian outcomes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_interests, beneficiary,
    powerful, generational, mobile, national).

% A hypothetical alternative institutional voice — the premise that legislatures themselves possess interpretive authority and finality over constitutional meaning in their own domain — is structurally excluded by the reading. Parliaments that claim co-equal authority are treated as breaching constitutional hierarchy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_courts_boundary_contestant, excluded,
    institutional, generational, constrained, national).

% Observers concerned with democratic legitimacy and majoritarian accountability measure the constraint's operation. They note the tension between democratic rule and counter-majoritarian judicial power, and track how judicial review affects legislative responsiveness.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, democratic_theorist_observer, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_institution).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional meaning across changing electoral cycles and majoritarian pressures by vesting final interpretive authority in a body insulated from political pressure. Solves the coordination problem of 'who decides what the foundational law means' by naming courts as the authoritative resolver.
% TRANSFER_FUNCTION: Moves interpretive authority from elected legislatures and electoral majorities to appointed judges; transfers the cost of gridlock when courts invalidate legislation from the judicial institution to the legislative/electoral system; transfers the benefit of counter-majoritarian protection to minorities and dissidents.
% ABSENT_VOICES: Legislatures claiming co-equal interpretive authority (parliamentary sovereignty reading) are structurally excluded from the judicial supremacy frame. Popular constitutionalism voices — those who believe constitutional meaning should emerge from ongoing democratic contestation rather than terminal judicial judgment — are also excluded.
% DISAPPEARANCE_RATIONALE: If judicial supremacy disappeared overnight, constitutional authority would redistribute: legislatures would claim final interpretive power, electoral majorities could enact legislation without fear of judicial nullification, and the gridlock costs currently borne by the legislative system would vanish. Counter-majoritarian protections would depend on legislative forbearance rather than judicial veto. The structure of institutional power would shift fundamentally.
% FOUNDING_PROBLEM: In early constitutional systems, multiple institutions claimed authority to interpret foundational law, producing competing readings and institutional conflict. The founding problem was to establish a clear, stable mechanism for constitutional meaning that would not shift with every election or political passion.
% FOUNDING_PROBLEM_CORROBORATION: The judicial institution and legal professional class attest the founding problem persists: legislative incursion on judicial independence threatens the stability of constitutional meaning. Legislatures and popular constitutionalists attest the problem is solved and the arrangement persists as institutional power capture: they argue that legislatures themselves can reliably interpret constitutions and that electoral accountability is a superior legitimacy mechanism to judicial independence. Comparative constitutional scholars document wide variation in how democracies allocate interpretive authority (some vest it in legislatures, some in courts, some distribute it), suggesting the founding problem is contingent, not universal.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the constraint concentrates interpretive authority in judges, enabling institutional rent-seeking while justified as neutral expertise. The series shows slow accumulation from 0.45 to 0.68 over 40 time units, modeling the expansion of judicial reach as courts elaborate doctrine and extend interpretive scope (early period: courts restrained by institutional caution; later period: courts confident in their authority, applying doctrine expansively). Suppression is high (0.72) because the reading enforces judicial exclusivity by delegitimizing legislative and popular claims to interpretive authority. When courts say 'the Constitution means X,' the reading forecloses legislative revision of that meaning except through formal amendment—a high-cost, low-frequency exit. Theater ratio rises to 0.41, capturing increasing reliance on legitimating narratives (expertise, neutrality, fidelity to original meaning or living law) as active justification for judicial authority, particularly when courts face political pressure or criticism. Accessibility collapse is moderate (0.62): once the system is understood, alternative institutional arrangements (parliamentary interpretation, popular deliberation) are theoretically accessible but practically blocked by path-dependence and constitutional entrenchment. Resistance is high (0.71): legislatures resist judicial incursion, populist movements challenge judicial authority, and constitutional scholars debate the legitimacy of judicial review. The constraint persists because multiple constituencies benefit (courts, lawyers, counter-majoritarian minorities) and because changing it requires high-consensus constitutional amendment.
 *
 * PERSPECTIVAL GAP:
 *   The judicial institution and the legal profession experience this constraint as a rope—genuine coordination (stable constitutional meaning) enabling professional authority. The legislative majority experiences it as a snare—they pay the cost of gridlock without meaningful exit, while courts collect the benefit of authority. Electoral majorities experience it as a snare with a secondary benefit: when courts block majoritarian legislation, some voters welcome the protection (minorities, dissidents), while others resent their policy defeats. Comparative court position: a legislator from a parliamentary sovereignty system (Westminster model) would classify this as extraction; a judge socialized into judicial supremacy would classify it as coordination. The engine computes this divergence from power/exit/time_horizon data: an institutional payer (legislative majority) with high suppression and high exit cost sits near the target end (d near 1.0), while an institutional beneficiary (judges) with analytical exit and authority sits near the beneficiary end (d near 0.0).
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial institution: d ≈ 0.1 (full beneficiary, institutional power, exit_options=analytical, no cost exposure). Legislative majority: d ≈ 0.85 (target, institutional power but constrained by judicial veto, time_horizon=biographical, exit_options=constrained, bears gridlock cost). Electoral majorities: d ≈ 0.8 (near-target, organized power but constrained by supermajority amendment requirement, bear electoral frustration cost). Legal profession: d ≈ 0.2 (secondary beneficiary, powerful actors who gain professional authority and income from legal expertise specialization). Counter-majoritarian interests: d ≈ 0.15 (beneficiary, gain veto power over majoritarian legislation, powerful actors with mobile exit). The reading does not declare directionality_overrides; the structural derivation from beneficiary/victim + exit produces accurate directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stable constitutional meaning across electoral cycles) is contested in status: the judicial institution and constitutional law scholars attest it remains live and requires judicial independence; legislatures and popular constitutionalists attest it is solved (legislatures can interpret constitutively; electoral cycles are features, not bugs) and the arrangement persists as institutional power capture. The disappearance verdict is world_rearranges: if judicial supremacy vanished, constitutional authority would redistribute to legislatures and electoral processes. This mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags potential mandatrophy: the constraint may have outlived its functional justification and now persists primarily because beneficiaries (judges, lawyers) maintain the legitimating narrative. The theater_ratio rising to 0.41 supports this: a growing share of enforcement activity is rhetorical (establishing expertise, claiming fidelity to law) rather than functional (preventing actual constitutional conflict through genuine coordination). However, the coordination function is not entirely atrophied: the constraint does stabilize constitutional meaning in some contexts (civil rights, First Amendment interpretation). The mandatrophy is partial, not complete—the constraint is a tangled rope (genuine coordination + asymmetric extraction) rather than a pure piton (atrophied function + theatrical maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_neutrality_vs_institutional_power,
    'Does judicial authority rest on genuine neutral expertise in constitutional law, or does it primarily consolidate institutional power under a legitimating expertise narrative?',
    'Comparative analysis of judicial decision-making: if judges converge on constitutional meaning independent of political pressure and party affiliation, the expertise claim is grounded; if judicial opinions correlate strongly with appointing president''s political alignment, the expertise narrative is cover for institutional power.',
    'If expertise is genuine, the constraint is a rope (coordination justified by real functional cost). If expertise is rhetorical, the constraint is a snare (extraction dressed as coordination). The empirical record—documented partisan drift in constitutional doctrine—suggests the expertise claim is partial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expertise_neutrality_vs_institutional_power, empirical, 'Whether judicial authority rests on specialized competence or institutional power capture').

omega_variable(
    amendment_difficulty_as_suppression,
    'Is the high cost of constitutional amendment (required to overturn judicial doctrine) a feature of the constitutional structure, or is it a suppressive mechanism that entrenches judicial power?',
    'Historical analysis of amendment patterns: if supermajority requirement were necessary only for foundational amendments and easy amendment has historically occurred for corrective measures, then suppression is the design function. If the supermajority requirement is systematically used to prevent reversal of judicial doctrines the electoral majority opposes, then suppression is entrenching judicial power.',
    'If suppression is structural design, the constraint is more rope-like (coordination justified by constitutional stability). If suppression primarily benefits judiciary by raising reversal costs, the constraint is more snare-like (extraction via trapped exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_difficulty_as_suppression, empirical, 'Whether amendment difficulty reflects constitutional principle or judicial entrenchment').

omega_variable(
    reading_foreclosure_parliamentary_sovereignty,
    'Is judicial supremacy logically compatible with parliamentary sovereignty within a single constitutional framework, or do the two readings foreclose each other?',
    'Constitutional theory examination: if a framework can coherently assert ''courts have final interpretive authority AND legislatures have final interpretive authority in their own domain,'' the readings coexist; if asserting both creates logical contradiction, they foreclose each other.',
    'If readings coexist, they are alternative readings held by different parties. If they foreclose, this reading logically eliminates the parliamentary sovereignty reading as internally coherent—a stronger claim of structural priority. The ''judicial supremacy vs. parliamentary sovereignty'' debate in constitutional theory (Marbury v. Madison vs. legislative supremacy models) hinges on this ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_parliamentary_sovereignty, conceptual, 'Whether judicial supremacy and parliamentary sovereignty can coexist or necessarily exclude each other').

omega_variable(
    counter_majoritarian_protection_legitimacy,
    'Does the counter-majoritarian function of judicial review constitute a legitimate coordination benefit (protecting constitutional minorities from majoritarian tyranny), or does it primarily enable institutional extraction (courts blocking majoritarian legislation using minority-protection rhetoric)?',
    'Empirical study of judicial review patterns: if courts consistently protect vulnerable minorities and prevent majoritarian oppression of rights, the legitimacy claim holds. If courts use minority protection rhetoric to block majoritarian redistribution or regulation of concentrated wealth, the rhetorical cover is revealed.',
    'If counter-majoritarian protection is genuine, the constraint''s beneficiary set expands to include constitutional minorities, supporting the coordination framing. If it is primarily rhetoric, minorities are false beneficiaries and the constraint is more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_protection_legitimacy, empirical, 'Whether counter-majoritarian judicial review genuinely protects minorities or primarily enables institutional power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(basi_tr_t30, observed).
narrative_ontology:measurement(basi_tr_t35, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(basi_tr_t35, observed).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(basi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(basi_be_t30, observed).
narrative_ontology:measurement(basi_be_t35, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(basi_be_t35, observed).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(basi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(basi_su_t30, observed).
narrative_ontology:measurement(basi_su_t35, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(basi_su_t35, observed).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(basi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.14).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_authority kernel. The sibling readings (parliamentary_sovereignty, popular_constitutionalism) are separate constraint stories with different beneficiary/victim structures and different ε values. All three readings compete for interpretive authority over the same foundational law; together they model the contested institutional arrangement. Links flow downstream from judicial supremacy (the most institutionally entrenched reading) to the alternative readings (which face suppression under the supremacy regime). Decomposition is necessary because a single constraint cannot encode three contradictory authority claims without fabricating a measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
