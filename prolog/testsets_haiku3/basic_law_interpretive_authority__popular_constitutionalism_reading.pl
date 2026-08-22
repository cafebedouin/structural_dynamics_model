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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Democratic Contestation of Constitutional Meaning
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Popular constitutionalism claims that constitutional meaning is not the
 *   terminal product of judicial or legislative authority but emerges through
 *   ongoing contestation by democratic publics, social movements, and
 *   multiple institutional sites. Under this reading, a constitution is not
 *   an object courts finalize but a living arena where meaning remains
 *   perpetually open. The constraint distributes gridlock costs across
 *   institutions (courts lose supremacy, legislatures face persistent
 *   override, publics sustain contestation) while benefiting those who gain
 *   voice outside formal adjudication. This is a TANGLED ROPE: it coordinates
 *   distributed authority (solves the legitimacy crisis) while extracting
 *   costs through perpetual contestation and institutional instability. It
 *   extracts from doctrines of finality and from the legal profession's
 *   monopoly on authoritative interpretation.
 *
 * KEY AGENTS:
 *   - Democratic publics: Gain standing as constitutional interpreters through contestation; benefit from voice but bear costs of ongoing struggle.
 *   - Appellate judiciary: Lose terminal authority; benefit from popular pressure that constrains their power but suffer loss of institutional supremacy.
 *   - National legislature: Retain veto power against judicial interpretation; benefit from leverage but lose stability of judicial closure.
 *   - Legal professional class: Lose monopoly on interpretation; expertise remains relevant but no longer commanding.
 *   - Extra-judicial movements: Gain recognition as legitimate interpreters; contestation is both opportunity and burden.
 *   - Institutional stability constituency: Excluded from the reading; would defend finality and predictability but are not seated.
 *   - Constitutional scholars: Observe and analyze the constraint's legitimacy conditions and gridlock costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism: Democratic Contestation of Constitutional Meaning").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, '25fa3423-b92b-4da9-9abb-655e35e82c7d').
narrative_ontology:cs_kernel_codification('25fa3423-b92b-4da9-9abb-655e35e82c7d', formalized).
narrative_ontology:cs_authority_grounding('25fa3423-b92b-4da9-9abb-655e35e82c7d', distributed).
narrative_ontology:cs_reading_relation('25fa3423-b92b-4da9-9abb-655e35e82c7d', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('25fa3423-b92b-4da9-9abb-655e35e82c7d', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('25fa3423-b92b-4da9-9abb-655e35e82c7d', foundational, constitutional_meaning_perpetually_contestable).
narrative_ontology:cs_axiom_status(constitutional_meaning_perpetually_contestable, holdable).
narrative_ontology:cs_axiom_grounding('25fa3423-b92b-4da9-9abb-655e35e82c7d', constitutional_meaning_perpetually_contestable, deontological).
narrative_ontology:cs_axiom('25fa3423-b92b-4da9-9abb-655e35e82c7d', foundational, distributed_interpretive_authority_legitimacy).
narrative_ontology:cs_axiom_status(distributed_interpretive_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('25fa3423-b92b-4da9-9abb-655e35e82c7d', distributed_interpretive_authority_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('25fa3423-b92b-4da9-9abb-655e35e82c7d', democratic_contestation_framework).
narrative_ontology:cs_drift_state('25fa3423-b92b-4da9-9abb-655e35e82c7d', contemporary_judicialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25fa3423-b92b-4da9-9abb-655e35e82c7d', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_publics).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, extra_judicial_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, local_political_communities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional_stability_doctrines).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legal_professional_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, national_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, subnational_communities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, appellate_judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, national_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legal_professional_class).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_meaning_as_process).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, distributed_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in ongoing constitutional meaning-making through protest, electoral choice, social movement, and contestation. They claim voice in defining what the constitution means through channels outside formal legal adjudication. Their benefit is recognition as legitimate interpreters rather than passive subjects of judicial decree.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_publics, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_publics, agenda_setter).

% Holds formal authority to resolve constitutional disputes, but under this reading loses the claim to TERMINAL authority. Their interpretations remain subject to contestation, legislative override, and public resistance. They bear the cost of diminished institutional supremacy and face ongoing pressure to justify decisions in terms popular movements find legitimate.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, appellate_judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, appellate_judiciary, agenda_setter).

% Retains power to respond to constitutional contestation through legislation, amendment, and noncompliance, but also faces the cost of perpetual contestation. They gain leverage against judicial supremacy but lose the stability of judicial finality. Gridlock costs distribute when courts, legislatures, and publics operate from conflicting constitutional readings.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, national_legislature, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, national_legislature, beneficiary).

% Loses the monopoly on authoritative constitutional interpretation. Under judicial supremacy they are gatekeepers of legitimate meaning; under popular constitutionalism, lawyers compete with organizers, activists, and publics for interpretive voice. Their expertise remains relevant but no longer commanding.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legal_professional_class, payer,
    powerful, biographical, constrained, national).

% Gain standing to contest and reinterpret constitutional meaning locally. They are not bound to await federal judicial resolution but can organize resistance, craft alternative readings, and force renegotiation of constitutional commitments through local political action.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, subnational_communities, beneficiary,
    moderate, generational, mobile, regional).

% Social movements, civil rights organizations, religious communities, and advocacy networks gain standing as legitimate constitutional interpreters. Their contestation shapes the constraint's meaning through protest, electoral mobilization, and reframing of constitutional values. They collect legitimacy and voice rather than material extraction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, extra_judicial_movements, beneficiary,
    organized, biographical, mobile, national).

% Actors invested in predictable, finalized constitutional interpretation (institutional investors, long-horizon planners, rule-of-law defenders) are structurally sidelined. They would argue for closure and finality but are not seated in the reading's operative framework. Their voice is present only as resistance to contestation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional_stability_constituency, excluded,
    institutional, generational, trapped, national).

% Analyze the legitimacy conditions of constitutional interpretation, trace how meaning shifts through contestation, and measure the cost of perpetual gridlock. They occupy no institutional seat but provide frameworks for understanding the constraint's operation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of constitutional legitimacy in a polity without a single authoritative source: instead of terminal adjudication by courts or legislatures, constitutional meaning emerges through ongoing contestation, allowing multiple institutional sites and popular movements to claim voice without requiring agreement on a highest arbiter.
% TRANSFER_FUNCTION: Transfers interpretive authority from centralized institutional gatekeepers (judges, established legal professionals) to distributed actors (social movements, electoral coalitions, subnational communities, ordinary citizens). It extracts stability costs—gridlock, delayed closure, perpetual contestation—and concentrates them at the sites of institutional ambiguity. It transfers legitimacy from expertise to popular will.
% ABSENT_VOICES: Actors invested in finalized, predictable constitutional meaning (long-horizon institutional investors, rule-of-law formalists, regime legitimacy custodians) are excluded from the reading's operative framework. They would argue that perpetual contestation erodes constitutional constraint itself and that authority must settle somewhere; their concerns are structural resistance to the reading, not seated parties.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism as a practice disappeared, courts would likely reassert terminal interpretive authority and legislatures would lose contestatory leverage. Publics would lose the incentive to organize around constitutional meanings that courts would simply override. But the disappearance would also restore institutional predictability and reduce gridlock costs. Whether the world rearranges depends on whether you read the disappearance as liberation from destructive contestation or as loss of democratic voice.
% FOUNDING_PROBLEM: How can constitutional meaning remain legitimate in a democracy when no single institution—courts, legislatures, or executive—can claim transparent democratic authority to decide it? The founding problem is the legitimacy crisis of terminal adjudication: courts claim independence from politics but lack electoral mandate; legislatures have mandate but are structurally biased toward inertia and faction. Popular constitutionalism solves this by distributing interpretive authority back to the democratic public, making contestation itself the legitimating process.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and democratic theorists outside the judiciary (Ackerman, Tushnet, Kramer) attest that the legitimacy problem is unresolved; judicial rulings regularly meet sustained public and legislative resistance (contraception, abortion, voting rights, immigration, surveillance), and these conflicts cannot be settled by court pronouncement alone. Courts and institutional stability advocates attest the founding problem is solved by their respective institutions' authority; public contestation of their rulings contradicts this claim.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62) because the constraint extracts institutional stability and closure costs from courts and legislatures while distributing burden of ongoing contestation across the public. Suppression is moderate (0.58) because the constraint requires active work to maintain perpetual contestation and prevent institutional closure—movements must organize, legislatures must resist finality, courts must accept revision. Theater is near symmetric (0.47) because the reading involves genuine contestation (the coordination function is real) but also relies on performative reframing: courts claim to respect popular will while retaining substantial gatekeeping power; legislatures claim to represent democratic voice while using procedure to block constitutional shifts; movements frame contestation as democratic participation while accepting perpetual frustration. Accessibility collapse is low (0.41) because alternatives remain persistently available—courts could reassert supremacy, legislatures could accept finality, publics could withdraw from contestation. Resistance is high (0.73) because institutions and rule-of-law doctrines actively resist the reading's claim that interpretation is perpetually open. The measurement series shows extractiveness rising through the interval's early phase (as the costs of maintaining contestation became clearer) and then plateauing, with theater rising slightly and suppression requirements moderating as the constraint normalized.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute dramatically differently. From the democratic publics' perspective, the constraint is a genuine coordination function (distributed authority solves the legitimacy problem) with moderate extraction (they bear the burden of organizing). From the judiciary's perspective, it is pure extraction (loss of authority without compensation) and delegitimization. From the legislature's perspective, it is a mixed coordination and extraction (they gain leverage but lose stability of closure). From the legal professional class, it is pure extraction (loss of monopoly). The engine computes these divergences from the structural data: beneficiary seats get low directionality (low effective extraction), payer seats get high directionality (high effective extraction). The asymmetry is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic publics are structural beneficiaries—they gain voice and standing without having to capture formal institutions. Their directionality is low (d ≈ 0.2): they collect interpretive authority. The judiciary loses terminal authority without compensation (victims in this reading), with directionality high (d ≈ 0.85). The legislature gains leverage but loses closure—directionality moderate (d ≈ 0.55). The legal profession loses monopoly—directionality high (d ≈ 0.80). Subnational communities and extra-judicial movements are beneficiaries (directionality ≈ 0.15), gaining standing and voice. Institutional stability doctrines and rule-of-law constituencies are victims of the reading's core premise (directionality ≈ 0.90, but they are not agents; the doctrines and constituencies they anchor are structural targets). The reading requires active enforcement of non-finality—courts must accept revision, legislatures must resist procedural closure, movements must sustain contestation—giving requires_active_enforcement: true.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional legitimacy in a polity without a single authoritative source) remains live: courts are regularly defied, legislatures override through amendment and statutory restatement, publics mobilize in constitutional contestation. The constraint prevents misclassification as pure rope (coordination without extraction) because the extraction costs are real and distributed (institutional instability, gridlock, perpetual contestation burden). It also prevents misclassification as pure snare (extraction with coordination cover) because the coordination function is structural—distributing authority does solve the legitimacy problem in a way centralized authority cannot. The classification as tangled rope is precise: both coordination and asymmetric extraction are present and necessary to the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_stability_tradeoff,
    'Does distributed democratic contestation produce greater constitutional legitimacy, or does it erode the rule-of-law stability required for constitutional legitimacy to function?',
    'Comparative institutional analysis of polities with strong popular constitutionalism (post-apartheid South Africa, post-1945 Germany with citizen mobilization) versus those with strong judicial supremacy (post-WWII US), measuring both constitutional legitimacy (public acceptance, norm-following without coercion) and rule-of-law stability (institutional predictability, property security, contract enforceability).',
    'If distributed contestation produces greater legitimacy without sacrificing stability, popular constitutionalism is a genuine coordination improvement. If it trades legitimacy for stability in ways that harm rule of law, the constraint becomes purely extractive, reclassifying toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_stability_tradeoff, empirical, 'Whether popular contestation increases or decreases constitutional legitimacy and rule-of-law stability.').

omega_variable(
    foreclose_or_coexist_judicial_supremacy,
    'Does the popular constitutionalism reading logically FORECLOSE judicial supremacy (making them mutually exclusive within any single framework), or do they COEXIST as competing readings held by different institutional actors?',
    'Analyze whether a single judge or court could coherently hold both ''courts have final authority'' (judicial supremacy axiom) and ''constitutional meaning emerges from ongoing contestation'' (popular constitutionalism axiom) without self-contradiction. If the axioms directly contradict—if one asserts finality and the other perpetual openness—they foreclose. If both can be held by different parties in the same polity, they coexist.',
    'If foreclosure, the reading relations should be ''forecloses'' rather than ''coexists_with,'' and the engine computes stronger incompatibility for the kernel. If coexistence, the readings are live alternatives in an ongoing institutional dispute with no logical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclose_or_coexist_judicial_supremacy, conceptual, 'Whether popular constitutionalism and judicial supremacy are logically incompatible or can coexist in the same framework.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of non-contestation (the enforced openness of constitutional meaning) structurally maintained through institutional mechanisms (courts reversing precedent, legislatures overriding, movements organizing) or internalized through constitutional culture (publics expecting perpetual contestation, accepting non-closure as normal)?',
    'Track post-exit suppression patterns: if suppression persists even when movements cease organizing or courts attempt closure, it is internalized; if suppression requires active institutional work to maintain, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the measured structural component suggests—the public has internalized the obligation to contest. If structural, the suppression is conditional on active institutional work and could relax if institutions chose to enforce finality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the constraint''s suppression (enforced contestation) is structural or internalized.').

omega_variable(
    reading_identity_fusion,
    'Is the popular constitutionalism reading HELD by agents as a strategic position (they could exit to judicial supremacy if incentives shifted) or FUSED with their institutional or movement identity (they cannot exit without losing core self-definition)?',
    'Examine agents'' revealed preferences when exit becomes possible: do democratic movements abandon popular constitutionalism rhetoric when courts offer settlement (strategic); do they persist even when courts offer closure, framing contestation as intrinsic to democracy (identity-fused)?',
    'If identity-locked, the reading''s exit_options for beneficiary agents should be rated ''identity_locked'' rather than ''mobile,'' raising effective extraction through reduced escape routes. This would increase the snare-pressure of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion, empirical, 'Whether popular constitutionalism is a strategic position or an identity commitment for its beneficiary movements.').


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
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement_basis(basi_tr_t30, observed).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement_basis(basi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(basi_be_t30, observed).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(basi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(basi_su_t30, observed).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(basi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'basic_law_interpretive_authority.' Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and extraction profiles. JUDICIAL_SUPREMACY_READING: courts hold final authority (lower extractiveness, high suppression of non-legal voices); PARLIAMENTARY_SOVEREIGNTY_READING: legislature holds final authority (high extraction from publics excluded from legislative process); POPULAR_CONSTITUTIONALISM_READING: contestation is perpetual (moderate extractiveness distributed across institutions). The readings are linked through network.affects_constraints because each shifts the legitimacy burden on the others—they form an interdependent kernel family, not a single constraint viewed from multiple angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
