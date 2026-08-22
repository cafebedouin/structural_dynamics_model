% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Interpretive Authority Through Democratic Contestation
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   Popular constitutionalism is one reading of how the U.S. Constitution's
 *   meaning is authoritatively determined. This reading holds that
 *   constitutional interpretation is not the exclusive province of courts —
 *   instead, constitutional meaning emerges from sustained democratic
 *   contestation among popular movements, legislatures, the executive, and
 *   courts. The civil rights movement, environmental movement, labor
 *   organizing, and recent populist mobilizations all exemplify popular
 *   constitutionalism: constituencies organize, pressure legislatures, occupy
 *   the streets, and challenge courts to recognize meanings aligned with
 *   democratic will rather than judicial precedent. The reading is claimed by
 *   its advocates as liberatory (democratizing constitutional voice) and
 *   resisted by its critics as destabilizing (undermining rule of law and
 *   counter-majoritarian protection for minorities). This JSON instantiates
 *   the popular constitutionalism reading as a constraint story: it names who
 *   benefits (popular movements, legislative majorities, anti-elitist
 *   constituencies), who bears the cost (judicial finality advocates,
 *   minorities needing counter-majoritarian protection), and how the
 *   authority structure operates (judicial authority contested and co-shaped
 *   by democratic pressure). The constraint is classified as TANGLED ROPE
 *   because it genuinely solves a coordination problem (how do we legitimize
 *   constitutional interpretation to non-elite constituencies) while
 *   asymmetrically extracting from vulnerable minorities who depend on the
 *   counter-majoritarian judicial authority it undermines.
 *
 * KEY AGENTS:
 *   - Popular movements: civil rights, labor, environmental, LGBTQ+ activism; beneficiary of democratized interpretive authority
 *   - Judicial branch (Supreme Court, federal courts): agenda-setter and enforcer; pays cost of contested authority
 *   - Legislative majorities: beneficiary; claim co-interpretive power
 *   - Minorities (racial, religious, LGBTQ+): victims; dependent on counter-majoritarian judicial review
 *   - Constitutional settlement seekers: institutional actors (corporations, established interests); pay cost of perpetual contestation
 *   - Academic theorists: observers; analyze the structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.72).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism: Interpretive Authority Through Democratic Contestation").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '6e1810b9-a33e-4917-8d2b-740453ca2e47').
narrative_ontology:cs_kernel_codification('6e1810b9-a33e-4917-8d2b-740453ca2e47', fixed_text).
narrative_ontology:cs_authority_grounding('6e1810b9-a33e-4917-8d2b-740453ca2e47', distributed).
narrative_ontology:cs_reading_relation('6e1810b9-a33e-4917-8d2b-740453ca2e47', us_constitution_interpretive__us_constitution_interpretive_originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e1810b9-a33e-4917-8d2b-740453ca2e47', us_constitution_interpretive__us_constitution_interpretive_living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('6e1810b9-a33e-4917-8d2b-740453ca2e47', foundational, constitutional_meaning_democratically_contestable).
narrative_ontology:cs_axiom_status(constitutional_meaning_democratically_contestable, holdable).
narrative_ontology:cs_axiom_grounding('6e1810b9-a33e-4917-8d2b-740453ca2e47', constitutional_meaning_democratically_contestable, deontological).
narrative_ontology:cs_axiom('6e1810b9-a33e-4917-8d2b-740453ca2e47', foundational, popular_movements_are_constitutional_interpreters).
narrative_ontology:cs_axiom_status(popular_movements_are_constitutional_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('6e1810b9-a33e-4917-8d2b-740453ca2e47', popular_movements_are_constitutional_interpreters, conventional).
narrative_ontology:cs_reference_frame('6e1810b9-a33e-4917-8d2b-740453ca2e47', democratic_constitutional_authority).
narrative_ontology:cs_drift_state('6e1810b9-a33e-4917-8d2b-740453ca2e47', contemporary_anti_majoritarian_backlash, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e1810b9-a33e-4917-8d2b-740453ca2e47', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_constituencies).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_seekers).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minorities_dependent_on_counter_majoritarian_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social movements (civil rights, labor, feminist, environmental) claim interpretive authority over constitutional meaning by mobilizing constituencies, engaging legislatures, and pressuring courts through sustained political action. They argue the Constitution belongs to the people, not judges alone. Their power derives from collective organizing and democratic participation; exit is shifting focus to local or state arenas or international forums.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, generational, mobile, national).

% Congress and state legislatures claim authority to interpret the Constitution through statute-making, constitutional amendment, and budgetary power. Popular constitutionalism decentralizes interpretation away from courts toward the legislative branch. Their power is institutional; exit involves accepting judicial supremacy they resist.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, generational, mobile, national).

% Voters and citizens who distrust unelected judges and believe constitutional interpretation should respond to democratic will rather than elite judicial pronouncements. They benefit from the popular constitutionalism frame, which legitimates their claim to constitutional voice. Exit involves acquiescing to judicial authority or relocating.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_constituencies, beneficiary,
    powerless, biographical, constrained, national).

% Judges, legal scholars, and constitutional theorists who argue judicial review requires finality: the Supreme Court's constitutional interpretation must be binding, not subject to reversal by popular movements or legislative re-interpretation. They pay the cost of contested authority and institutional legitimacy challenges. Exit involves ceding interpretive power.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Institutional and corporate actors who depend on stable, predictable constitutional rules and fear that perpetual democratic contestation over meaning creates legal uncertainty and prevents settled governance. They bear the cost of interpretive instability. Exit is limited; they must engage in the same political struggle or accept outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_seekers, payer,
    powerful, generational, constrained, national).

% Racial minorities, religious minorities, LGBTQ+ populations, and other groups whose constitutional rights depend on judicial protection against majoritarian override. Popular constitutionalism's emphasis on democratic contestation and legislative interpretation threatens their primary refuge: counter-majoritarian judicial review. They are both victims of the constraint and excluded from its primary beneficiary frame.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minorities_dependent_on_counter_majoritarian_protection, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, minorities_dependent_on_counter_majoritarian_protection, excluded).

% The courts, especially the Supreme Court, must navigate the claim to sole interpretive authority while facing sustained challenge from popular movements and legislatures. They enforce constitutional doctrine while that doctrine's authority is itself under contestation. Exit involves surrendering authority they have historically exercised.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).

% Law professors and constitutional scholars who articulate, defend, and critique the popular constitutionalism reading and its relationship to sibling readings. They analyze the constraint's operation without directly collecting or paying.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, academic_constitutional_theorists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels constitutional meaning-making through democratic processes rather than judicial pronouncement alone: popular movements, legislatures, and the executive branch claim interpretive authority, legitimating broader participation in constitutional governance and reducing dependence on judicial interpretation as the sole authoritative voice.
% TRANSFER_FUNCTION: Transfers interpretive authority from judges (institutional, insulated seat) to popular movements and electoral majorities (democratic, responsive seats). Also transfers the cost of constitutional settlement and legal predictability to those who lose stability when meaning is contested perpetually.
% ABSENT_VOICES: Minorities dependent on counter-majoritarian protection are structurally silenced in the popular constitutionalism frame: their strongest voice (the courts) is demoted, and their weakest power base (the ballot, democratic contestation) is elevated. They are present as victims but absent from the beneficiary coalition that drives this reading.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism as a binding interpretive principle disappeared, constitutional meaning would revert to judicial supremacy: courts would once again be the final arbiter, legislatures would have weaker claim to co-interpretive authority, popular movements would lose their primary rhetorical and political vehicle for constitutional change outside the ballot and litigation. The entire landscape of how constitutional contestation is framed and resolved would shift.
% FOUNDING_PROBLEM: Judicial supremacy in constitutional interpretation concentrates power in an unelected body insulated from democratic accountability; constitutional meaning emerges from a narrow elite of judges and lawyers rather than from the people's understanding of their fundamental law.
% FOUNDING_PROBLEM_CORROBORATION: Popular constitutionalism scholars (Tushnet, Kramer, Balkin) and social movement historians attest the founding problem is live and urgent — courts regularly impose constitutional meanings at odds with popular understanding and democratic will (voting rights cases, abortion, campaign finance). Judicial finality advocates dispute this characterization, arguing courts serve as counter-majoritarian protection. Academic analysis from outside the beneficiary movements (comparative constitutional scholars, political scientists) corroborates that judicial-legislative-popular contestation over meaning is a persistent feature of constitutional governance, though they debate whether the popular constitutionalism reading diagnoses or solves it.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1960) to 0.68 (2024) because the popular constitutionalism reading, as it becomes more institutionalized and articulated, increasingly transfers interpretive authority from courts to majoritarian processes — a genuine benefit for organized constituencies, but an extraction from minorities who need counter-majoritarian protection. The rise is not monotonic (steeper 1960–1990 during civil rights era, plateauing 2005–2024 as the reading becomes established legal theory) because its extraction mechanism is paradoxical: it benefits through democratic legitimacy while harming through majoritarian override. Suppression requirement rises from 0.45 to 0.72 because the judicial branch must actively resist and contain the claim to co-interpretive authority to preserve its own power; popular movements must suppress contrary arguments (that courts are the proper interpreter, that stability requires finality) to advance their reading. Theater ratio rises from 0.18 to 0.41 because an increasing share of popular constitutionalism activity is performative — claiming democratic legitimacy through social media, street protest, and symbolic constitutional gestures — rather than producing binding constitutional change. The three metrics share one time grid (both measured at 1960, 1975, 1990, 2005, 2015, 2024) so temporal analysis can assess their covariation without misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The popular movements and legislative majorities perceive a tangled_rope (real coordination function: legitimate democratic voice in constitutional meaning) with acceptable asymmetry (majorities should prevail). Judicial finality advocates perceive a snare (no real coordination, pure extraction of judicial authority by majoritarian pressure). Minorities perceive a snare-weighted tangled_rope (coordination benefit for the broader public, but extraction from their specific protection). The engine's per-seat computation reveals these divergences; the story-level claim (tangled_rope) reflects the reading's own self-characterization — the constraint as it appears from the democratic-contestation seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The core asymmetry is power + exit. Popular movements are organized and mobile (can shift tactics, venues, framing) but powerless institutionally — beneficiaries with good exit and constrained power receive low d (subsidy-tilted). Legislative majorities are institutional and powerful (can amend, budget, legislate) — beneficiaries with mobile exit and institutional power receive low d (subsidy-tilted). Judicial finality advocates are institutional and powerful but trapped by the contestation (cannot exit the constitutional system, can only resist) — payers with constrained exit and institutional power receive high d (target-tilted). Minorities are powerless and trapped by identity (cannot exit their status as minority; constitutional meaning always applies to them) — victims with trapped exit and powerless power receive highest d (full-target). Judicial branch straddles: it sets the agenda but its agenda is contested; directionality is near-symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not present in this reading. The founding problem (judicial supremacy is undemocratic) remains live, and the constraint addresses it directly (democratizes interpretive authority). No gap exists between the constraint's function and its justification. However, a sibling reading (originalism) might experience mandatrophy if originalist interpreters argued 'courts should apply the Constitution as written/intended' but then judicial supremacy itself became the problem (if originalist judges issued rulings contrary to majoritarian will and faced delegitimation). That would be a different constraint's mandatrophy, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_foreclosure,
    'Do the core premises of popular constitutionalism and originalism logically foreclose each other within a single constitutional framework, or can they coexist as different hermeneutic approaches?',
    'Textual analysis of whether ''meaning fixed at ratification'' (originalism) and ''meaning shaped by ongoing democratic contestation'' (popular constitutionalism) represent contradictory or merely different emphases on the same text. If originalists can accommodate evolving application without changing fixed meaning, coexistence is possible; if popular constitutionalism requires meaning to drift over time in response to majoritarian pressure, foreclosure is real.',
    'If foreclosure: the engine''s reading_relations would mark ''forecloses'' rather than ''coexists_with''. If coexistent: the two readings are live alternatives, suggesting the kernel permits multiple legitimate readings. Classification of this constraint would remain tangled_rope either way, but the kernel''s stability would differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether popular constitutionalism and originalism are contradictory or complementary approaches to the same constitutional text.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of counter-majoritarian protection primarily structural (institutional power differentials, voting weight, legislative access) or internalized (cultural legitimacy granted to majority rule and democratic process)?',
    'Post-movement analysis: if suppression persists after structural barriers (like gerrymandering or voting restrictions) are removed but majoritarian cultural narratives remain strong, suppression is internalized. If suppression dissipates as institutions democratize, suppression is structural. Comparison across jurisdictions with different institutional designs and cultural narratives about democracy.',
    'If internalized: minorities retain suppression even after exit from legal/political barriers, and the constraint''s effective suppression is higher than structural measurement suggests. If structural: fixing institutions removes suppression. This affects whether fixing popular constitutionalism (restoring judicial supremacy) would actually protect minorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'The mechanism of suppression of counter-majoritarian values in majoritarian democratic order.').

omega_variable(
    democratic_legitimacy_vs_tyranny_of_majority,
    'Does democratizing constitutional interpretation increase legitimacy without increasing the tyranny-of-majority risk to minorities, or are these inextricably linked such that gains in democratic voice for majorities necessarily amplify minority vulnerability?',
    'Empirical study of correlation between popular constitutionalism strength (legislative override of judicial interpretation, social movement constitutional pressure) and minority rights protection across time and jurisdictions. Do democracies that grant constitutional voice to popular movements show better or worse minority protection than those insulating courts?',
    'If legitimacy and protection are decoupled (democratic gains need not harm minorities): the extraction is illusory; the constraint might reclassify toward rope. If they are linked (gains in democratic voice necessarily increase majoritarian pressure on minorities): extraction is structural and unavoidable, supporting tangled_rope or snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_tyranny_of_majority, empirical, 'Whether democratic constitutionalism and counter-majoritarian minority protection can coexist or are intrinsically opposed.').

omega_variable(
    alternate_framing_of_popular_constitutionalism,
    'Is popular constitutionalism better framed as a corrective to judicial supremacy (the framing adopted here) or as a cover story for majoritarian override of constitutionally protected rights?',
    'Genealogical analysis of which movements invoke popular constitutionalism and toward what ends: if civil rights and minority-protection movements invoke it, the corrective framing is authentic; if majoritarian and anti-rights movements invoke it, the override framing is more accurate. Historical study of which constitutional meanings popular movements have successfully imposed through contestation (were they protective or extractive of vulnerable groups?).',
    'This framing determines whether beneficiaries and victims are correctly identified. If popular constitutionalism is primarily a majoritarian tool, the victims might be more numerous and the beneficiaries more concentrated than authored. This would affect the constraint''s type: a snare (pure extraction) rather than tangled_rope (genuine coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternate_framing_of_popular_constitutionalism, conceptual, 'Whether popular constitutionalism is a democratizing corrective or a majoritarian power grab dressed as democratization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1960, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(us_c_tr_t1975, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1975, 0.24).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1990, 0.31).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1960, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(us_c_be_t1975, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1960, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(us_c_su_t1975, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive_originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive_living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacy_doctrine).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, legislative_override_mechanisms).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, social_movement_constitutional_mobilization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'us_constitution_interpretive' (How is the Constitution's meaning authoritatively determined?). Sibling readings are originalist (meaning fixed at ratification) and living-constitution (meaning evolves with contemporary values). Popular constitutionalism differs in asserting that meaning emerges from democratic contestation among courts, legislatures, and popular movements — not from courts alone, not from original text alone, but from ongoing political struggle. The three readings have different beneficiaries, different types, and different extracted values. Network edges capture the structural relationships: this reading influences the others by contesting their authority claims; it is influenced by originating in reaction to perceived judicial supremacy (captured in the network edge to judicial_supremacy_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
