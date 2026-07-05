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
 *   human_readable: Popular Constitutionalism Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the popular constitutionalism reading of the
 *   contested kernel governing U.S. constitutional interpretive authority:
 *   the claim that constitutional meaning is legitimately generated through
 *   sustained political mobilization and democratic contestation, not solely
 *   (or even primarily) through judicial pronouncement. This is one of three
 *   sibling readings of the same kernel — originalist (meaning fixed at
 *   ratification) and living-constitution (meaning evolves through reasoned
 *   judicial adaptation) are separate constraint stories with their own ε
 *   values, beneficiary sets, and classifications. Do not average across
 *   readings; this file characterizes only the popular constitutionalism
 *   claim on its own terms.
 *
 * KEY AGENTS:
 *   - popular_political_movements: Primary beneficiary (organized/mobile) — gains legitimated extra-judicial constitutional authority
 *   - legislative_majorities: Primary beneficiary (institutional/mobile) — gains standing to contest judicial doctrine through statute and amendment campaigns
 *   - judicial_finality_advocates: Primary target (institutional/constrained) — professional authority diminished by contestability claim
 *   - counter_majoritarian_protected_minorities: Primary target (powerless/trapped) — loses insulation from majoritarian political pressure
 *   - constitutional_law_scholars: Analytical observer — documents the empirical and normative dispute without resolving it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.52).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.44).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'd171f908-6c59-4b22-bb74-4b694795e88b').
narrative_ontology:cs_kernel_codification('d171f908-6c59-4b22-bb74-4b694795e88b', distributed).
narrative_ontology:cs_authority_grounding('d171f908-6c59-4b22-bb74-4b694795e88b', distributed).
narrative_ontology:cs_reading_relation('d171f908-6c59-4b22-bb74-4b694795e88b', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d171f908-6c59-4b22-bb74-4b694795e88b', us_constitution_interpretive__living_constitution_reading, influences).
narrative_ontology:cs_axiom('d171f908-6c59-4b22-bb74-4b694795e88b', foundational, popular_mobilization_confers_constitutional_authority).
narrative_ontology:cs_axiom_status(popular_mobilization_confers_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('d171f908-6c59-4b22-bb74-4b694795e88b', popular_mobilization_confers_constitutional_authority, conventional).
narrative_ontology:cs_axiom('d171f908-6c59-4b22-bb74-4b694795e88b', secondary, judicial_pronouncement_is_provisional_not_final).
narrative_ontology:cs_axiom_status(judicial_pronouncement_is_provisional_not_final, holdable).
narrative_ontology:cs_axiom_grounding('d171f908-6c59-4b22-bb74-4b694795e88b', judicial_pronouncement_is_provisional_not_final, conventional).
narrative_ontology:cs_reference_frame('d171f908-6c59-4b22-bb74-4b694795e88b', judicial_review_as_contestable_practice).
narrative_ontology:cs_drift_state('d171f908-6c59-4b22-bb74-4b694795e88b', contemporary_polarized_confirmation_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('d171f908-6c59-4b22-bb74-4b694795e88b', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_political_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, settlement_dependent_commercial_actors).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_protected_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize outside courts — through elections, protest, legislative pressure, constitutional amendment campaigns — to press claims about what the Constitution means. Under this reading their sustained political mobilization (abolitionism, women's suffrage, the civil rights movement, the New Deal realignment) is treated as a legitimate mode of constitutional lawmaking, not mere advocacy awaiting judicial ratification. They gain standing to reshape settled meaning without waiting on courts.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, agenda_setter).

% Enact statutes and pursue constitutional amendments that embody a contested reading of constitutional text, sometimes in direct tension with prior judicial doctrine. This reading treats their democratic mandate as an independent source of constitutional meaning, entitled to contest and sometimes supersede judicial pronouncement through sustained political action rather than deferring to courts as final arbiter.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, agenda_setter).

% Populist and grassroots actors who reject the premise that unelected judges hold privileged access to constitutional truth. They benefit from a framework legitimizing their claims that the document belongs to the people who ratify amendments and elect representatives, not to a credentialed interpretive priesthood.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_political_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Judges, appellate lawyers, and legal scholars whose professional authority rests on courts having the last word on constitutional meaning. Under popular constitutionalism their rulings are treated as provisional and contestable rather than final, subject to being overridden through sustained political mobilization, court-packing threats, or jurisdiction-stripping. Their institutional standing is directly diminished by the reading's core claim.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Businesses, investors, and contracting parties who rely on stable, judicially settled constitutional doctrine (commerce clause scope, property protections, contract enforcement) to plan long-horizon investment. When constitutional meaning is understood as perpetually contestable through political movements rather than fixed by precedent, their planning horizon shortens and litigation/political risk rises even where no formal doctrinal change has yet occurred.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, settlement_dependent_commercial_actors, payer,
    powerful, biographical, constrained, national).

% Groups whose rights (racial minorities, religious minorities, criminal defendants, unpopular speakers) have historically depended on courts resisting majoritarian political pressure. Popular constitutionalism's premise that meaning shifts through democratic contestation threatens the very insulation from majority will that judicial review was designed to provide for them; they cannot organize a comparably powerful political movement in the near term and have no exit from the jurisdiction whose constitutional meaning is being contested.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_protected_minorities, payer,
    powerless, biographical, trapped, national).

% Study and debate whether constitutional meaning is best understood as generated through political struggle (this reading), evolving judicial reasoning (living constitution), or fixed original meaning (originalism). They document historical episodes — Reconstruction, the New Deal settlement, the civil rights era — as evidence for and against the popular constitutionalism thesis without themselves holding formal power to resolve the kernel dispute.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which sustained, broad-based political mobilization can register durable constitutional change without requiring formal Article V amendment or judicial re-interpretation first — coordinating mass political action into recognized constitutional authority.
% TRANSFER_FUNCTION: Moves interpretive authority and the practical benefits of prevailing constitutional meaning from courts and judicially-protected settled expectations toward whichever political coalition can sustain the largest and most durable movement, at the expense of groups whose protections depend on interpretive stability insulated from majority politics.
% ABSENT_VOICES: Counter-majoritarian minorities without the numbers or resources to mount a comparable political movement are structurally disadvantaged by a framework that measures constitutional legitimacy partly by mobilization capacity; they are rarely centered in popular-constitutionalist scholarship, which tends to valorize successful past movements (civil rights) while saying less about groups a hypothetical hostile majority movement could target next.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism's claim to interpretive legitimacy vanished and courts alone were treated as final constitutional arbiters, social movements would lose one of their strongest legitimating frames for extra-judicial constitutional claims-making, legislative majorities pursuing contested constitutional visions would face stronger judicial-supremacy pushback, and minorities dependent on stable doctrine would gain a stronger (if double-edged) shield — the practical politics of constitutional change would reorganize around litigation strategy rather than movement-building.
% FOUNDING_PROBLEM: Historically, formal Article V amendment and judicial doctrine alone failed to register major shifts in constitutional understanding that mass political movements had already substantively won (Reconstruction's incomplete realization, the New Deal's constitutional revolution preceding doctrinal catch-up, the civil rights movement's political victories anticipating and pressuring judicial change) — the reading was built to explain and legitimate this observed gap between formal process and actual constitutional change.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the popular-constitutionalism scholarly camp (including originalist and process-theory scholars who are not beneficiaries of this reading) corroborate that the historical pattern of political-movement-driven constitutional change is real, while disputing whether it should be treated as a normatively legitimate mode of constitutional lawmaking or merely as an empirical description of how doctrine sometimes gets pressured to catch up to politics; courts themselves have never formally endorsed the reading as displacing judicial finality.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.52 at interval end) — the reading does not extract resources in a direct material sense, but it redistributes interpretive authority and its downstream practical benefits from courts and settlement-dependent parties toward whichever coalition can sustain mobilization, which functions as a genuine transfer over time. Suppression is moderate (0.44): the reading does not physically coerce dissent, but its normalization requires ongoing delegitimation of judicial-supremacy claims and requires courts, in practice, to accommodate or yield to sustained political pressure campaigns (court-packing threats, jurisdiction-stripping proposals, confirmation-battle escalation) that function as real constraint pressure on judicial independence. Theater ratio is moderate (0.38) reflecting that some invocations of 'the people's constitutional voice' are genuine mobilization and some are rhetorical cover for ordinary partisan preference. Accessibility collapse is comparatively low (0.35) because judicial, legislative, and movement-based avenues for constitutional argument all remain nominally open — the reading does not foreclose alternatives so much as contest which channel deserves primacy. Resistance is high (0.68): judicial-finality advocates, legal formalists, and rule-of-law institutionalists actively resist the reading's normalization precisely because it threatens settled professional and doctrinal authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements and legislative majorities sit near the beneficiary end: the reading directly legitimates their preferred mode of constitutional action and lowers the reputational and institutional cost of contesting judicial doctrine outside courts. Judicial finality advocates and settlement-dependent commercial actors sit near the target end: their institutional and economic interests depend on the opposite premise (judicial finality, doctrinal stability) that this reading contests. Counter-majoritarian minorities are the sharpest target case — trapped exit options, powerless political position, and a direct structural stake in the counter-majoritarian judicial protection this reading treats as provisional rather than fixed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the empirical gap between formal amendment/doctrine and actual constitutional change driven by mass movements — remains genuinely contested rather than resolved or dead, which is why this is authored as tangled_rope rather than snare: there is a real coordination function (recognizing and channeling durable political consensus into constitutional practice) alongside real asymmetric cost imposed on parties who depend on the counter-majoritarian, anti-political function of judicial review. Treating this reading as either pure coordination (ignoring the cost to minorities and settlement-dependent actors) or pure extraction (ignoring the genuine historical pattern of movement-driven constitutional change) would mislabel the structure; the tangled_rope classification holds both truths in view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    movement_legitimacy_vs_majoritarian_capture,
    'Is popular constitutionalism a genuine democratic corrective to judicial overreach and elite capture of constitutional meaning, or a mechanism by which transient majoritarian coalitions can erode counter-majoritarian protections that constitutional design specifically intended to insulate from ordinary politics?',
    'Comparative historical analysis of episodes where popular constitutionalist claims were vindicated (civil rights movement, New Deal) versus episodes where similar rhetoric was deployed to justify rights retrenchment (post-Reconstruction redemption movements, contemporary efforts to relitigate settled equal protection doctrine); track whether the framework''s normative force is symmetric across both cases or selectively invoked only for outcomes the invoking scholar favors.',
    'If the reading''s legitimating force is asymmetric — celebrated when it expands rights, condemned when it contracts them — it functions less as a neutral theory of constitutional authority and more as post-hoc justification for whichever outcome a given movement achieves, which would push the classification toward snare for the retrenchment use-case while leaving the expansion use-case closer to tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_legitimacy_vs_majoritarian_capture, conceptual, 'Whether popular constitutionalism is a symmetric theory of authority or an asymmetrically-invoked legitimation device.').

omega_variable(
    kernel_framing_committer_choice,
    'This story frames the kernel as a contest between judicial-supremacy and politically-generated meaning. An alternative framing treats the kernel as a contest over WHO counts as ''the people'' whose movements confer legitimacy — a question this reading''s own axioms leave underspecified. Does the choice of framing (institutional-authority contest vs. constituency-legitimacy contest) change which classification applies?',
    'Author a further-decomposed sibling story isolating the constituency-legitimacy question (which movements count, and by what threshold of mobilization) as its own constraint, per the ε-invariance decomposition principle, and compare classifications.',
    'If the constituency-legitimacy question yields a starkly different beneficiary/victim structure than the institutional-authority framing used here, this indicates the ''popular constitutionalism'' label itself may conflate two structurally distinct claims requiring further decomposition beyond the three-reading kernel already declared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_committer_choice, conceptual, 'Whether the kernel framing chosen for this reading is the only defensible decomposition, or conceals a further sub-contest over constituency legitimacy.').

omega_variable(
    natural_vs_constructed_movement_authority,
    'Is the authority popular movements accrue under this reading a natural feature of democratic legitimacy that any constitutional order must eventually recognize, or is it a constructed doctrine advanced because it benefits specific political actors (movements, legislative majorities) who gain standing and leverage from its adoption?',
    'Examine whether the doctrine is invoked consistently across the ideological spectrum (by both left and right social movements with comparable mobilization capacity) or predominantly by whichever coalition currently lacks judicial sympathy, which would indicate constructed strategic use rather than a neutral descriptive theory.',
    'Consistent cross-ideological invocation would support treating the underlying democratic-legitimacy claim as closer to a structural feature of constitutional design; asymmetric strategic invocation would support treating this reading as a constructed doctrine serving the interests of whichever coalition currently benefits from contesting judicial authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_movement_authority, empirical, 'Whether the popular-constitutionalist claim to authority is invoked as neutral principle or strategic tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 60, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, living_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the kernel us_constitution_interpretive. originalist_reading fixes meaning at ratification and grounds authority in fidelity to framers' intent; living_constitution_reading treats meaning as evolving through reasoned judicial adaptation; this reading (popular_constitutionalism_reading) locates authority partly outside courts in sustained political mobilization. Each reading has independently authored ε, beneficiaries, victims, and classification per the ε-invariance principle — they are linked here via network edges, not merged into one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
