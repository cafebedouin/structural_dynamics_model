% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Declaration of Independence as Universal Equality Principle Requiring Iterative Expansion
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The universalist reading treats 'all men are created equal' as a
 *   textually universal principle that obligates iterative constitutional and
 *   legal expansion to include historically excluded populations, regardless
 *   of the founders' intent or the categories they recognized. Under this
 *   reading, exclusion of any group from equal status becomes an
 *   inconsistency the principle itself demands correcting. The universalist
 *   reading is neither the originalist reading (which bounds equality by
 *   founding-era taxonomy) nor the textualist paradox reading (which treats
 *   the contradiction between universal language and restricted application
 *   as performative incoherence). This constraint story instantiates the
 *   universalist reading alone: ε is moderate (coordination costs of
 *   continuous expansion; institutional resistance to reinterpreting
 *   boundaries) and benefits are concentrated among historically excluded
 *   groups claiming inclusion. The constraint is Tangled Rope: genuine
 *   coordination function (the principle coordinates expansion mechanisms)
 *   AND asymmetric extraction (those defending restricted categories bear the
 *   cost of constant reinterpretation demands).
 *
 * KEY AGENTS:
 *   - historically_excluded_groups: the beneficiary party; identity-locked into the constraint (exit means accepting exclusion as just)
 *   - egalitarian_reform_movements: the agenda-setter; organized power pressing for expansion
 *   - originalist_jurisprudence_defenders: institutional power bearing the cost (their interpretive framework is directly contested)
 *   - power_incumbents_resisting_expansion: powerful actors bearing extractive costs (hierarchies are delegitimized by universal principle)
 *   - constitutional_courts: mediating institutional seat; applies the principle to new cases, determining expansion timing and scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.48).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.42).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Declaration of Independence as Universal Equality Principle Requiring Iterative Expansion").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'bb4fbd98-150b-42f4-b79c-86a3c09aa7fe').
narrative_ontology:cs_kernel_codification('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', fixed_text).
narrative_ontology:cs_authority_grounding('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', extraction).
narrative_ontology:cs_interpretation_layer_present('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe').
narrative_ontology:cs_reading_relation('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', foundational, universal_language_binds_scope).
narrative_ontology:cs_axiom_status(universal_language_binds_scope, holdable).
narrative_ontology:cs_axiom_grounding('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', universal_language_binds_scope, deontological).
narrative_ontology:cs_axiom('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', foundational, systematic_exclusion_violates_principle).
narrative_ontology:cs_axiom_status(systematic_exclusion_violates_principle, holdable).
narrative_ontology:cs_axiom_grounding('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', systematic_exclusion_violates_principle, deontological).
narrative_ontology:cs_axiom('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', secondary, expansion_required_by_text_not_intent).
narrative_ontology:cs_axiom_status(expansion_required_by_text_not_intent, holdable).
narrative_ontology:cs_axiom_grounding('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', expansion_required_by_text_not_intent, deontological).
narrative_ontology:cs_reference_frame('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', universal_equality_as_textual_obligation).
narrative_ontology:cs_drift_state('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb4fbd98-150b-42f4-b79c-86a3c09aa7fe', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, egalitarian_reform_movements).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, power_incumbents_resisting_expansion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, originalist_jurisprudence_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women, enslaved and formerly enslaved persons, racial minorities, Indigenous peoples, non-propertied people, LGBTQ+ persons, and contemporary populations claiming equal status. Under the universalist reading, the Declaration's universal language grounds their legal and moral standing to press inclusion. Their exit from this constraint means accepting exclusion from equal-status recognition as just—a position that fuses with their identity. They depend entirely on the universalist reading's institutional authority to make their claims cognizable.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, historically_excluded_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Organized coalitions (abolitionist, women's suffrage, civil rights, LGBTQ+ rights, contemporary social justice movements) that invoke the Declaration's universal language to mobilize political pressure for inclusion. They set the agenda by naming new groups and demanding constitutional recognition of their equal status. They benefit from a universalist reading that makes their claims textually grounded, but remain constrained by institutions that must be changed.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, egalitarian_reform_movements, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, egalitarian_reform_movements, beneficiary).

% Conservative constitutional scholars, jurists (particularly Supreme Court justices and appellate judges), and institutional actors committed to interpreting the Constitution and Declaration according to original public meaning. They bear the cost of the universalist reading: their interpretive authority is subordinated to the principle's universal language, their reliance on founding intent is treated as secondary, and they must continuously defend bounded interpretations against expansion claims grounded in the same text.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_jurisprudence_defenders, payer,
    institutional, generational, constrained, national).

% Beneficiaries of existing hierarchies (historical slaveholders and slavery's successors, male-supremacist institutional orders, racial hierarchies, gender categories that restrict opportunity, wealth-based stratification). The universalist reading subjects their hierarchies to constant normative and legal challenge: every exclusion becomes an inconsistency the principle obligates correcting. Their exit would require abandoning hierarchical benefits; their constraint is to defend existing orderings against expansion claims.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, power_incumbents_resisting_expansion, payer,
    powerful, biographical, constrained, national).

% Federal and state judiciaries, particularly the Supreme Court, that adjudicate which populations fall within the equal-status principle's scope. Courts receive expansion claims from reform movements and originalist defenses, then decide whether new populations (women, racial minorities, LGBTQ+ persons, immigrants, disability categories) count as within the principle's referent. Their decisions effectively operationalize the principle's expansion or contraction.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Academic constitutional theorists who focus on textual interpretation and the logical coherence of universal language applied to restricted practice. They analyze the founding document's literal meaning and highlight the performative contradiction between universal assertion and restricted application—sometimes supporting universalist expansion, sometimes emphasizing the paradox as unresolvable.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_scholars_textualist_school, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, historically_excluded_groups).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The universalist reading coordinates a self-correcting mechanism for identifying and remedying systematic exclusions from equal status: by treating equality as textually universal, it empowers excluded populations to invoke the principle directly and compels institutional response. This solves the coordination problem of 'how do we correct systematic exclusion without constitutional amendment'—answer: by interpreting the universal language as binding.
% TRANSFER_FUNCTION: Moves interpretive authority from founding-intent-bounded readings toward universal-language-binding readings. It transfers legal standing from institutional gatekeepers (those defending original categories) to claimants for inclusion (historically excluded groups). It moves the burden of proof: those defending exclusion must now justify why universal language should not apply to their case.
% ABSENT_VOICES: Originalist jurists and constitutional defenders are partly marginalized in this reading's own framework—their interpretive claims are treated as secondary to textual universality. Historical beneficiaries of categorical exclusion are structurally excluded by the reading's operative logic. Future populations not yet recognized as categories (persons-not-yet-born, identity categories not yet named) are implicitly excluded from the current conversation about equal status.
% DISAPPEARANCE_RATIONALE: If the universalist reading disappeared, leaving only originalist or textualist readings, constitutional law would stabilize at the founders' categorical scheme. Women would lack constitutional grounds for suffrage beyond what the founders intended, racial slavery would have textual constitutional warrant, property restrictions on political participation would be textually defensible, and the principle would carry no obligation to expand. Reform movements would lose their most powerful legal lever. The institutional and social order would rearrange into rigid founding-era categories without a built-in mechanism for correcting systematic exclusion.
% FOUNDING_PROBLEM: The Declaration asserts universal equality in principle ('all men are created equal') but excludes vast populations in practice (enslaved persons, women, non-propertied, Indigenous peoples). This creates a performative contradiction: the principle's universal language logically obligates inclusion, but historical practice and founding intent restricted application. The universalist reading addresses this by asserting that the universal language IS the binding obligation, and that systematic exclusion is an error to be corrected through iterative expansion.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from three non-founder sources: (1) Historical reform movements (abolitionist, women's suffrage, civil rights) who explicitly grounded their legal claims in the Declaration's universal language, treating exclusion as an inconsistency demanding correction. These movements succeeded in expanding the constitutional order precisely by invoking the universal principle against restrictive practice. (2) Judicial decisions from Warren Court onward (Brown v. Board, Loving v. Virginia, Obergefell v. Hodges) have repeatedly recognized new equality claims by invoking the principle's universal scope rather than founding-era categories. (3) Contemporary constitutional scholars outside the originalist school (Akhil Amar, Cass Sunstein, Jack Balkin) have documented how the founding problem—universal language + restrictive practice—is the permanent structural feature that obligates expansion. By contrast, originalist corroboration (intent-bounded readings) originates from the same institutional gatekeepers whose interpretive authority the reading subordinates. The absence of originalist corroboration from outside the defending institution is structural: originalists must defend bounded readings against the text's surface meaning, a harder case.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48 at interval end) reflects moderate extraction: the universalist reading imposes costs on originalist interpreters and power incumbents (requires continuous reinterpretation, delegitimizes existing hierarchies), but does not extract material rents—it redistributes interpretive authority and legal standing. Suppression requirement declines steeply over the interval (0.85→0.42) because the principle accumulates legitimacy through successful expansions (abolitionism, suffrage, civil rights), reducing the suppressive force required to maintain it—fewer people need to be forcibly excluded from invoking equal status as the principle normalizes. Theater ratio rises modestly (0→0.28) because contemporary invocations of equality increasingly include performative elements (symbolic commitments to diversity, rhetorical inclusivity alongside material stratification), though the functional core (legal standing for excluded groups to claim inclusion) remains substantial. Accessibility collapse is moderate-high (0.65) because once the universalist principle is articulated, alternatives (purely originalist readings, founding-intent-bounded equality) become cognitively difficult to maintain in the face of the text's universal language—the principle structures perception of what equality means. Resistance is substantial (0.58) because originalist and textualist jurists actively contest the universalist reading, producing steady counterarguments and institutional barriers; this is not a one-sided consensus but a live dispute.
 *
 * PERSPECTIVAL GAP:
 *   The most significant divergence is between the originalist-defender seat and the historically-excluded-groups seat. From the originalist perspective, the constraint imposes cost by requiring continuous reinterpretation of a stable founding document, subordinating historical meaning to present-day expansion demands—they experience extraction of their interpretive authority. From the excluded-groups perspective, the constraint is the ONLY mechanism by which they can claim standing; it does not extract from them but includes them where they would otherwise be permanently denied. Constitutional courts sit in the middle, adjudicating which new groups fall within the principle's scope—from their position the constraint coordinates expansion but requires ongoing institutional labor. The engine should compute d_universalist ~0.7-0.9 for historically excluded groups (near-full beneficiary), d_originalist_defenders ~0.3-0.5 (partial target of reinterpretation demands), and d_courts ~0.45-0.55 (symmetric coordination/enforcement burden).
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups are near-full beneficiaries (d→0.0): they collect standing, legal leverage, moral ground for pressing inclusion claims. Egalitarian movements are partial beneficiaries (d→0.2-0.3): they benefit from the principle but also carry the cost of continuous organizing and institutional struggle. Originalist jurisprudence defenders are partial targets (d→0.4-0.5): they bear the cost of constant reinterpretation demands on their interpretive framework, but retain significant institutional power. Power incumbents resisting expansion are near-full targets (d→0.8-0.9): they bear the cost of delegitimization and the threat of hierarchy collapse, with almost no countervailing benefit. Constitutional courts are symmetric (d→0.5): they coordinate the expansion mechanism but also enforce it, balancing the demands of new claimants against originalist resistance. These directionalities are derived from the structural beneficiary/victim declarations (historically_excluded_groups as beneficiary, originalist_defenders and power_incumbents as victims) modulated by exit options: historically excluded groups are identity_locked (the constraint IS their mechanism for inclusion—exit means accepting exclusion), while originalists are constrained (they can resist but cannot opt out of the constitutional landscape), and power incumbents are constrained (hierarchy collapse is enforced by courts).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (logical incoherence between universal assertion and restricted application) remains LIVE at interval end (2026), preventing mandatrophy resolution. The universalist reading does not claim the problem is solved—it claims the principle obligates solving it through iterative expansion. New groups (LGBTQ+ persons, non-citizens, documented immigrants, disability categories) continue to press inclusion claims grounded in the principle's universal language. The constraint will not resolve to mandatrophy until either: (A) all humanly identifiable populations are formally included in equal-status recognition (unlikely end-state), (B) the universalist reading loses institutional authority to originalist or textualist readings (triggering a different constraint as the live one), or (C) the founding problem is explicitly redefined as resolved by authoritative restatement (constitutional amendment). Current trajectory: the principle accumulates expansions (women, racial minorities, LGBTQ+ persons, disability protections) while the underlying incoherence persists (undocumented immigrants, non-citizen residents, future unforeseen groups remain formally outside or partially outside). Mandatrophy is deferred indefinitely by the reading's own logic: universalist interpretation obligates perpetual expansion, not resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_vs_boundedness_ambiguity,
    'Is the Declaration''s universal language (''all men'') a binding statement of principle that obligates iterative expansion, or a rhetorical assertion constrained by founding-era categorization?',
    'This is fundamentally a question about constitutional interpretation methodology—which trumps: text or intent? Different constitutional traditions (living constitutionalism vs. originalism) produce different answers. Resolution would require either a formal supersession of one methodology by constitutional amendment or a shift in which interpretive school commands institutional authority.',
    'If universality is binding, the constraint persists indefinitely with expanding scope; if boundedness by intent is binding, the constraint resolves to a fixed, 18th-century definition of equality. This is the deepest structural ambiguity of the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_vs_boundedness_ambiguity, conceptual, 'Whether equality''s universal language or its founding-era scope is the binding constraint.').

omega_variable(
    expansion_mechanism_endogenous_vs_exogenous,
    'Does the universalist reading''s expansion mechanism derive FROM the principle itself (the text obligates expansion) or is expansion driven by EXTERNAL political and social movements invoking the principle as rhetorical lever?',
    'Historical analysis of expansion moments (abolitionism, suffrage, civil rights, LGBTQ+ equality): in each case, did the principle compel expansion or did movements compel courts to invoke the principle? Likely finding: both are always simultaneously true, making the distinction empirically difficult to maintain.',
    'If expansion is endogenous to the principle, the constraint is genuinely coordinating (the principle self-expands); if exogenous, the principle is a weapon wielded by external actors. The distinction affects whether extractiveness should be classified as coordination cost (endogenous) or as extraction by reform movements (exogenous).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_mechanism_endogenous_vs_exogenous, empirical, 'Whether expansion is driven by the principle''s internal logic or external political movements.').

omega_variable(
    exclusion_mechanism_inherent_vs_contingent,
    'Are the historical exclusions from equal status (women, enslaved persons, racial minorities, non-propertied) inherently required by the 18th-century understanding of ''men,'' or were they contingent exclusions that violated the principle even at the time?',
    'Originalist and textualist scholarship on founding-era meaning vs. contemporary philosophical analysis of the logical coherence of categorical exclusion from a universalist principle. Likely finding: founding-era actual meaning was exclusionary, but the principle''s logical structure is universalist, creating permanent tension.',
    'If exclusions are inherent to founding meaning, the originalist reading is more defensible and the universalist reading is more extractive (it imposes costs to correct non-errors). If exclusions are contingent violations, the universalist reading is coordinating (correcting a founding-era error), not extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_inherent_vs_contingent, conceptual, 'Whether historical exclusions were inherent to founding meaning or contingent violations of it.').

omega_variable(
    court_role_as_gatekeeper_vs_instrument,
    'Do constitutional courts exercise independent gatekeeping authority over which expansion claims count as applications of the universal principle, or do they serve as institutional instruments for movements pressing inclusion?',
    'Comparative study of judicial decision-patterns: do courts reject some inclusion claims as inconsistent with the principle, or do they systematically uphold all expansion claims grounded in equal status? Pattern analysis would show whether gatekeeping is real or rhetorical.',
    'If gatekeeping is real, courts are more neutral arbiters and the constraint is genuinely coordinating; if instrumental, courts are captured by reform movements and the constraint is more extractive against originalist resisters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_role_as_gatekeeper_vs_instrument, empirical, 'Whether courts independently gate expansion or serve as institutional conduits for movement demands.').

omega_variable(
    identity_lock_fragility_for_excluded_groups,
    'How durable is the identity-lock binding historically excluded groups to the universalist reading? If an originalist or textualist reading became institutionally dominant, would excluded groups'' exit options expand or would the lock persist?',
    'Counterfactual analysis: if the Supreme Court formally adopted originalist interpretation and rejected universalist expansion, would historically marginalized groups accept the constraint''s redefinition, or would they resist and continue pressing claims? Historical evidence from periods when originalism held more institutional power.',
    'If the lock is durable even under unfavorable interpretation, excluded groups remain trapped in a constraint that constrains them; if the lock is fragile and they would resist, the constraint''s persistence depends on continued universalist institutional authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_fragility_for_excluded_groups, empirical, 'Whether the identity-lock binding excluded groups to the universalist reading survives institutional shifts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.0).
narrative_ontology:measurement(all__tr_t1850, all_men_created_equal__universalist_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(all__tr_t1960, all_men_created_equal__universalist_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__universalist_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__universalist_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all__be_t1850, all_men_created_equal__universalist_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(all__be_t1960, all_men_created_equal__universalist_reading, base_extractiveness, 1960, 0.46).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__universalist_reading, base_extractiveness, 2000, 0.47).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__universalist_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.85).
narrative_ontology:measurement(all__su_t1850, all_men_created_equal__universalist_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(all__su_t1960, all_men_created_equal__universalist_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__universalist_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(all__su_t2026, all_men_created_equal__universalist_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, equal_protection_doctrine__expansionist_jurisprudence).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, voting_rights_framework__universal_suffrage_grounding).

% DUAL FORMULATION NOTE:
% The 'all men are created equal' kernel decomposes into three structurally distinct constraints: (1) originalist_reading: equality bounded by founding-era taxonomy, negligible extraction from originalists' seat, high extraction from excluded groups; (2) textualist_paradox_reading: contradiction between universal language and restricted application, medium extraction across all seats due to interpretive incoherence; (3) universalist_reading (this story): equality as universal principle requiring expansion, moderate extraction from originalists and power incumbents, benefits concentrated in historically excluded groups. These are not measurements of the same constraint from different angles—they have different ε values, different beneficiary/victim structures, different causal mechanisms. Each is a separate story linked by kernel affect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
