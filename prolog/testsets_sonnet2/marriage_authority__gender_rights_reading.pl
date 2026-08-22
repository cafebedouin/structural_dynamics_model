% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Personal Law Marriage Authority — Gender Rights Contest Reading
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story instantiates the gender-rights reading of the contested
 *   marriage-authority kernel: the standing arrangement under contest is the
 *   male-administered interpretive monopoly over community personal law as it
 *   currently produces unequal divorce, maintenance, and inheritance outcomes
 *   for women. Unlike the communal-autonomy or federalist-millet readings,
 *   which treat the pluralist personal law system itself as the legitimate
 *   object of defense, this reading treats specific gendered practices within
 *   that system as the extraction mechanism, and constitutional equality
 *   litigation as the corrective lever. The reading cross-cuts the
 *   communal/secular divide: it does not ask whether personal law pluralism
 *   should exist (secularist question) or whether it protects minorities from
 *   majoritarian domination (federalist-millet question); it asks whether, as
 *   currently administered, it extracts from women specifically. ε is
 *   authored high because the extraction here is concrete and material —
 *   unequal maintenance sums, unilateral divorce with no reciprocal right,
 *   disinherison — not diffuse cultural friction.
 *
 * KEY AGENTS:
 *   - women_within_patriarchal_personal_law: primary target (powerless/trapped) — bears concrete extraction through unequal divorce, maintenance, and inheritance outcomes
 *   - male_community_authorities: primary beneficiary/agenda-setter (institutional/arbitrage) — holds interpretive monopoly, faces no reciprocal constraint
 *   - women_rights_advocates: secondary beneficiary (organized/mobile) — gains standing and political capital from litigation success without bearing the underlying extraction themselves
 *   - constitutional_courts: agenda-setter and analytical observer (institutional/analytical) — adjudicates specific practices without displacing the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.81).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.72).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Personal Law Marriage Authority — Gender Rights Contest Reading").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'bb6761c9-df3b-4c3b-9fd6-f76f506b3b02').
narrative_ontology:cs_kernel_codification('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', distributed).
narrative_ontology:cs_authority_grounding('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', distributed).
narrative_ontology:cs_reading_relation('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', foundational, constitutional_equality_overrides_communal_interpretive_authority).
narrative_ontology:cs_axiom_status(constitutional_equality_overrides_communal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', constitutional_equality_overrides_communal_interpretive_authority, deontological).
narrative_ontology:cs_axiom('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', foundational, gendered_outcome_disparity_within_community_is_extraction_not_tradition).
narrative_ontology:cs_axiom_status(gendered_outcome_disparity_within_community_is_extraction_not_tradition, holdable).
narrative_ontology:cs_axiom_grounding('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', gendered_outcome_disparity_within_community_is_extraction_not_tradition, empirically_contingent).
narrative_ontology:cs_reference_frame('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', constitutional_equality_supremacy_over_personal_law).
narrative_ontology:cs_drift_state('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', post_triple_talaq_criminalization_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bb6761c9-df3b-4c3b-9fd6-f76f506b3b02', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, male_community_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, personal_law_board_leadership).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, communal_male_elders_and_clergy).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, constitutional_equality_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governed by community-administered marriage, divorce, maintenance, and inheritance rules that a male-led interpretive authority applies to them without their consent or representation in that authority. Unilateral divorce practices, unequal maintenance entitlements, and skewed inheritance shares fall on them specifically; exit from the marriage often means exit from community standing, housing, and social network simultaneously, so formal legal remedies exist on paper long before they exist in practice.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, national).

% Interpret and administer marriage, divorce, and maintenance doctrine for the community, adjudicate disputes, and certify religious compliance. Their interpretive monopoly is the mechanism through which gendered outcomes are produced and defended; they can selectively cite tradition to resist reform pressure while facing no equivalent constraint on their own family arrangements.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, male_community_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, male_community_authorities, beneficiary).

% Litigate test cases, mobilize public opinion, and press constitutional courts to strike down or narrow specific gendered practices (instant unilateral divorce, discriminatory maintenance ceilings, unequal succession shares). They gain standing, resources, and political capital as reform cases succeed; their exit options are unconstrained because they are not themselves subject to the personal law regime they challenge on others' behalf in the same coercive way.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Hear equality challenges to specific personal law practices and issue rulings that invalidate or reinterpret them under constitutional equality guarantees, without displacing the personal law system as a whole. Their intervention is practice-specific and incremental, generating a patchwork of struck-down provisions rather than a unified code.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, constitutional_courts, observer).

% Are not parties to the equality litigation but experience judicial rulings as external incursions into doctrinal authority they consider theirs to interpret. They mobilize politically against reform, framing it as state or feminist intrusion into religious self-governance, and are largely absent from the courtroom where the practices they administer are being adjudicated.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_male_elders_and_clergy, excluded,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, communal_male_elders_and_clergy, beneficiary).

% Watches judicial rulings accumulate practice-by-practice reform without itself legislating a uniform code, largely to avoid the political cost of appearing to override communal religious authority. Retains formal power to codify or generalize the court's holdings but declines to exercise it.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, state_legislature, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constitutional courts coordinate a floor of equal treatment across otherwise fragmented personal law regimes by adjudicating specific practices against equality guarantees, allowing incremental correction without requiring legislative consensus on a uniform code.
% TRANSFER_FUNCTION: Moves interpretive and material authority — over divorce validity, maintenance entitlement, and inheritance share — away from male-administered communal authorities and toward women litigants and the constitutional equality framework, practice by practice rather than systemically.
% ABSENT_VOICES: Communal male elders and clergy who administer the doctrine are structurally absent from the equality litigation itself; they experience adverse rulings as external impositions but are not represented as parties, only as the target of subsequent political mobilization. Women who do not seek litigation — the majority still living under the unreformed practices — are also largely unheard except through advocacy organizations claiming to speak for them.
% DISAPPEARANCE_RATIONALE: If judicial equality review of personal law practices vanished, unilateral and unequal divorce, maintenance, and inheritance practices would revert to unreviewed communal administration; women currently protected by struck-down provisions (e.g., criminalized instant divorce) would lose that protection immediately, and the incremental reform track that substitutes for a uniform code would disappear entirely.
% FOUNDING_PROBLEM: Formally equal constitutional citizenship coexisted with personal law regimes that produced systematically unequal marriage, divorce, maintenance, and inheritance outcomes for women within specific religious communities, with no legislative appetite for a uniform code to resolve the contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholarship and comparative family law researchers outside both the advocacy organizations and the personal law boards document continuing gendered outcome disparities in maintenance awards, divorce validity contests, and inheritance litigation; national human rights commission reports and international treaty-body reviews (CEDAW committee observations) corroborate that the underlying disparity persists notwithstanding case-by-case judicial correction.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.81 at interval end) reflects the material transfer embedded in specific practices — unilateral divorce without reciprocal maintenance obligation, skewed inheritance shares — which is concrete and quantifiable, not merely symbolic. Suppression (0.72) is high because exit from an adverse personal law ruling frequently means exit from communal standing, housing networks, and family relationships simultaneously — the coercion is social and economic, not merely legal. Theater ratio is comparatively low (0.28) because the litigation and rulings track real material outcomes rather than performing reform without substance, though the ratio rises over the measured interval as the reform track increasingly substitutes visible case wins for the harder, undelivered legislative generalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Women within patriarchal personal law are declared victims and sit near the full-target end of directionality: trapped exit options, powerless structural position, and the practices adjudicated (divorce validity, maintenance, inheritance) extract from them specifically and materially. Male community authorities are the structural beneficiary — institutional power, arbitrage-grade exit (they are not bound by the outcomes they administer for others), and they retain interpretive discretion even after adverse rulings. Women's rights advocates are beneficiaries of a different kind: they collect political capital and legal standing from reform litigation without being subject to the underlying extraction in the same coercive way, which is why their exit options are authored as mobile rather than trapped — this is a structural distinction from the class they represent, not a claim about their motives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constitutional equality guarantees coexisting with unequal personal law outcomes — remains live by outside corroboration (CEDAW committee observations, independent family law scholarship), which rules against classifying this as a resolved-mandate piton. But the reform mechanism itself (case-by-case judicial review without legislative generalization) risks becoming its own scaffold that never sunsets: each favorable ruling addresses one practice while leaving the interpretive monopoly and the underlying asymmetry intact for the next contested practice. The gender-rights reading does not claim the personal law system overall is illegitimate (that is the secularist reading's claim); it claims specific practices within it are extractive and correctable through the existing constitutional equality doctrine, which is why victims are named narrowly and beneficiaries include the reform apparatus itself alongside the extractive authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope_boundary,
    'This story is the gender-rights reading of the marriage_authority kernel — it targets specific extractive practices (unilateral divorce, unequal maintenance, discriminatory inheritance) rather than personal law pluralism as a system. Sibling readings (communal_autonomy_reading, federalist_millet_reading, secularist_reading, judicial_harmonization_reading) evaluate the same underlying arrangement by different lights: communal autonomy defends the interpretive monopoly itself as legitimate religious self-governance; federalist-millet defends legal pluralism as anti-majoritarian protection; secularist treats personal law pluralism as an anomaly awaiting a uniform code; judicial harmonization treats the same case-by-case court rulings as constitutional-floor-building rather than gender-specific correction. Where is the disagreement located structurally?',
    'Compare each reading''s beneficiary/victim structure and claimed_type directly: the communal-autonomy reading would authorize the same male community authorities as legitimate stewards rather than beneficiaries of extraction; the federalist-millet reading would treat the fragmentation itself (not specific practices) as the coordination function, with no per-practice victim; the secularist reading would name the personal law system''s existence as the extraction target rather than specific gendered practices within it; judicial harmonization would treat the courts'' incremental doctrine-building as the coordination function with legal uncertainty (not gender) as its cost.',
    'If the sibling readings are adopted instead of this one, women_within_patriarchal_personal_law would not appear as a distinct victim class at all in the communal-autonomy or federalist-millet readings — the disagreement is not about facts but about which arrangement is the referent of ε and who counts as party to it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope_boundary, conceptual, 'This story instantiates one reading (gender_rights_reading) of the contested marriage_authority kernel; the disagreement with sibling readings is located in what the referent arrangement is and who is named as victim/beneficiary, not in factual dispute over practices.').

omega_variable(
    litigation_beneficiary_capture_risk,
    'Do women''s rights advocacy organizations that litigate personal-law equality cases risk becoming a beneficiary class whose institutional interest (continued high-profile litigation, funding, standing) diverges from the interest of the women they represent (durable, generalized reform rather than incremental case wins)?',
    'Track whether advocacy organizations pursue legislative codification of won principles (which would end the need for repeat litigation) versus continuing to litigate practice-by-practice; a persistent preference for litigation over codification, holding political feasibility constant, would support capture.',
    'If capture is present, the gain_flow declaration (women_rights_advocates) should be read as partially diverging from the stated beneficiary class (women_within_patriarchal_personal_law as ultimate intended beneficiaries of reform) — the story would need a secondary victim/beneficiary distinction between represented women and the advocacy apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(litigation_beneficiary_capture_risk, empirical, 'Whether advocacy-driven litigation as reform mechanism has its own institutional interest that can diverge from the interest of the population it represents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__gender_rights_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__gender_rights_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__gender_rights_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__gender_rights_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(marr_be_t8, marriage_authority__gender_rights_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(marr_be_t16, marriage_authority__gender_rights_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(marr_be_t24, marriage_authority__gender_rights_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(marr_be_t32, marriage_authority__gender_rights_reading, base_extractiveness, 32, 0.79).
narrative_ontology:measurement(marr_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t8, marriage_authority__gender_rights_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(marr_su_t16, marriage_authority__gender_rights_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(marr_su_t24, marriage_authority__gender_rights_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(marr_su_t32, marriage_authority__gender_rights_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(marr_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the marriage_authority kernel. Each reading names a different referent arrangement and produces a different ε and beneficiary/victim structure from the same underlying dispute over who has authority to define marriage, divorce, maintenance, and inheritance within personal law systems. The gender_rights_reading is distinguished from the others by targeting specific extractive practices rather than the system-level structure (secularist, federalist_millet) or the interpretive authority itself (communal_autonomy), and by naming a concrete material victim class (women_within_patriarchal_personal_law) that the other readings either do not name or name differently (judicial_harmonization treats the same court rulings as constitutional-floor-building with legal uncertainty, not gender, as the primary cost axis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
