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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Patriarchal Personal Law Practices as Gender-Extractive Marriage Authority
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story authors the gender-rights reading of the marriage-authority
 *   kernel: the personal law arrangement is read as a set of specific,
 *   judicially contestable practices — unilateral divorce, unequal
 *   maintenance, unequal inheritance — that extract economic security and
 *   autonomy from women within the community while communal male authorities
 *   and conservative boards retain interpretive control over marriage exit.
 *   The reading cross-cuts the communal/secular divide: it does not ask
 *   whether personal law pluralism itself should survive (that is the
 *   secularist and federalist-millet readings' fight) but targets specific
 *   practices for constitutional equality scrutiny, which is why litigation
 *   proceeds practice-by-practice rather than via comprehensive code
 *   replacement. Per DP-001, ε here (0.79) is authored for the standing
 *   arrangement of specific gender-unequal practices as this reading sees
 *   them — high extraction, high suppression via communal exit-cost — not for
 *   the equal-rights alternative this reading advocates, which would register
 *   near-zero extraction under its own lights.
 *
 * KEY AGENTS:
 *   - women_within_patriarchal_personal_law: primary target (powerless/trapped) — bears the maintenance, inheritance, and divorce-procedure extraction
 *   - male_communal_authorities: primary beneficiary and agenda setter (institutional/arbitrage) — administers and retains discretionary interpretive power
 *   - conservative_religious_boards: institutional beneficiary (organized/mobile) — derives authority and funding from interpretive monopoly
 *   - women_rights_advocates: reading-aligned beneficiary (organized/constrained) — gains precedent and standing through litigation but lacks standing legislative power
 *   - constitutional_courts: analytical adjudicator (institutional/analytical) — resolves practice-by-practice, does not restructure system
 *   - moderate_community_members: excluded voice (moderate/constrained) — privately favor reform but unrepresented in either contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.79).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Patriarchal Personal Law Practices as Gender-Extractive Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'b7b83ba4-d5a1-4210-99bb-6f79f281d748').
narrative_ontology:cs_kernel_codification('b7b83ba4-d5a1-4210-99bb-6f79f281d748', distributed).
narrative_ontology:cs_authority_grounding('b7b83ba4-d5a1-4210-99bb-6f79f281d748', practice).
narrative_ontology:cs_interpretation_layer_present('b7b83ba4-d5a1-4210-99bb-6f79f281d748').
narrative_ontology:cs_reading_relation('b7b83ba4-d5a1-4210-99bb-6f79f281d748', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('b7b83ba4-d5a1-4210-99bb-6f79f281d748', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7b83ba4-d5a1-4210-99bb-6f79f281d748', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('b7b83ba4-d5a1-4210-99bb-6f79f281d748', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('b7b83ba4-d5a1-4210-99bb-6f79f281d748', foundational, intra_community_equality_overrides_communal_deference).
narrative_ontology:cs_axiom_status(intra_community_equality_overrides_communal_deference, holdable).
narrative_ontology:cs_axiom_grounding('b7b83ba4-d5a1-4210-99bb-6f79f281d748', intra_community_equality_overrides_communal_deference, deontological).
narrative_ontology:cs_axiom('b7b83ba4-d5a1-4210-99bb-6f79f281d748', secondary, practice_level_reform_preserves_pluralist_structure).
narrative_ontology:cs_axiom_status(practice_level_reform_preserves_pluralist_structure, holdable).
narrative_ontology:cs_axiom_grounding('b7b83ba4-d5a1-4210-99bb-6f79f281d748', practice_level_reform_preserves_pluralist_structure, instrumental).
narrative_ontology:cs_reference_frame('b7b83ba4-d5a1-4210-99bb-6f79f281d748', communal_marital_self_governance).
narrative_ontology:cs_drift_state('b7b83ba4-d5a1-4210-99bb-6f79f281d748', post_triple_talaq_judgment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b7b83ba4-d5a1-4210-99bb-6f79f281d748', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, male_communal_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, conservative_religious_boards).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, constitutional_equality_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to unilateral divorce mechanisms, truncated maintenance rights, and unequal inheritance and property claims administered under community personal law. Exiting the community's religious jurisdiction means losing marital status recognition, custody standing, and social belonging simultaneously; approaching secular courts requires resources, literacy, and social risk-tolerance most do not have.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, national).

% Administer and interpret personal law codes (talaq procedure, mahr, maintenance, inheritance shares) through community boards and clergy. Frame these practices as religiously mandated and non-negotiable, resist codification or judicial override as external interference, and retain the discretionary power the current arrangement gives them over exit and remarriage terms for women in the community.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, male_communal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Institutional bodies (personal law boards, seminary networks) that derive authority, membership loyalty, and donor funding from being the recognized interpreters of marriage practice. A judicial or legislative equality intervention reduces their interpretive monopoly and the deference they are owed by the state and by community members.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, conservative_religious_boards, beneficiary,
    organized, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, conservative_religious_boards, agenda_setter).

% Litigators, women's collectives, and reform-minded community members who bring constitutional challenges against specific practices (triple talaq, unequal maintenance formulas, inheritance ratios). They gain legal precedent and public standing when courts strike down a practice, but remain structurally outside the community authority structure they are challenging and depend on case-by-case litigation rather than standing legislative power.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, women_rights_advocates, excluded).

% Adjudicate individual constitutional challenges to specific personal law practices, applying equality-guarantee reasoning practice-by-practice rather than issuing a comprehensive personal law code. Their rulings bind the specific practice struck down but do not restructure the underlying communal authority architecture.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Men and women within the community who privately support reform of specific inequitable practices but do not litigate or publicly organize, fearing communal backlash, social ostracism, or being cast as aligned with majoritarian or secularist agendas that could be used against the community as a whole. Their preferences do not register in either the communal-authority or the litigation contest.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, moderate_community_members, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, male_communal_authorities).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Personal law arrangements do coordinate something real: they provide a recognized, community-legible procedure for marriage, divorce, and inheritance that spares individuals from having to construct family status from scratch through secular courts, and they preserve a domain of communal self-governance in a religiously plural state.
% TRANSFER_FUNCTION: Specific practices — unilateral and unequal divorce procedure, truncated maintenance formulas, unequal inheritance shares — move economic security, custodial standing, and post-marital autonomy away from women within the community and toward the men and institutions who administer those practices, while enforcement of the community's interpretive authority is what keeps the transfer from being contestable inside the community's own forums.
% ABSENT_VOICES: Moderate community members who would support targeted reform without abandoning communal identity are structurally absent from both sides of the conflict: they are not represented in the boards that administer personal law, and they rarely appear as named litigants, since litigation is dominated by advocacy organizations and individual claimants willing to bear the social cost.
% DISAPPEARANCE_RATIONALE: If the specific extractive practices vanished overnight — replaced by equal maintenance, equal inheritance shares, and mutual/judicial divorce procedure — the economic and custodial position of women within these communities would materially improve, communal boards would lose a significant share of their interpretive leverage over marriage exit, and the reform litigation apparatus that currently exists to contest practice-by-practice would have far less to target.
% FOUNDING_PROBLEM: Personal law codes originally addressed the problem of religiously plural societies needing a recognized, community-administered mechanism for marriage and family status without forcing uniform secular family law on communities that saw such uniformity as majoritarian imposition or colonial administrative convenience.
% FOUNDING_PROBLEM_CORROBORATION: Communal authorities and their boards attest the arrangement still solves a live problem of religious self-governance. Independent legal scholarship, dissenting judicial opinions in triple-talaq and maintenance litigation, and cross-community women's rights organizations — parties outside both the conservative boards and the pure secularist camp — attest that whatever coordination problem personal law once solved, the specific gender-unequal practices at issue no longer track any live religious necessity and instead track retained administrative discretion; no source entirely outside advocacy or judicial contexts corroborates the boards' account of continued necessity.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.79, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.79) because the practices under contest — unilateral talaq, minimal or time-limited maintenance, unequal inheritance shares — transfer concrete economic and custodial value away from women with no reciprocal coordination benefit specific to them; the community-cohesion benefit these practices are said to protect accrues to the administering authorities, not to the women bearing the cost. Suppression (0.71) reflects that exit from the extractive practice typically requires exit from communal identity and social standing altogether — a compound cost, not a simple procedural one. Theater ratio is comparatively low (0.28) because the practices are functionally, not merely performatively, extractive — the maintenance and inheritance formulas do real material work of allocation, they are not empty ritual — though a rising theater component over the measured interval reflects boards increasingly defending practices as symbolic markers of religious identity even where the underlying substantive justification has weakened under litigation pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of male communal authorities and conservative boards, the arrangement reads as legitimate religious self-governance under threat from external constitutional overreach — a rope or tangled_rope by their own lights. From the seat of women within the community bearing unequal maintenance and inheritance outcomes, the same arrangement reads as a snare: coordination language covering a transfer they cannot contest inside the community's own forums and can only contest at high personal cost through external courts. The engine computes both seats from the same structural data; the divergence is expected and is exactly what this reading exists to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Women within patriarchal personal law are declared victims with trapped exit — d sits near the full-target end, since leaving the extractive practice typically requires leaving the community's recognized marital status entirely. Male communal authorities and conservative boards are declared beneficiaries with arbitrage-grade exit (they can shift interpretive strategy, forum, or framing in response to legal pressure without losing underlying authority) — d sits near the full-beneficiary end. Women's rights advocates are also declared beneficiaries (they gain precedent, funding, and standing from successful litigation) but their exit options are constrained, not arbitrage — they remain structurally exposed to backlash and dependent on case-by-case outcomes, which is why they are NOT given the same directionality treatment as the boards despite both being coded 'beneficiary.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a recognized communal mechanism for family status in a religiously plural state — has genuine continuing relevance for family-status recognition generally, which prevents this reading from mislabeling all personal law pluralism as pure extraction. But the specific practices this reading targets (unequal maintenance, unilateral divorce, unequal inheritance) do not track any live requirement of that founding problem; they persist because dismantling them threatens the discretionary power of the administering authorities, not because family-status recognition requires gender inequality. Classifying this reading as snare rather than tangled_rope is deliberate: the coordination function (family-status recognition) belongs to the personal-law-pluralism system as a whole, not to the specific gender-unequal practices this reading isolates — those practices carry no coordination benefit of their own that would qualify as the 'genuine function' half of a tangled rope; the beneficiary column (women_rights_advocates) reflects reform-side gain from litigation, not a coordination function performed by the extractive practices themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_necessity_vs_administrative_discretion,
    'Are the specific contested practices (unequal maintenance, unilateral divorce, unequal inheritance) genuinely required by the community''s religious doctrine, or are they administrative interpretations that could be reformed within the tradition without abandoning religious authority?',
    'Comparative theological and jurisprudential analysis across jurisdictions where the same religious tradition has adopted gender-equal reforms (e.g., reformed maintenance or inheritance rules in other national contexts) without a schism in doctrinal legitimacy; testimony from reformist scholars within the tradition versus incumbent board authorities.',
    'If the practices are genuinely doctrinally required, targeted judicial reform runs into a deeper church-state/religious-freedom conflict that changes the classification calculus (closer to tangled_rope, with genuine coordination-of-belief function on one side). If they are administrative discretion dressed as doctrine, the snare classification is strongly reinforced and the coordination story is closer to pure cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_necessity_vs_administrative_discretion, conceptual, 'Whether the contested practices are doctrinally necessary or administratively discretionary.').

omega_variable(
    litigation_reach_vs_communal_containment,
    'Does practice-by-practice constitutional litigation actually change outcomes for the broad population of women within patriarchal personal law, or does it produce visible precedent that benefits litigants and advocacy organizations while leaving most women''s day-to-day maintenance and inheritance outcomes unchanged because enforcement remains internal to community forums?',
    'Empirical tracking of post-judgment compliance rates and actual maintenance/inheritance outcomes for non-litigant women within the affected communities, compared to pre-judgment baselines.',
    'If compliance and outcomes shift broadly, the gender_rights_reading''s reform mechanism (litigation-driven equality) is validated as substantively effective, lowering theater_ratio over time. If compliance stays narrow to litigants, the theater_ratio for the reform apparatus itself should be authored higher, and the reading''s own beneficiary (women_rights_advocates) risks becoming closer to a symbolic-victory beneficiary than a substantive one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_reach_vs_communal_containment, empirical, 'Whether litigation victories translate into broad substantive change or remain narrow precedent.').

omega_variable(
    kernel_framing_under_determination,
    'Is the correct unit of analysis for this reading the individual contested practice (triple talaq, a specific maintenance formula) or the pattern of gendered discretion across all such practices considered as a single extractive structure?',
    'None fully resolves this — it is a framing choice. Comparing case outcomes where courts strike a single practice versus outcomes where courts articulate a general equality principle applicable across practices would show whether the narrower or broader framing better predicts subsequent legal and social change.',
    'The narrower framing (practice-by-practice) is what was chosen for this story, consistent with the expected structural delta (''targets specific practices ... rather than system-level structure''). A broader framing treating the entire discretionary structure as one constraint would likely require decomposition into a constraint family per practice, each with potentially different ε — the current single-story treatment aggregates several practices under one ε (0.79), which is a simplification the family-decomposition principle would eventually require if practice-level ε values diverge significantly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether to treat contested practices individually or as one aggregate extractive pattern; this story chose the aggregate for tractability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__gender_rights_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__gender_rights_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__gender_rights_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__gender_rights_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(marr_be_t8, marriage_authority__gender_rights_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(marr_be_t16, marriage_authority__gender_rights_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(marr_be_t24, marriage_authority__gender_rights_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(marr_be_t32, marriage_authority__gender_rights_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(marr_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t8, marriage_authority__gender_rights_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(marr_su_t16, marriage_authority__gender_rights_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(marr_su_t24, marriage_authority__gender_rights_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(marr_su_t32, marriage_authority__gender_rights_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(marr_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__gender_rights_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the marriage_authority kernel. communal_autonomy_reading and federalist_millet_reading treat the pluralist system itself as the legitimate object of analysis (self-governance, anti-majoritarian structure) and would author near-zero or low ε for personal law pluralism as such. secularist_reading treats the entire pluralist arrangement as a transitional anomaly with its own ε profile oriented toward system replacement. judicial_harmonization_reading shares this reading's practice-by-practice method but reads the courts' activity as building a general constitutional floor rather than responding to a specific gendered extraction pattern. This reading (gender_rights_reading) is unique among the five in authoring a snare classification with a concrete, named victim group (women_within_patriarchal_personal_law) and ε=0.79 — the other readings do not author this beneficiary/victim structure. Each sibling gets its own file, its own ε, and its own stakeholders per the ε-invariance principle; this file's affects_constraints links to all four to preserve the constraint family for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
