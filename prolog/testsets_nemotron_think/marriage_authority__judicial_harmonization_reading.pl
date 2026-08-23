% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Marriage Authority via Constitutional Floor
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   India's Supreme Court has progressively imposed a constitutional floor on
 *   marriage and divorce laws across religious personal codes through
 *   case-by-case litigation (Shah Bano, Shayara Bano, Joseph Shine, etc.),
 *   without Parliament enacting a Uniform Civil Code. This constraint story
 *   captures the judicial harmonization reading of the marriage_authority
 *   kernel: the Court acts as a scaffold-like mechanism that coordinates
 *   rights protection but extracts institutional authority from communities
 *   and legislature alike. The claimed type is tangled_rope because the
 *   arrangement simultaneously coordinates (provides a rights floor) and
 *   extracts (judicial aggrandizement, communal autonomy loss) under active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.62).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.68).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Marriage Authority via Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, '737c9cab-3681-4582-9c34-c6c68380273a').
narrative_ontology:cs_kernel_codification('737c9cab-3681-4582-9c34-c6c68380273a', fixed_text).
narrative_ontology:cs_authority_grounding('737c9cab-3681-4582-9c34-c6c68380273a', extraction).
narrative_ontology:cs_interpretation_layer_present('737c9cab-3681-4582-9c34-c6c68380273a').
narrative_ontology:cs_reading_relation('737c9cab-3681-4582-9c34-c6c68380273a', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('737c9cab-3681-4582-9c34-c6c68380273a', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('737c9cab-3681-4582-9c34-c6c68380273a', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('737c9cab-3681-4582-9c34-c6c68380273a', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_axiom('737c9cab-3681-4582-9c34-c6c68380273a', foundational, constitutional_court_as_primary_harmonizer).
narrative_ontology:cs_axiom_status(constitutional_court_as_primary_harmonizer, holdable).
narrative_ontology:cs_axiom_grounding('737c9cab-3681-4582-9c34-c6c68380273a', constitutional_court_as_primary_harmonizer, conventional).
narrative_ontology:cs_axiom('737c9cab-3681-4582-9c34-c6c68380273a', foundational, incremental_judicial_harmonization_superior_to_legislative_ucc).
narrative_ontology:cs_axiom_status(incremental_judicial_harmonization_superior_to_legislative_ucc, holdable).
narrative_ontology:cs_axiom_grounding('737c9cab-3681-4582-9c34-c6c68380273a', incremental_judicial_harmonization_superior_to_legislative_ucc, instrumental).
narrative_ontology:cs_reference_frame('737c9cab-3681-4582-9c34-c6c68380273a', constitutional_court_led_harmonization).
narrative_ontology:cs_drift_state('737c9cab-3681-4582-9c34-c6c68380273a', contemporary_judicial_activism_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('737c9cab-3681-4582-9c34-c6c68380273a', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, gender_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, religious_communities).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, traditional_personal_law_boards).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, constitutional_supremacy_in_family_law).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, gender_equality_as_constitutional_floor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The apex court issues case-by-case rulings that impose a constitutional floor on marriage and divorce provisions across all personal law codes. It accumulates interpretive authority and institutional prestige by positioning itself as the guardian of fundamental rights within family law, without legislative mandate for a Uniform Civil Code. Its decisions are binding on all lower courts and personal law boards.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, supreme_court, beneficiary).

% Women's rights organizations and feminist legal groups litigate test cases to push the court toward progressive interpretations of equality and dignity. They gain concrete legal victories (e.g., striking down instant triple talaq, equalizing divorce grounds) but remain dependent on judicial goodwill and cannot codify gains legislatively. Their exit is constrained by the lack of alternative forums for systemic reform.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, gender_rights_advocates, beneficiary,
    organized, biographical, constrained, national).

% Muslim, Christian, Parsi, and Hindu personal law communities experience progressive erosion of their autonomous family law jurisdiction. Each ruling narrows the space for community-specific norms. They bear the cost of compliance, litigation, and perceived loss of cultural-religious autonomy. Exit is constrained because the constitutional order is inescapable; resistance takes the form of political mobilization and demands for legislative override.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, religious_communities, payer,
    organized, biographical, constrained, national).

% Bodies like the All India Muslim Personal Law Board or Christian denominational authorities lose interpretive monopoly over marriage and divorce. Their authority is displaced by court-appointed amicus curiae and constitutional bench reasoning. They are trapped within a legal framework that treats their norms as subordinate to fundamental rights, with no recognized exit to a parallel legal order.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, traditional_personal_law_boards, payer,
    moderate, biographical, trapped, national).

% The democratic legislature is bypassed by judicial harmonization. While it retains formal power to enact a Uniform Civil Code, political deadlock and vote-bank calculations prevent action. The court's case-by-case approach fills the legislative vacuum, making Parliament a spectator to the redefinition of family law. It could reassert authority by passing UCC but has not done so for decades.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, parliament_legislature, excluded,
    institutional, generational, analytical, national).

% Academic commentators track the doctrinal coherence, legitimacy, and sociological impact of judicial harmonization. They provide the intellectual framework for both critique and defense but hold no decision-making power. Their exit is analytical — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legal_scholars_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform constitutional floor for marriage rights across diverse personal laws, preventing a race to the bottom in rights protection and resolving inter-community conflicts through a single authoritative interpreter.
% TRANSFER_FUNCTION: Transfers interpretive authority from communal religious bodies to the constitutional court; moves decision-making power from community-level adjudication to apex court precedent; shifts the cost of legal uncertainty from individual litigants (mostly women) to the collective institutional apparatus of personal law boards.
% ABSENT_VOICES: Religious minorities who view personal law as essential to cultural survival and political representation; their voices are marginalized in the judicial process which is dominated by majoritarian constitutional interpretation and rights-discourse framed in universalist terms. Also absent: the hypothetical 'uniform civil code' that would make this judicial mechanism obsolete — it is a phantom alternative that never materializes.
% DISAPPEARANCE_RATIONALE: If judicial harmonization vanished overnight, personal law codes would revert to community control without a constitutional floor. Gender-discriminatory provisions (e.g., unilateral divorce, unequal maintenance) would regain legal force until/unless Parliament enacted a UCC. The rights landscape would fragment along community lines, and the Supreme Court would lose its central role in family law reform.
% FOUNDING_PROBLEM: The problem of unequal rights within personal law codes, especially gender inequality in marriage and divorce, combined with legislative deadlock on enacting a Uniform Civil Code (UCC) since the Constitution's directive principle (Article 44). The court stepped into the vacuum to protect fundamental rights case by case.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations (e.g., BMMA, AIDWA) and constitutional scholars (e.g., Granville Austin, Upendra Baxi) outside the judiciary attest to the persistence of gender inequality in personal law and the necessity of judicial intervention. The judiciary itself claims the problem is being addressed incrementally. Religious community leaders argue the founding problem is a manufactured crisis used to justify judicial overreach.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the Court's accumulation of interpretive monopoly and the transfer of decision costs to personal law boards. Suppression (0.68) is high because communities cannot exit the constitutional order and face contempt sanctions for non-compliance. Theater ratio (0.38) rises over time as the Court's 'gap-filling' rhetoric masks a de facto legislative function. Accessibility collapse (0.78) is high because once a constitutional floor is declared, alternative community-based norms become legally inoperable. Resistance (0.55) is moderate: communities resist politically but comply legally.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's seat, the constraint is a rope (coordination of rights). From the community boards' seat, it is a snare (extraction of autonomy). From gender advocates' seat, it is a scaffold (temporary until UCC). The engine's per-seat classification will reveal this divergence. The claimed type (tangled_rope) reflects the aggregate structure: genuine coordination function + asymmetric extraction + active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court is the primary beneficiary (d ~ 0.15) — it gains authority, prestige, and institutional centrality. Gender rights advocates are secondary beneficiaries (d ~ 0.35) — they gain rights but remain dependent. Religious communities and personal law boards are payers (d ~ 0.85) — they lose interpretive control and bear compliance costs. Parliament is excluded (d ~ 0.5) — it retains formal power but is structurally displaced. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (protect fundamental rights in family law) remains live, but the mechanism (judicial case-by-case harmonization) has outlived its provisional justification. The founding problem (legislative deadlock on UCC) is contested — some argue the Court's intervention reduces pressure for legislative solution (mandatrophy), others argue it is the only viable path. The classification as tangled_rope prevents mislabeling this as pure coordination (rope) or pure extraction (snare) by forcing recognition of both functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the judicial harmonization reading a distinct normative position or merely the institutional mechanism that implements the gender_rights_reading?',
    'Analyze whether the Court''s jurisprudence contains a unique theory of marriage authority (e.g., ''constitutional morality as harmonizer'') separable from the gender equality claims it adjudicates. Compare citation networks and doctrinal reasoning across sibling readings.',
    'If it is merely a mechanism, it should not be a separate constraint story but a sub-mechanism of gender_rights_reading. If distinct, its ε and beneficiary structure (judiciary as primary beneficiary) must be modeled independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading has independent normative content or is purely instrumental.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of communal autonomy structural (court orders, contempt power) or internalized (communities accept judicial supremacy as legitimate)?',
    'Post-compliance observation: if communities comply only under threat of sanction, suppression is structural. If they voluntarily adopt constitutional language in their own adjudication, internalization is present. Survey data on community leaders'' attitudes toward judicial review of personal law.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists even without active enforcement. This would push classification toward snare/piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in intercommunity legal pluralism.').

omega_variable(
    naturalness_of_constitutional_floor,
    'Is the constitutional floor a natural evolution of constitutional law (inevitable working out of equality guarantees) or a constructed judicial overreach (invention of a harmonization mandate not in the text)?',
    'Historical analysis of constituent assembly debates on Article 44 (UCC) and fundamental rights chapter. Comparative study of other pluralistic democracies (Israel, Lebanon, Canada) to see if judicial harmonization emerges without legislative UCC.',
    'If natural evolution, the constraint trends toward rope/mountain (low extraction). If constructed overreach, extraction is higher and mandatrophy risk rises. This is the core ambiguity for false_summit_mountain detection if claimed_type were mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_constitutional_floor, conceptual, 'Whether the constitutional floor is discovered or invented by the Court.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mahr_tr_t1985, marriage_authority__judicial_harmonization_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(mahr_tr_t1995, marriage_authority__judicial_harmonization_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(mahr_tr_t2005, marriage_authority__judicial_harmonization_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(mahr_tr_t2015, marriage_authority__judicial_harmonization_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(mahr_tr_t2020, marriage_authority__judicial_harmonization_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(mahr_tr_t2025, marriage_authority__judicial_harmonization_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(mahr_be_t1985, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(mahr_be_t1995, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(mahr_be_t2005, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement(mahr_be_t2015, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(mahr_be_t2020, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(mahr_be_t2025, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mahr_su_t1985, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(mahr_su_t1995, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(mahr_su_t2005, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(mahr_su_t2015, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(mahr_su_t2020, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2020, 0.67).
narrative_ontology:measurement(mahr_su_t2025, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__judicial_harmonization_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, uniform_civil_code_legislation).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, personal_law_board_authority).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_equality_jurisprudence).

% DUAL FORMULATION NOTE:
% Part of marriage_authority kernel family. This reading (judicial_harmonization) differs from gender_rights_reading by centering the Court as beneficiary and mechanism, not just the equality outcome. It differs from secularist_reading by accepting pluralism's persistence and using judicial rather than legislative harmonization. Linked via affects_constraints to the UCC legislation constraint that would supplant it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
