% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Constitutional Equality Floor on Personal Law (Gender Rights Reading)
 *   domain: legal/constitutional/family
 *
 * SUMMARY:
 *   The gender_rights_reading of marriage_authority instantiates a constraint
 *   where constitutional courts expand equality guarantees (Articles 14, 15,
 *   21) to strike down or read down gender-discriminatory practices in
 *   personal law codes — notably triple talaq, unequal maintenance, and
 *   discriminatory property rights. The reading presents this as vindicating
 *   the constitution's equality promise against the pluralist exception for
 *   family law. The claimed structural type is snare: the coordination story
 *   (gender equality) is real but the constraint's persistence depends on
 *   judicial coercion overriding communal authority, and the extraction falls
 *   asymmetrically on the women within patriarchal systems who bear
 *   disruption costs (loss of community standing, backlash, inadequate state
 *   enforcement) while professionalized women's rights advocates capture the
 *   status, funding, and doctrinal authority of the reform. The constraint
 *   targets specific practices rather than the pluralist system as a whole,
 *   cross-cutting the communal/secular divide: it allies with secularists on
 *   specific reforms but rejects the UCC legislative route; it opposes
 *   communal autonomy but uses the federalist pluralist structure as the
 *   lever for case-by-case intervention.
 *
 * KEY AGENTS:
 *   - women_rights_advocates: Primary beneficiary (organized/biographical/mobile) — captures doctrinal authority, litigation funding, institutional recognition from constitutional equality jurisprudence
 *   - women_within_patriarchal_personal_law: Primary victim (powerless/biographical/trapped) — bears disruption costs: communal backlash, loss of informal protections, inadequate state enforcement substitutes, litigation centered on advocates not them
 *   - communal_religious_authorities: Payer (institutional/generational/constrained) — loses interpretive monopoly over family law; resists through political mobilization and claims of religious freedom
 *   - supreme_court: Agenda setter (institutional/generational/analytical) — expands constitutional floor case-by-case; extracts institutional authority from preventing personal law revision
 *   - state_legislature: Excluded (powerful/biographical/constrained) — bypassed by judicial route; would prefer UCC legislation or status quo
 *   - legal_scholars: Observer (analytical/civilizational/analytical) — maps the doctrinal evolution; some corroborate equality contradiction, others critique judicial overreach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.78).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.75).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Constitutional Equality Floor on Personal Law (Gender Rights Reading)").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/constitutional/family").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866').
narrative_ontology:cs_kernel_codification('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', formalized).
narrative_ontology:cs_authority_grounding('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', extraction).
narrative_ontology:cs_interpretation_layer_present('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866').
narrative_ontology:cs_reading_relation('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', foundational, constitutional_equality_trumps_personal_law).
narrative_ontology:cs_axiom_status(constitutional_equality_trumps_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', constitutional_equality_trumps_personal_law, deontological).
narrative_ontology:cs_axiom('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', foundational, gender_equality_as_non_derogable_constitutional_floor).
narrative_ontology:cs_axiom_status(gender_equality_as_non_derogable_constitutional_floor, holdable).
narrative_ontology:cs_axiom_grounding('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', gender_equality_as_non_derogable_constitutional_floor, deontological).
narrative_ontology:cs_reference_frame('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', constitutional_equality_guarantee_primacy).
narrative_ontology:cs_drift_state('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', contemporary_judicial_activism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce6b0b1c-84c6-48a7-ae8a-77e8fe5ae866', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, communal_religious_authorities).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, constitutional_equality_guarantee).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, gender_equality_as_fundamental_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professionalized NGOs, lawyers, and academics who litigate constitutional equality cases against personal law provisions. They gain doctrinal authority, international recognition, funding, and institutional positions from successful litigation. Their exit is mobile — they can shift to other rights frameworks (labor, violence, representation) without losing professional standing.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% Women subject to personal law codes (Muslim, Hindu, Christian, Parsi) who face triple talaq, unequal maintenance, denied property rights. When courts strike down practices, they bear backlash: communal ostracization, loss of informal dispute resolution, inadequate state legal aid, and litigation strategies that center advocate priorities over their situated needs. Exit is trapped — leaving the community means losing kinship, economic, and social survival networks; staying means enduring the reformed-but-unenforced rights.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, local).

% Bodies that administer personal law (All India Muslim Personal Law Board, Hindu Dharmashastra councils, church tribunals). They lose interpretive monopoly when courts read down discriminatory provisions. They resist through political mobilization (claiming religious freedom under Article 25), legislative lobbying, and community discipline. Their exit is constrained — they cannot opt out of constitutional review but can delay compliance and shape communal adherence.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_religious_authorities, payer,
    institutional, generational, constrained, national).

% The apex court expands the constitutional equality floor through PILs and direct petitions (Shah Bano, Shayara Bano, Joseph Shine, etc.). It extracts institutional authority by positioning itself as the guardian of constitutional morality against legislative inaction and communal resistance. Its exit is analytical — it observes the constraint's effects from the adjudicative seat, not as a participant bearing costs.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Parliament and state legislatures hold the UCC mandate (Article 44) but are bypassed by judicial reform. They would prefer either full UCC legislation (secularist) or status quo pluralism (communal allies). They are excluded from the gender_rights_reading's reform path because the reading treats legislative inaction as the justification for judicial intervention. Their exit is constrained — they can enact overriding legislation but face political costs.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, state_legislature, excluded,
    powerful, biographical, constrained, national).

% Academics mapping the doctrinal evolution of constitutional equality in personal law. Some corroborate the equality contradiction (citing CEDAW, constitutional text); others critique judicial overreach and advocacy capture. They neither collect nor pay — they provide the epistemic infrastructure for all readings.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, legal_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Imposes a constitutional equality floor on personal law codes to solve the coordination problem of gender-discriminatory practices (triple talaq, maintenance, property) that vary across communities and violate formal constitutional guarantees.
% TRANSFER_FUNCTION: Moves interpretive authority over family law from communal religious bodies to the constitutional court; moves the cost of disruption from advocates to women within patriarchal systems who lose community standing without gaining enforceable state alternatives; moves institutional capital to the advocacy class that architects the litigation strategy.
% ABSENT_VOICES: Women within patriarchal personal law systems who are not represented by either communal authorities or professionalized rights advocates — they would object to both the patriarchal practices and the top-down judicial reform that disregards their situated agency. They are structurally excluded from both the communal decision-making and the litigation strategy-setting.
% DISAPPEARANCE_RATIONALE: If the constitutional equality floor vanished overnight, communal authorities would regain interpretive monopoly; triple talaq and discriminatory maintenance/property rules would be reinstated; the advocacy infrastructure built around PIL litigation would lose its central docket; the legislature would face renewed pressure for UCC or status quo. The pluralist equilibrium would shift back toward communal autonomy.
% FOUNDING_PROBLEM: The founding problem was the constitutional contradiction between formal equality guarantees (Articles 14, 15, 21) and the persistence of gender-discriminatory personal law practices permitted under the legal pluralism framework (Article 25, 26, 44).
% FOUNDING_PROBLEM_CORROBORATION: Constitutional text (Articles 14, 15, 21) and international human rights treaty obligations (CEDAW ratified 1993) corroborate the equality contradiction from outside the beneficiary set. Communal authorities and some feminist scholars (e.g., Flavia Agnes, Madhu Kishwar) contest whether judicial imposition solves rather than displaces the problem — they attest the founding problem is either solved by community reform or requires legislative not judicial resolution.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint moves interpretive authority and material costs from communal bodies to courts while the nominal beneficiaries (women in patriarchal systems) receive neither full protection nor agency in the process — the advocacy class captures the gains. Suppression (0.75) is high because the constraint's operation requires active judicial displacement of communal authority, and exits are blocked: women cannot easily leave the personal law system (identity_locked/trapped), communal authorities cannot opt out of constitutional review, and the legislature is excluded from the reform path. Theater ratio (0.42) is moderate: the equality jurisprudence is genuine but a growing share of litigation activity serves advocacy career-building and doctrinal refinement rather than material relief for affected women. Accessibility collapse (0.71) is high because once the constitutional floor is declared, communal alternatives are legally foreclosed — but practical alternatives (state enforcement, social support) remain inaccessible. Resistance (0.72) is high: communal authorities mobilize politically, legislatures delay codification, and affected women often resist the reform's communal disruption.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is sharp: from the supreme_court and women_rights_advocate seats, the constraint appears as rope/tangled_rope (genuine coordination solving a real equality problem with some asymmetric costs). From the women_within_patriarchal_personal_law seat, it appears as snare (coordination story covers extraction of advocacy capital; they bear costs without agency). From the communal_authority seat, it appears as snare (pure extraction of their legitimate authority). The engine computes this divergence from the structural data — the declared roles, power, and exit options make the directionality derivation produce high χ for the victim seat and low/negative χ for the beneficiary seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: women_rights_advocates (organized, mobile, biographical horizon) — they collect the constraint's gains: doctrinal authority, institutional recognition, funding. Their exit is arbitrage-grade (can shift to other rights frameworks). Victim: women_within_patriarchal_personal_law (powerless, trapped, biographical) — they bear the costs: communal backlash, loss of informal protections, litigation they don't control. Their exit is identity_locked (community membership constitutes their social survival). Agenda_setter: supreme_court (institutional, analytical, generational) — sets the constitutional floor, extracts institutional authority from kernel stability. The directionality derivation from these declarations produces d ≈ 0.15 for advocates, d ≈ 0.85 for affected women, d ≈ 0.2 for court — yielding high effective extraction for the victim seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (constitutional equality) has not atrophied — the equality contradiction persists. But the reading's method (judicial expansion) has displaced the legislative mandate (UCC via Article 44). The constraint prevents mandatrophy mislabeling by exposing that the coordination function (gender equality) is real but the extraction path (judicial advocacy capture) is distinct. A pure coordination reading would miss the victim seat's trapped position; a pure extraction reading would miss the genuine doctrinal victories (triple talaq struck down). The snare classification captures both: the coordination story is the vehicle, the extraction is the payload.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'This constraint is one reading (gender_rights_reading) of the contested kernel marriage_authority. What structural elements do sibling readings change, and where is the disagreement located?',
    'Compare the beneficiary/victim structure, ε referent, and coordination function across all five declared readings of the marriage_authority kernel. The disagreement is located in: (1) whether constitutional equality overrides communal authority (forecloses communal_autonomy), (2) whether judicial vs legislative reform is the legitimate path (coexists_with secularist), (3) whether pluralism is a consociational shield or patriarchal vehicle (influences federalist_millet), (4) whether gender-specific equality floor is the whole of harmonization or a subset (influences judicial_harmonization).',
    'If the kernel framing shifts, the constraint''s ε referent shifts: the standing arrangement under contest changes from ''patriarchal personal law practices'' to ''legal pluralism itself'' or ''legislative inaction on UCC''. This changes the extraction calculus entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Kernel/reading structure: which reading this is, what siblings change, where disagreement sits').

omega_variable(
    advocacy_capture_vs_gender_justice,
    'Does the judicial gender-equality strategy genuinely serve women within patriarchal personal law, or does it extract advocacy capital for professionalized rights actors while imposing disruption costs on the nominal beneficiaries?',
    'Longitudinal study of litigation outcomes vs. material conditions of affected women: do judicial victories on triple talaq, maintenance, property translate into enforceable rights and improved bargaining power, or do they trigger backlash, communal retrenchment, and loss of informal protections without state substitutes?',
    'If advocacy capture dominates, the constraint is a snare with women_rights_advocates as beneficiary and women_within_patriarchal_personal_law as victim. If material gains are real and diffuse, the constraint shifts toward tangled_rope (coordination + asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_capture_vs_gender_justice, empirical, 'Whether the constraint''s coordination function (gender equality) is genuine or cover for advocacy extraction').

omega_variable(
    suppression_mechanism_judicial_vs_communal,
    'Is the measured suppression structural (judicial enforcement displacing communal authority) or internalized (women accepting reform as liberation while losing community standing)?',
    'Post-reform ethnographic tracking: if women report increased agency and access to state enforcement without communal ostracization, suppression is primarily structural (judicial). If women report using state rights as leverage within community but facing new forms of informal sanction, suppression is hybrid. If women reject the reform as alien, suppression is internalized (the reform itself becomes a new constraint).',
    'If internalized, effective suppression is higher than structural measure — the target carries the constraint''s logic into spaces the court cannot reach. This would raise χ for the victim seat and reinforce snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_vs_communal, empirical, 'Structural vs internalized suppression in interpersonal/communal constraint dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__gender_rights_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__gender_rights_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__gender_rights_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__gender_rights_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marr_be_t8, marriage_authority__gender_rights_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(marr_be_t16, marriage_authority__gender_rights_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(marr_be_t24, marriage_authority__gender_rights_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(marr_be_t32, marriage_authority__gender_rights_reading, base_extractiveness, 32, 0.73).
narrative_ontology:measurement(marr_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t8, marriage_authority__gender_rights_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(marr_su_t16, marriage_authority__gender_rights_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(marr_su_t24, marriage_authority__gender_rights_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(marr_su_t32, marriage_authority__gender_rights_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(marr_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__gender_rights_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% Marriage authority kernel decomposes into five constraint stories with divergent ε values. This reading (gender_rights) has ε_high (snare) because it centers advocacy capture of judicial reform. communal_autonomy has ε_low (rope/mountain) for communities but ε_high for women. secularist has ε_medium (tangled_rope) — legislative coordination with transition costs. federalist_millet has ε_low (rope) for pluralism as coordination but ε_high for women. judicial_harmonization has ε_medium (tangled_rope) — broader coordination with asymmetric enforcement costs. The family is linked by affects_constraints edges from each to its dependents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, institutional, 0.2).
constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
