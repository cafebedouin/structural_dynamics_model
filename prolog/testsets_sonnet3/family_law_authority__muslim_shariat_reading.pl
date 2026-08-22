% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Muslim Personal Law Reading of Marriage as Nikah Contract (India)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story authors ONE reading — the Muslim personal law (shariat)
 *   reading — of the contested family_law_authority kernel in the Indian
 *   legal system, where marriage is governed by parallel personal law regimes
 *   rather than a single uniform civil code. Under this reading, nikah is a
 *   civil contract grounded in Quranic injunction and hadith, carrying
 *   distinctive structural features: mahr as a contractual dower obligation,
 *   permitted polygyny (up to four wives under stated conditions), and
 *   historically asymmetric dissolution access — unilateral talaq available
 *   to husbands versus judicially mediated khula for wives, a gap partially
 *   but not fully closed by the 2019 criminalization of instant triple talaq.
 *   The extraction measured here is internal to this reading's own operation
 *   (asymmetric dissolution burden on wives), not a comparison against the
 *   sibling readings, which are separate constraints with their own ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.52).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Personal Law Reading of Marriage as Nikah Contract (India)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'a9d4d774-bcbb-40c3-bfd0-373ada22fbab').
narrative_ontology:cs_kernel_codification('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', fixed_text).
narrative_ontology:cs_authority_grounding('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', lineage).
narrative_ontology:cs_interpretation_layer_present('a9d4d774-bcbb-40c3-bfd0-373ada22fbab').
narrative_ontology:cs_reading_relation('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', foundational, marriage_as_dissoluble_civil_contract).
narrative_ontology:cs_axiom_status(marriage_as_dissoluble_civil_contract, holdable).
narrative_ontology:cs_axiom_grounding('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', marriage_as_dissoluble_civil_contract, conventional).
narrative_ontology:cs_axiom('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', secondary, unilateral_male_initiated_dissolution_permissible).
narrative_ontology:cs_axiom_status(unilateral_male_initiated_dissolution_permissible, overridden).
narrative_ontology:cs_axiom_grounding('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', unilateral_male_initiated_dissolution_permissible, empirically_contingent).
narrative_ontology:cs_reference_frame('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', classical_fiqh_contractual_marriage).
narrative_ontology:cs_drift_state('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', post_2019_triple_talaq_criminalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a9d4d774-bcbb-40c3-bfd0-373ada22fbab', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, husbands_under_unilateral_talaq).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_religious_authorities_qazis).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, aimplb_personal_law_board).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, wives_facing_unilateral_divorce).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, wives_in_polygynous_households).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_denied_equal_dissolution_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, qazis_and_dar_ul_qazas).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_marital_contract_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, religious_personal_law_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, a husband may historically pronounce talaq to unilaterally dissolve the marriage without judicial process or the wife's consent (pre-2019 triple talaq ban), and may enter polygynous marriage up to four wives under conditions the reading treats as scripturally permitted. He owes mahr but otherwise holds asymmetric procedural power over dissolution.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, husbands_under_unilateral_talaq, beneficiary,
    moderate, biographical, mobile, national).

% A wife seeking dissolution historically had to pursue khula or judicial divorce through community qazis or courts, a slower and more contested path than her husband's unilateral talaq. Even where mahr is owed, enforcement is weak and social and economic dependency on the marriage or natal family narrows her real exit options.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, wives_facing_unilateral_divorce, payer,
    powerless, biographical, trapped, national).

% Where a husband takes additional wives under the permission this reading grants, existing wives bear reduced household resources, status competition, and diminished practical recourse, since the reading does not require their consent to the subsequent marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, wives_in_polygynous_households, payer,
    powerless, biographical, constrained, national).

% Functions as the principal institutional voice articulating and defending this reading of Muslim personal law before courts, legislature, and public opinion, resisting codification or reform (including opposing the 2019 triple talaq criminalization) on the ground that the community's own religious authority, not the state, should govern marital dissolution.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, aimplb_personal_law_board, agenda_setter,
    institutional, generational, arbitrage, national).

% Community-appointed religious adjudicators administer nikah, talaq, khula and mahr disputes under this reading's framework, deriving institutional standing and income from being the recognized interpreters of the marital contract; they have professional and reputational stake in the reading's continued authority.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, qazis_and_dar_ul_qazas, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, qazis_and_dar_ul_qazas, beneficiary).

% Groups such as those that litigated Shayara Bano argue the gender-asymmetric dissolution structure is not compelled by the Quran itself but by a particular juristic tradition, and press for reform from within Islamic jurisprudence. They are heard in courts and legislature but are not the recognized voice of the personal law board and are frequently characterized by it as external to authentic religious authority.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, reformist_muslim_womens_organizations, excluded,
    organized, biographical, constrained, national).

% Adjudicates constitutional challenges to specific practices (e.g., Shayara Bano v. Union of India, 2017) within this personal law framework, balancing religious freedom claims against constitutional equality guarantees; its rulings (and the 2019 criminalization statute) alter the enforcement landscape without displacing the underlying kernel reading.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, indian_state_judiciary, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, indian_state_judiciary, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, diffuse).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, religiously sanctioned framework for entering and dissolving marriage that specifies obligations (mahr), permissible plural structures, and recognized dissolution mechanisms (talaq, khula, mubarat), giving the community an internally legitimate alternative to relying solely on state civil law.
% TRANSFER_FUNCTION: Moves procedural control over marital dissolution disproportionately toward husbands (unilateral talaq) and toward existing household structures that absorb additional wives, while wives bear the cost of slower, contested exit and of resource-sharing they did not consent to; mahr is the formal offsetting transfer but its enforcement is inconsistent.
% ABSENT_VOICES: Reformist Muslim women's organizations and dissenting jurists who read the Quranic text as requiring reconciliation attempts and arbitration before talaq are present in courts but excluded from the personal law board's institutional voice, which frames itself as the community's sole authentic interpreter.
% DISAPPEARANCE_RATIONALE: If this reading's institutional apparatus (personal law board authority, qazi adjudication networks, community recognition of talaq/khula) vanished overnight, marital dissolution for millions of Muslim Indians would default entirely to secular civil procedure, the qazi courts would lose their function, and both the coordination benefits (community-legitimate dissolution) and the asymmetric costs (unilateral talaq exposure) would disappear together.
% FOUNDING_PROBLEM: Early Islamic jurisprudence sought to formalize marriage as an enforceable civil contract with defined mutual obligations (mahr, maintenance, defined dissolution paths) in societies where marriage had previously been governed by looser tribal custom offering women even less formal protection.
% FOUNDING_PROBLEM_CORROBORATION: The personal law board and allied qazis attest the framework still serves its founding function of protecting women's contractual rights (mahr) and community religious autonomy. Reformist Muslim jurists, the Supreme Court in Shayara Bano, and independent scholars of Islamic law attest that the specific unilateral-talaq practice diverges from the Quran's own emphasis on arbitration and reconciliation, and that its persistence serves institutional and patriarchal interest more than the original contractual-protection problem.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58, dipping slightly at t=40 after the 2019 statutory intervention) reflects the asymmetric cost wives bear in dissolution access and polygyny exposure, offset partially by the mahr obligation and by khula/judicial-divorce avenues that do exist. Suppression (0.52, also easing post-2019) is moderate rather than severe: the constraint operates through community and institutional authority (personal law board, qazi networks) rather than pure coercion, and reform pressure from within Muslim civil society and the judiciary has had real effect. Theater ratio (0.28, rising then partially retreating) captures the personal law board's public defense of religious autonomy increasingly serving institutional self-preservation once courts began narrowing the specific practice (instant triple talaq) most exposed to challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the personal law board's and qazi networks' seats, the arrangement is a defended religious-autonomy coordination structure protecting community self-governance against state encroachment. From the seat of a wife facing unilateral talaq, the identical structure is experienced as asymmetric extraction of dissolution control. The engine computes this divergence from the differing power/exit declarations; the claimed_type does not resolve it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Husbands historically exercising unilateral talaq and the institutional actors (personal law board, qazis) who administer and defend this reading sit near the beneficiary end: they hold procedural control and institutional standing without bearing the asymmetric dissolution cost. Wives facing unilateral divorce and wives in polygynous households sit near the target end: trapped or constrained exit, bearing the transfer, with mahr as an inconsistent offsetting mechanism. The judiciary sits in an observer/agenda-setter dual role — it does not benefit from or pay into the arrangement, but its 2017-2019 interventions materially shifted enforcement without displacing the underlying kernel reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — formalizing marriage as an enforceable contract with defined obligations in place of looser, less protective custom — retains partial vitality (mahr enforcement, contractual clarity) even as the specific unilateral-talaq mechanism has drifted from coordination toward asymmetric extraction. Classifying this as tangled_rope rather than snare preserves that the coordination function (contractual formalization, community-legitimate dissolution paths) is real and not merely cover, while still registering the asymmetric enforcement cost on wives that a pure Rope reading would erase. It also avoids classifying the whole reading as pure extraction (snare), which would ignore mahr, khula, and the internal reform movement's own use of the same Quranic text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quranic_text_vs_juristic_accretion,
    'Is the gender-asymmetric dissolution structure (unilateral talaq without arbitration) compelled by the Quranic text itself, or is it a juristic-tradition accretion that later schools of fiqh added and that reformist readings can legitimately strip away?',
    'Comparative textual and historical analysis of classical fiqh commentary against the specific Quranic verses on divorce (2:229-232) and hadith on reconciliation requirements, cross-referenced against reform movements'' own textual arguments (as advanced in Shayara Bano litigation).',
    'If the asymmetry is accretion rather than core doctrine, the reading''s high extraction is a corrigible institutional choice rather than an irreducible feature of the kernel reading itself, supporting internal reform over external imposition. If compelled by text, reform pressure structurally conflicts with the reading''s own axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_text_vs_juristic_accretion, conceptual, 'Whether unilateral talaq asymmetry is textually compelled or juristically accreted.').

omega_variable(
    personal_law_board_representativeness,
    'Does the AIMPLB''s public defense of this reading represent a genuine consensus among Indian Muslims, or does it represent a particular institutional and largely male leadership whose interest in the reading''s persistence diverges from the broader community''s?',
    'Survey data on Muslim women''s and men''s actual preferences regarding codification and reform, compared against the board''s public positions and its internal composition.',
    'If representativeness is low, the tangled_rope classification''s coordination function is weaker than claimed and the arrangement tilts further toward the board''s own institutional extraction rather than genuine community coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(personal_law_board_representativeness, empirical, 'Whether AIMPLB''s defense of the reading reflects community consensus or institutional self-interest.').

omega_variable(
    state_intervention_boundary,
    'Where is the legitimate boundary between state constitutional oversight (equality guarantees) and religious community self-governance in personal law — does the 2019 triple-talaq criminalization represent proper correction of a rights violation, or improper state intrusion into the reading''s own internal reform capacity?',
    'Tracking whether internal reform mechanisms (khula reform, arbitration requirements) would have addressed the asymmetry absent state criminalization, versus whether external intervention was necessary because internal reform had stalled.',
    'Bears on whether future drift in this reading''s extraction/suppression profile should be attributed to state action or to the reading''s own internal reform trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intervention_boundary, preference, 'The proper locus of authority for resolving this reading''s internal asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fami_tr_t8, family_law_authority__muslim_shariat_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fami_tr_t16, family_law_authority__muslim_shariat_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(fami_tr_t24, family_law_authority__muslim_shariat_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(fami_tr_t32, family_law_authority__muslim_shariat_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__muslim_shariat_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fami_be_t8, family_law_authority__muslim_shariat_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(fami_be_t16, family_law_authority__muslim_shariat_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(fami_be_t24, family_law_authority__muslim_shariat_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(fami_be_t32, family_law_authority__muslim_shariat_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(fami_be_t40, family_law_authority__muslim_shariat_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fami_su_t8, family_law_authority__muslim_shariat_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(fami_su_t16, family_law_authority__muslim_shariat_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(fami_su_t24, family_law_authority__muslim_shariat_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(fami_su_t32, family_law_authority__muslim_shariat_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(fami_su_t40, family_law_authority__muslim_shariat_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language kernel 'family law authority in India' (per the ε-invariance principle: each religious personal law tradition and the secular civil code option constitute structurally distinct constraints with different beneficiary/victim sets and different ε, not one constraint measured five ways). This file authors the muslim_shariat_reading only; christian_canonical_reading, hindu_dharmashastra_reading, parsi_zoroastrian_reading, and secular_contractual_reading are separate files linked via affects_constraints. The secular_contractual_reading is most directly influenced by this reading's drift (state intervention such as the 2019 triple-talaq statute reshapes the boundary between religious personal law and secular civil code jurisdiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
