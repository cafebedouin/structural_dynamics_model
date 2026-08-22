% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Based Category Membership for 'Woman'/'Female' (Biology Reading)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story instantiates the sex-biology reading of the contested
 *   'woman'/'female' category kernel: membership is fixed by chromosomal sex,
 *   reproductive anatomy, and developmental biology, independent of
 *   self-identification. Under this reading, sex-segregated institutions
 *   (prisons, shelters, sport, changing facilities) use a biological
 *   criterion to allocate access, producing a clean, verifiable boundary that
 *   benefits natal females seeking sex-based protections but categorically
 *   excludes trans women from those same spaces. This is a distinct
 *   constraint from the gender_identity_reading (which would flip beneficiary
 *   and victim sets entirely) and the hybrid_contextual_reading (which splits
 *   the criterion by context). Each reading is authored as its own file with
 *   its own epsilon; they are linked through the kernel network, not blended
 *   into one measurement.
 *
 * KEY AGENTS:
 *   - natal_females_seeking_sex_based_protections: beneficiary of the categorical boundary in safety and competitive contexts
 *   - womens_sport_governing_bodies and sex_segregated_shelter_operators: institutional agenda-setters who administer and enforce the biological criterion
 *   - trans_women_seeking_female_space_access: excluded/target population under this reading, bearing high physical-safety and dignitary costs
 *   - legislators_and_courts: the contested authority seat where this reading and its siblings fight for legal supremacy
 *   - clinicians_and_biologists: analytical observers whose findings are invoked selectively by all readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.58).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.52).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Based Category Membership for 'Woman'/'Female' (Biology Reading)").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '65dcbfb6-2cbc-4183-9a84-575c247b5a87').
narrative_ontology:cs_kernel_codification('65dcbfb6-2cbc-4183-9a84-575c247b5a87', distributed).
narrative_ontology:cs_authority_grounding('65dcbfb6-2cbc-4183-9a84-575c247b5a87', distributed).
narrative_ontology:cs_reading_relation('65dcbfb6-2cbc-4183-9a84-575c247b5a87', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('65dcbfb6-2cbc-4183-9a84-575c247b5a87', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('65dcbfb6-2cbc-4183-9a84-575c247b5a87', foundational, reproductive_biology_is_the_operative_category_criterion).
narrative_ontology:cs_axiom_status(reproductive_biology_is_the_operative_category_criterion, holdable).
narrative_ontology:cs_axiom_grounding('65dcbfb6-2cbc-4183-9a84-575c247b5a87', reproductive_biology_is_the_operative_category_criterion, empirically_contingent).
narrative_ontology:cs_axiom('65dcbfb6-2cbc-4183-9a84-575c247b5a87', secondary, sex_segregated_protections_require_verifiable_not_self_reported_criteria).
narrative_ontology:cs_axiom_status(sex_segregated_protections_require_verifiable_not_self_reported_criteria, holdable).
narrative_ontology:cs_axiom_grounding('65dcbfb6-2cbc-4183-9a84-575c247b5a87', sex_segregated_protections_require_verifiable_not_self_reported_criteria, instrumental).
narrative_ontology:cs_reference_frame('65dcbfb6-2cbc-4183-9a84-575c247b5a87', biological_sex_common_law_tradition).
narrative_ontology:cs_drift_state('65dcbfb6-2cbc-4183-9a84-575c247b5a87', post_gender_recognition_legislation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('65dcbfb6-2cbc-4183-9a84-575c247b5a87', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, womens_sport_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, sex_segregated_shelter_operators).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_seeking_female_space_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on sex-defined categories to access single-sex prisons, shelters, changing rooms, and sport tiers on the premise that average male physical advantage and male-pattern violence risk persist regardless of a person's gender identity. Their protections are secured by excluding anyone who does not meet the chromosomal/anatomical criterion, whatever that person's identity or intentions.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    moderate, biographical, constrained, national).

% Set eligibility rules for women's competitive categories using birth-sex or puberty-based criteria, citing retained athletic advantage from male puberty. They administer testing and enforcement, and benefit reputationally and competitively from a legible, defensible category boundary.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, womens_sport_governing_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, womens_sport_governing_bodies, beneficiary).

% Operate domestic violence shelters and rape crisis services on a sex-based admissions model, screening intake by anatomy/birth sex to preserve trauma-informed single-sex space for a population disproportionately victimized by males. They enforce the boundary through intake policy and bear reputational and legal risk for how they draw it.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, sex_segregated_shelter_operators, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, sex_segregated_shelter_operators, beneficiary).

% Live and identify as women but do not meet the chromosomal/anatomical criterion this reading uses to define the category. Under this reading they are categorically excluded from female-designated prisons, shelters, changing rooms, and sport tiers regardless of transition status, hormone therapy, or legal gender recognition, and instead face placement in male facilities or exclusion from services entirely — an outcome many describe as itself producing acute physical danger.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women_seeking_female_space_access, payer,
    powerless, biographical, trapped, national).

% Write and adjudicate the statutory and case-law definitions of 'sex' and 'woman' that this reading relies on for legal force — equality law exceptions, prison classification statutes, sport eligibility regulation. They can broaden, narrow, or strike the biological definition, and are the site where this reading and its siblings directly contest legal supremacy.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legislators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Argue the biological criterion is under-inclusive of lived risk and misgendering harm and press for self-identification or hybrid standards instead; within this reading's own institutional processes (sport governance panels, shelter accreditation bodies) their objections are heard but structurally cannot change the criterion without abandoning the reading itself.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_rights_advocacy_organizations, excluded,
    organized, biographical, constrained, national).

% Describe the empirical facts of sexual dimorphism, gamete size, and the developmental effects of puberty and hormone therapy that this reading cites as its evidentiary basis. They do not set policy but their findings are contested and selectively invoked by all three readings of the kernel.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, clinicians_and_biologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, third-party-verifiable criterion (chromosomes, gonadal/gamete structure, developmental history) for allocating access to single-sex spaces and competitive categories, avoiding case-by-case discretionary judgment about who 'counts' and reducing the risk that self-report can be strategically exploited by bad actors to gain access to vulnerable populations.
% TRANSFER_FUNCTION: Moves access to sex-segregated safety and competitive resources (shelter beds, prison housing, changing facilities, competitive sport tiers) toward natal females as a class, and away from trans women, who are categorically excluded from those resources under this reading regardless of their gender identity or legal recognition status.
% ABSENT_VOICES: Trans women themselves are heard in policy consultations but hold no seat that can alter the criterion without changing readings entirely; intersex individuals whose chromosomal/anatomical status does not cleanly sort under this reading's binary are almost entirely absent from the debate as structured.
% DISAPPEARANCE_RATIONALE: If sex-based category membership vanished as a legal and institutional criterion overnight, single-sex prisons, shelters, changing rooms, and sport tiers would need an entirely different eligibility test (self-identification or a hybrid standard), immediately changing who occupies those spaces and how safety and fairness claims are adjudicated — the arrangement is load-bearing for a large body of existing law, sport governance, and service provision, not a redundant label.
% FOUNDING_PROBLEM: Historically, 'woman'/'female' as a legal and social category tracked reproductive and developmental biology because that biology correlated with a durable set of vulnerabilities (pregnancy, average physical size/strength differential, disproportionate victimization by males) that legal and institutional protections were built to address.
% FOUNDING_PROBLEM_CORROBORATION: Sport scientists and some domestic-violence-service researchers, who are not parties benefiting from the categorical exclusion of trans women, attest that average post-pubertal physical differences persist meaningfully for elite competition and that some shelter residents report distress at mixed-sex housing regardless of the other resident's legal gender status — corroboration exists but is itself contested by other researchers and by trans advocacy organizations who argue the framing pathologizes trans women as a class rather than assessing individual risk.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that trans women bear a substantial, concentrated cost (exclusion from safety-critical and socially significant spaces) while the coordination benefit (administrable, verifiable category boundary) is real but not the whole story — this is not pure extraction because the underlying safety and fairness concerns for natal females are genuine, hence tangled_rope rather than snare. Suppression (0.52) is moderate: enforcement relies on statute, institutional policy, and intake screening rather than criminal coercion, but it is actively maintained and resists case-by-case appeal. Accessibility collapse (0.45) is only moderate because legal and institutional contest is ongoing — this is not a settled, uncontestable boundary like a mountain; resistance (0.78) is high, reflecting sustained litigation, protest, and organizing against the criterion by excluded and advocacy parties. Theater ratio is low (0.2) because the enforcement (intake screening, eligibility testing) is functionally connected to the stated purpose, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the natal-female beneficiary and institutional agenda-setter seats, this reading looks like a rope: a workable, science-grounded solution to a genuine coordination problem (verifiable eligibility, safety assurance). From the trans-woman payer seat, the identical structure computes as extraction dressed in safety language — the category boundary imposes a concentrated, often severe cost (housing in male facilities, exclusion from services, competitive exclusion) that the seat did not choose and cannot exit. The engine is expected to register this asymmetry directly from the stakeholder structure rather than from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females and the institutions administering sex-based protections sit near the beneficiary end: the category boundary is built around their claimed vulnerabilities and they retain agenda-setting power. Trans women sit near the full-target end: trapped exit options (no institutional path to reclassify under this reading without changing readings entirely), concentrated cost, and no seat at the table that can alter the criterion. Legislators and courts are agenda-setters with analytical exit (they can rewrite the rule) but are themselves the contested terrain between this reading and its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (biological correlates of vulnerability driving protective institutional design) is contested rather than dead: sport scientists and some service researchers outside the beneficiary class attest continuing relevance, while advocacy organizations and some clinicians argue the criterion is now doing exclusionary work beyond its original safety rationale, especially given hormonal and surgical transition options that were not contemplated when many of these institutions were designed. Classifying this as tangled_rope rather than snare or rope prevents both mislabelings: it is not pure extraction (the safety concern for natal females is not manufactured), and it is not pure coordination (the cost to trans women is concentrated, severe, and non-consensual, and the criterion actively forecloses accommodation short of changing readings).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is chromosomal/anatomical sex the correct operative criterion for ''woman''/''female'' category membership in law and institutional policy, or should gender identity (gender_identity_reading) or a context-dependent hybrid (hybrid_contextual_reading) govern instead?',
    'This is not empirically resolvable by biology alone — it is a contested normative and legal question being fought out in legislatures, courts, and sport governance bodies across jurisdictions, with different jurisdictions currently landing on different readings.',
    'Adopting the gender_identity_reading would invert the beneficiary/victim sets entirely (trans women become beneficiaries, natal-female-only claims become the excluded position). Adopting the hybrid_contextual_reading would split the criterion by context, producing a different, more fragmented set of victim/beneficiary relationships depending on domain (medical/sports/safety vs. social/legal recognition). Each reading is authored as a separate constraint story linked via network.affects_constraints; this omega documents that the choice among them is the central live contest, not a settled fact this story can adjudicate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, preference, 'Which reading of the woman/female kernel ought to govern institutional and legal category membership is a contested normative choice, not an empirical question this story resolves.').

omega_variable(
    safety_evidence_contestation,
    'How much of the measured extractiveness against trans women is offset by genuine, empirically-grounded safety concerns for natal females (retained average physical differences, disproportionate male-perpetrated violence), versus how much reflects a categorical exclusion that outpaces the actual risk profile of transitioned individuals?',
    'Longitudinal, methodologically rigorous studies of outcomes in facilities/sports that have adopted different eligibility criteria (biological, hybrid, self-ID), ideally conducted or reviewed by researchers without a stake in either reading''s institutional survival.',
    'If safety outcomes are shown to track transition status/hormonal profile rather than birth sex, that would weaken the coordination-function claim of this reading and shift its computed type toward snare; if outcomes track birth sex robustly regardless of transition status, the coordination-function claim strengthens and the tangled_rope classification''s coordination component is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_evidence_contestation, empirical, 'The degree to which the biological criterion''s safety rationale is empirically supported versus categorically overbroad is unresolved and central to the story''s classification.').

omega_variable(
    cs_framing_underdetermination,
    'Should the kernel here be framed as a legal/definitional dispute over a formalized statutory text (kernel_codification: formalized, authority_grounding: lineage via case law and legislative history), or as a more distributed, ongoing social contest with no single adjudicating authority (kernel_codification: distributed, authority_grounding: distributed)?',
    'Jurisdictional survey: in jurisdictions with settled statutory or constitutional definitions and stable case law, the formalized/lineage framing fits; in jurisdictions actively relitigating the definition across multiple competing court systems and legislatures with no settled precedent, the distributed framing fits better.',
    'The formalized/lineage framing implies existing legal text and precedent partially constrain and interpret this reading (an interpretation_layer_present), damping some of the raw contest; the distributed framing implies no stable interpretive buffer exists and every dispute is litigated from first principles, which would raise the story''s effective resistance and suppression metrics further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether this constraint''s commitment-system structure is best modeled as formalized/lineage or distributed/no-authority is itself contested and affects downstream interpretation-layer claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t6, woman_female_category__sex_biology_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__sex_biology_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(woma_tr_t18, woman_female_category__sex_biology_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(woma_tr_t24, woman_female_category__sex_biology_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(woma_be_t6, woman_female_category__sex_biology_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(woma_be_t12, woman_female_category__sex_biology_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(woma_be_t18, woman_female_category__sex_biology_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(woma_be_t24, woman_female_category__sex_biology_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(woma_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t6, woman_female_category__sex_biology_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(woma_su_t12, woman_female_category__sex_biology_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(woma_su_t18, woman_female_category__sex_biology_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(woma_su_t24, woman_female_category__sex_biology_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(woma_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the woman_female_category kernel. sex_biology_reading (this file) fixes membership by chromosomal/anatomical criteria and computes trans women as the victim class with high extraction on physical-safety grounds. gender_identity_reading fixes membership by self-identification and would compute an inverted beneficiary/victim structure. hybrid_contextual_reading splits the criterion by domain and produces a mixed, domain-dependent structure. Each carries its own stable epsilon per the epsilon-invariance principle; none is a measurement of the same constraint under a different observable — they are three structurally distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
