% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Contextual Category Membership for Woman/Female (Hybrid Reading)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint story captures the hybrid contextual reading of the
 *   'woman/female' category kernel: biological sex governs category
 *   membership in medical, sports, and safety contexts (where material bodily
 *   differences are held to be relevant), while gender identity governs in
 *   social and legal recognition contexts (where self-identification and
 *   dignity are held to be paramount). The reading is instantiated in policy
 *   frameworks like the UK Equality Act's 'sex-based exceptions' alongside
 *   Gender Recognition Certificate provisions, US Title IX regulatory
 *   oscillations, and IOC/World Athletics eligibility standards contrasted
 *   with legal gender recognition laws. The constraint is actively enforced
 *   through institutional gatekeeping: sports federations police testosterone
 *   thresholds and eligibility criteria; healthcare systems apply sex-based
 *   protocols; legal systems issue and recognize gender markers; employers
 *   administer single-sex spaces and equality monitoring. Extraction is
 *   moderate (ε=0.42) and rising: both constituency groups bear costs in
 *   contexts where their preferred reading is subordinated — trans people
 *   face exclusion and medical gatekeeping in sports/medical contexts;
 *   cisgender women face competitive displacement and space erosion in
 *   social/legal contexts. Institutional actors benefit from
 *   conflict-minimization: they avoid taking a definitive position on the
 *   kernel dispute by outsourcing the contradiction to context boundaries.
 *   This is a genuine tangled rope: it solves a real coordination problem
 *   (how to administer woman/female category across domains with conflicting
 *   stakes) but does so through asymmetric extraction that requires active
 *   enforcement to hold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.42).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.38).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Contextual Category Membership for Woman/Female (Hybrid Reading)").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, 'b5005671-8803-4c92-b4ed-537f59dde2f4').
narrative_ontology:cs_kernel_codification('b5005671-8803-4c92-b4ed-537f59dde2f4', distributed).
narrative_ontology:cs_authority_grounding('b5005671-8803-4c92-b4ed-537f59dde2f4', extraction).
narrative_ontology:cs_interpretation_layer_present('b5005671-8803-4c92-b4ed-537f59dde2f4').
narrative_ontology:cs_reading_relation('b5005671-8803-4c92-b4ed-537f59dde2f4', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5005671-8803-4c92-b4ed-537f59dde2f4', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('b5005671-8803-4c92-b4ed-537f59dde2f4', foundational, domain_differentiated_category_criteria).
narrative_ontology:cs_axiom_status(domain_differentiated_category_criteria, holdable).
narrative_ontology:cs_axiom_grounding('b5005671-8803-4c92-b4ed-537f59dde2f4', domain_differentiated_category_criteria, instrumental).
narrative_ontology:cs_axiom('b5005671-8803-4c92-b4ed-537f59dde2f4', foundational, institutional_conflict_minimization_legitimizes_compromise).
narrative_ontology:cs_axiom_status(institutional_conflict_minimization_legitimizes_compromise, holdable).
narrative_ontology:cs_axiom_grounding('b5005671-8803-4c92-b4ed-537f59dde2f4', institutional_conflict_minimization_legitimizes_compromise, conventional).
narrative_ontology:cs_reference_frame('b5005671-8803-4c92-b4ed-537f59dde2f4', pragmatic_domain_sensitivity).
narrative_ontology:cs_drift_state('b5005671-8803-4c92-b4ed-537f59dde2f4', contemporary_kernel_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b5005671-8803-4c92-b4ed-537f59dde2f4', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, healthcare_institutions).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, legal_administrative_state).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, employer_compliance_departments).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_in_sports_medical_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_men_in_sports_medical_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cisgender_women_in_social_legal_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, gender_nonconforming_people_in_all_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, intersex_people_in_all_contexts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, trans_advocacy_organizations).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, gender_critical_feminist_organizations).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, contextual_sensitivity_principle).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, domain_specific_policy_design).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer sex-based clinical protocols, research categories, and insurance billing codes. Benefit from stable biological categories for medical safety and research validity. Bear costs of maintaining dual recording systems (sex assigned at birth + gender identity) and navigating context boundaries in transitional care. Collect institutional legitimacy from appearing to balance competing claims.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, healthcare_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Set eligibility criteria for female competition categories based on testosterone thresholds and puberty-based rules. Benefit from appearing to protect female sport while avoiding blanket exclusion lawsuits. Bear costs of scientific uncertainty, legal challenges from both sides, and athlete harm from invasive testing. Their exit is constrained by Olympic charter obligations and national government funding.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Operates dual-track recognition: sex-based exceptions in equality law (prisons, refuges, sports) alongside gender recognition certificates for legal gender change. Benefits from conflict-minimization: avoids definitive ruling on what 'woman' means by outsourcing to context. Bears costs of administrative complexity, contradictory court rulings, and legislative gridlock. Can shift context boundaries through regulation and guidance.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, legal_administrative_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Implement workplace policies for single-sex facilities, equality monitoring, and anti-discrimination compliance. Benefit from clear (if contradictory) regulatory frameworks that reduce litigation risk. Bear costs of policy complexity, employee conflict, and reputational exposure. Can exit by adopting maximalist policies (all self-ID or all biology) but face legal risk either way.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, employer_compliance_departments, agenda_setter,
    organized, biographical, mobile, local).

% Face eligibility bans, testosterone suppression requirements, and invasive verification in sports; face gatekeeping, pathologization, and denial of gender-affirming care in medical contexts. Their identity is fused with the category 'woman' — exit from the constraint means surrendering their self-understanding or abandoning sport/healthcare. Extraction takes the form of exclusion from participation and medical autonomy.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women_in_sports_medical_contexts, payer,
    powerless, biographical, identity_locked, national).

% Face forced competition in female categories despite male physiology (if pre-transition) or exclusion from male categories (if post-transition); face erasure in reproductive healthcare, pregnancy care, and sex-based screening. Their identity is fused with the category 'man' — the hybrid reading renders them illegible in both context regimes. Extraction takes the form of institutional invisibility and misclassification.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_men_in_sports_medical_contexts, payer,
    powerless, biographical, identity_locked, national).

% Experience competitive displacement in sports where self-ID operates without biological criteria; face erosion of single-sex spaces (prisons, refuges, changing rooms) where gender identity governs access; lose sex-based data collection and equality monitoring. Their exit is constrained: they cannot leave the legal/social system, and advocacy for sex-based rights faces institutional capture. Extraction takes the form of lost protections and category dilution.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cisgender_women_in_social_legal_contexts, payer,
    moderate, biographical, constrained, national).

% Are rendered illegible by both context regimes: biology-reading contexts force them into sex-assigned categories; identity-reading contexts force them into binary gender categories. Neither context recognizes non-binary or genderqueer existence. They bear administrative erasure, denial of appropriate healthcare, and exclusion from single-sex spaces that don't match their assigned sex. No exit exists within the binary contextual framework.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_nonconforming_people_in_all_contexts, payer,
    powerless, biographical, trapped, national).

% Biological sex characteristics don't fit binary categories, so both context regimes misclassify them. Sports bodies impose testosterone regulations targeting intersex women; medical systems perform non-consensual infant surgeries to enforce binary sex; legal systems require binary sex markers. They bear bodily autonomy violations, athletic exclusion, and legal non-recognition across all contexts. The hybrid reading offers no accommodation for intersex existence.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, intersex_people_in_all_contexts, payer,
    powerless, biographical, trapped, national).

% Gain partial victories in social/legal recognition contexts (self-ID laws, gender recognition reform) while conceding sports/medical exclusions. Benefit from institutional access and funding in identity-governed domains. Bear costs of internal division over hybrid compromise vs. monist demands. Their position is mobile: they can pivot toward full self-ID advocacy or accept contextual compromise.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Gain partial victories in sports/medical contexts (sex-based eligibility, single-sex exemptions) while conceding social/legal recognition. Benefit from platform access and alliances with conservative institutions. Bear costs of political isolation from mainstream feminism and LGBTQ+ coalitions. Their position is mobile: they can pivot toward full biology-based advocacy or accept contextual compromise.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_critical_feminist_organizations, beneficiary,
    organized, biographical, mobile, national).

% Analyze the hybrid reading's coherence, its track record in courts and policy, and its effects on the constituencies it governs. They do not bear extraction nor collect rents; they produce the interpretive frameworks that institutional agenda-setters draw on. Their exit is analytical: they can change their reading without material cost.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, bioethics_and_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Administers the woman/female category across institutional domains with materially different stakes: biological sex matters for fair competition, medical safety, and bodily vulnerability; gender identity matters for dignity, self-determination, and non-discrimination. The hybrid reading coordinates by assigning each domain its 'appropriate' criterion rather than forcing a single criterion everywhere.
% TRANSFER_FUNCTION: Moves compliance costs and category access between constituency groups depending on context: in sports/medical/safety contexts, trans people transfer access and autonomy to cisgender women and institutional gatekeepers; in social/legal contexts, cisgender women transfer category integrity and single-sex protections to trans people and institutional administrators. Institutions transfer conflict-resolution burden to context boundaries.
% ABSENT_VOICES: Intersex people are structurally excluded from both context regimes — the binary contextual split has no category for them. Detransitioners and desisters are absent from policy design despite bearing unique costs from both context regimes. Global South feminist and trans movements are excluded from the Anglo-American institutional frameworks that dominate this constraint's design. Children and adolescents (subject to puberty blockers, sports bans, social transition policies) have no organized voice in the constraint's administration.
% DISAPPEARANCE_RATIONALE: If the hybrid contextual reading vanished overnight, institutions would be forced to choose between monist regimes: either biology-only (sex_biology_reading) or identity-only (gender_identity_reading). Sports federations would lose their compromise eligibility rules; healthcare systems would lose dual recording; legal systems would lose sex-based exceptions alongside gender recognition. The world would rearrange around a definitive kernel adjudication — which is exactly what institutional beneficiaries have structured the hybrid reading to avoid.
% FOUNDING_PROBLEM: Late 20th/early 21st century institutional systems faced escalating conflict between sex-based feminism (demanding biological criteria for woman/female category) and trans rights movements (demanding self-identification criteria). Courts, legislatures, and regulatory bodies needed a way to administer the category across domains without taking a definitive position on the metaphysical question 'what is a woman?'. The hybrid reading emerged as a pragmatic compromise: contextual sensitivity as conflict-minimization.
% FOUNDING_PROBLEM_CORROBORATION: The hybrid reading's founding problem is attested by policy historians (e.g., UK Gender Recognition Act 2004 parliamentary debates, US Title IX regulatory history, IOC Stockholm consensus 2003) from outside the beneficiary set. However, trans advocates corroborate that the problem was manufactured by institutional refusal to recognize self-ID; gender-critical feminists corroborate that the problem was manufactured by institutional capture of sex-based rights. No neutral corroborator exists — the founding problem itself is a site of kernel contest.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).
:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the constraint distributes costs across both constituency groups rather than concentrating on one — but it is rising as context boundaries become enforcement frontiers. Suppression (0.38) is significant but not maximal: the constraint operates through institutional gatekeeping (eligibility rules, documentation requirements, access controls) rather than direct physical coercion, but non-compliance carries real penalties (exclusion from competition, denial of care, legal non-recognition). Theater ratio (0.28) reflects that the contextual distinction performs genuine coordination work (domains really do have different stakes) but a growing share of enforcement activity serves to maintain the boundary itself rather than solve domain-specific problems. Accessibility collapse (0.45) is moderate: alternatives (self-ID everywhere, biology everywhere, third categories) exist and are advocated but are institutionally marginalized. Resistance (0.52) is high: all three readings have organized advocacy networks, litigation strategies, and political mobilization. The measurement series shows extraction and suppression rising over 2010-2024 as the kernel dispute intensifies and institutional actors harden context boundaries rather than resolve them.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional beneficiary seat, the hybrid reading appears as pragmatic compromise solving real domain-differentiated problems — a rope-like coordination. From trans women in sports/medical contexts, it appears as biology-essentialist exclusion — a snare. From cisgender women in social/legal contexts, it appears as identity-ideological erosion — a snare. From gender nonconforming people, it appears as binary enforcement — a snare. The engine computes these divergences from the structural data; the authoring seat declares the constraint is structurally a tangled rope because the coordination function (domain-differentiated administration) is real AND the extraction is asymmetric across constituencies.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional actors (healthcare, sports bodies, legal state, employers) are structural beneficiaries: they gain conflict-minimization, administrative coherence, and avoidance of definitive kernel adjudication — their directionality d is low (near beneficiary end). Trans women in sports/medical contexts are primary targets: they bear exclusion, medicalization, and eligibility policing — d is high (near target end). Trans men in same contexts face parallel but distinct extraction (invisibility, forced misclassification). Cisgender women in social/legal contexts are secondary targets: they experience competitive displacement and space erosion where self-ID operates — d is moderately high. Gender nonconforming and intersex people are targets across ALL contexts: the binary contextual split renders them illegible in both domains — d is highest. The engine will compute per-seat χ from these structural positions; the claimed type (tangled_rope) reflects the authoring seat's assessment that genuine coordination and asymmetric extraction coexist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administering woman/female category across domains with materially different stakes) remains live — the domains have not converged and the stakes remain divergent. But the hybrid solution has accumulated extraction: what began as pragmatic domain-sensitivity has become a mechanism for institutions to avoid accountability while extracting compliance costs from both sides. The mandatrophy is not resolved; the arrangement persists because no institutional actor bears the cost of fixing it (would require definitive kernel adjudication), and both constituency groups are too divided to force resolution. The theater ratio rise signals Goodhart drift: the context boundary itself becomes the managed metric, not the domain outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''woman_female_category'', or does it instantiate a structurally distinct constraint?',
    'Compare ε values and victim/beneficiary structures across the three declared readings (sex_biology_reading, gender_identity_reading, hybrid_contextual_reading). If ε and structural roles differ materially, they are distinct constraints per DP-001 ε-invariance.',
    'If distinct constraints, each gets its own JSON file linked via network.affects_constraints. If same constraint under different observables, DP-001 is violated — the label conflates structurally distinct claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the hybrid contextual reading is a distinct constraint from the biology and identity readings per ε-invariance').

omega_variable(
    sibling_reading_foreclosure_structure,
    'Does the hybrid contextual reading logically foreclose the sex biology reading, the gender identity reading, or neither — and in what structural sense?',
    'Assess whether a single institutional framework could simultaneously hold: (a) biological sex determines category membership in medical/sports/safety contexts, AND (b) gender identity determines it in social/legal contexts, alongside a framework that holds (c) biological sex determines it in ALL contexts, or (d) gender identity determines it in ALL contexts. Test logical compatibility, not political coexistence.',
    'If hybrid contextual reading coexists_with both siblings, the kernel is a genuine pluralist dispute. If it forecloses one or both, the kernel has an internal logical hierarchy. This determines cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_structure, conceptual, 'Logical relationship between hybrid contextual reading and sibling readings — foreclosure vs coexistence').

omega_variable(
    institutional_beneficiary_extraction_boundary,
    'Do the declared institutional beneficiaries (healthcare institutions, sports bodies, legal state, employers) actually capture the extraction, or do they merely administer a compromise that extracts from both constituency groups?',
    'Trace resource flows: do these institutions gain budget, authority, or legitimacy from maintaining the hybrid regime, or do they bear net costs of administering dual standards? Compare to the piton pattern: administrator could change it but cost to fix exceeds what it bears.',
    'If institutions capture extraction → gain_flow names a seat (snare-flavored). If they administer at net cost → gain_flow = ''diffuse'' (piton candidate). This determines receipt surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_extraction_boundary, empirical, 'Whether institutional actors are net beneficiaries or net administrators of the hybrid regime').

omega_variable(
    context_boundary_stability,
    'Are the context boundaries (medical/sports/safety vs social/legal) structurally stable, or do they bleed into each other creating a de facto single-context regime?',
    'Track litigation and policy disputes over boundary cases: prison housing, shelter access, school facilities, medical transition for minors, data collection categories. If boundaries are litigated to collapse, the hybrid reading de facto becomes one of the monist readings.',
    'If boundaries collapse toward biology-only → hybrid reading converges on sex_biology_reading. If toward identity-only → converges on gender_identity_reading. If boundaries hold → hybrid reading is structurally stable as a distinct constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(context_boundary_stability, empirical, 'Whether the context boundaries that define this reading''s structure are stable or eroding').

omega_variable(
    trans_women_victim_heterogeneity,
    'Do trans women experience the same extraction across medical, sports, and safety contexts, or do different sub-groups (post-transition, non-medical-transition, youth) face qualitatively different victimization?',
    'Disaggregate victim set by transition status, age, and context. A monolithic ''trans_women_in_sports_medical_contexts'' victim group may mask distinct constraint operations (e.g., testosterone suppression requirements vs. blanket exclusion vs. documentation barriers).',
    'If victim experience is heterogeneous, this single constraint story may need decomposition per ε-invariance (different ε for different victim sub-groups). If homogeneous, single story is adequate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trans_women_victim_heterogeneity, empirical, 'Whether the trans women victim group is structurally homogeneous or requires decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2010, woman_female_category__hybrid_contextual_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__hybrid_contextual_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(woma_tr_t2020, woman_female_category__hybrid_contextual_reading, theater_ratio, 2020, 0.23).
narrative_ontology:measurement(woma_tr_t2022, woman_female_category__hybrid_contextual_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__hybrid_contextual_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t2010, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2010, 0.22).
narrative_ontology:measurement(woma_be_t2015, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(woma_be_t2020, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(woma_be_t2022, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2022, 0.39).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2010, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(woma_su_t2015, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2015, 0.22).
narrative_ontology:measurement(woma_su_t2020, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2020, 0.31).
narrative_ontology:measurement(woma_su_t2022, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint family (woman_female_category kernel) decomposes the colloquial label 'woman/female category' into three structurally distinct constraints per ε-invariance: (1) sex_biology_reading — ε≈0.15 (low extraction, high naturalness claim), Mountain candidate; (2) gender_identity_reading — ε≈0.35 (moderate extraction, institutional capture of self-ID), Tangled Rope; (3) hybrid_contextual_reading — ε=0.42 (moderate-high extraction, dual victim sets, active enforcement), Tangled Rope. The hybrid reading's ε is highest because it operates active enforcement across TWO context regimes, extracting from both constituency groups. The biology reading influences the hybrid reading (medical/sports contexts import biology-reading logic); the identity reading influences the hybrid reading (social/legal contexts import identity-reading logic). The hybrid reading influences both siblings by legitimizing context boundaries as a structural solution, reducing pressure for monist resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, institutional, 0.15).
constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, powerless, 0.85).
constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
