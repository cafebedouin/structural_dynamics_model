% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Sex-Based Category Membership (Biological Markers Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'gendered category
 *   membership' kernel: the biological-sex reading, which grounds category
 *   membership (specifically 'woman'/'man') in chromosomal and
 *   reproductive-anatomy markers fixed at birth. Under this reading,
 *   sex-segregated spaces, sports categories, and legal sex markers are
 *   correctly administered when sorted by these immutable biological facts,
 *   and incorrectly administered when sorted by self-declared gender identity
 *   or social role. The reading has intensified in political salience and
 *   enforcement infrastructure over the past three decades as trans
 *   visibility and legal gender recognition have expanded, producing
 *   escalating contestation over sports eligibility, prison placement, and
 *   single-sex space access. This is NOT a story about which reading is
 *   correct — it is a clean instantiation of what the biological-sex reading
 *   structurally is, at its own ε. The sibling readings
 *   (gender_identity_reading, social_role_reading) are separate constraint
 *   stories with their own ε values, beneficiary/victim structures, and
 *   classifications; they are not blended into this one.
 *
 * KEY AGENTS:
 *   - trans_women: primary target (powerless/identity_locked) — excluded from category regardless of transition status
 *   - intersex_people_with_ambiguous_markers: secondary target (powerless/trapped) — bodies do not sort cleanly, forced into binary anyway
 *   - cis_women_in_sex_segregated_spaces: primary beneficiary (organized/constrained) — retains access premised on the boundary
 *   - sports_governance_bodies: institutional agenda-setter (institutional/arbitrage) — administers and profits from the boundary's continuation
 *   - biological_essentialist_advocacy_groups: political agenda-setter (organized/mobile) — organizational purpose partly constituted by defending the boundary
 *   - clinical_and_developmental_biologists: analytical observer (analytical/analytical) — documents biological variability that complicates the binary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.68).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.71).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Sex-Based Category Membership (Biological Markers Reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '3eb41e41-3749-4d34-add1-b252ffa9c909').
narrative_ontology:cs_kernel_codification('3eb41e41-3749-4d34-add1-b252ffa9c909', distributed).
narrative_ontology:cs_authority_grounding('3eb41e41-3749-4d34-add1-b252ffa9c909', distributed).
narrative_ontology:cs_reading_relation('3eb41e41-3749-4d34-add1-b252ffa9c909', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('3eb41e41-3749-4d34-add1-b252ffa9c909', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('3eb41e41-3749-4d34-add1-b252ffa9c909', foundational, birth_anatomy_determines_permanent_category).
narrative_ontology:cs_axiom_status(birth_anatomy_determines_permanent_category, holdable).
narrative_ontology:cs_axiom_grounding('3eb41e41-3749-4d34-add1-b252ffa9c909', birth_anatomy_determines_permanent_category, empirically_contingent).
narrative_ontology:cs_axiom('3eb41e41-3749-4d34-add1-b252ffa9c909', secondary, self_declared_identity_cannot_alter_category_membership).
narrative_ontology:cs_axiom_status(self_declared_identity_cannot_alter_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('3eb41e41-3749-4d34-add1-b252ffa9c909', self_declared_identity_cannot_alter_category_membership, conventional).
narrative_ontology:cs_reference_frame('3eb41e41-3749-4d34-add1-b252ffa9c909', chromosomal_birth_anatomy_binary).
narrative_ontology:cs_drift_state('3eb41e41-3749-4d34-add1-b252ffa9c909', contemporary_legal_medical_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3eb41e41-3749-4d34-add1-b252ffa9c909', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women_in_sex_segregated_spaces).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, sports_governance_bodies).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, biological_essentialist_advocacy_groups).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_people_with_ambiguous_markers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Excluded from the 'woman' category as defined by this reading regardless of legal transition, medical treatment, or years of lived social identity as women. Barred from women's shelters, prisons, sports categories, and single-sex spaces administered under this standard. Cannot exit the classification by any action available to them since it is fixed at birth by definition; the only escape is a category the reading does not recognize as available to them.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Their chromosomal and anatomical markers do not sort cleanly into the binary the reading requires, yet administrative and medical systems built on this standard force an assignment. They bear the cost of a category system that treats their actual biological condition as an edge case to be resolved rather than as evidence against the binary's completeness.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_people_with_ambiguous_markers, payer,
    powerless, biographical, trapped, national).

% Use single-sex shelters, prisons, sports categories, and changing facilities premised on a strict biological boundary. Some organize politically to defend the boundary, citing safety, fairness in athletic competition, and privacy. Their access to these spaces depends on the boundary being maintained and enforced against reclassification claims.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women_in_sex_segregated_spaces, beneficiary,
    organized, generational, constrained, national).

% Set eligibility rules for competitive categories using chromosomal or birth-anatomy tests, administer verification procedures, and adjudicate disputes. Derive legitimacy and continued authority from being the body that draws this line; changing the standard would require rebuilding their entire eligibility apparatus and would expose past rulings to challenge.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sports_governance_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, sports_governance_bodies, beneficiary).

% Campaign to enshrine this reading in law and policy, framing it as protecting women's sex-based rights. Gain political capital, funding, and institutional standing from maintaining the boundary as a live and contested political issue; their organizational purpose is partly constituted by this fight continuing.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, biological_essentialist_advocacy_groups, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, biological_essentialist_advocacy_groups, beneficiary).

% Administer birth certificates, ID documents, and medical intake forms that encode the biological marker standard. Apply the classification rule as given, with limited discretion to deviate even where it produces outcomes they may find administratively awkward or ethically uncomfortable.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, medical_and_legal_gatekeepers, agenda_setter,
    institutional, biographical, constrained, national).

% Argue category membership should track gender identity, not birth anatomy, and are typically not granted standing in the rule-making bodies (sports federations, some legislatures) that set the biological-marker standard. Their objections are heard in courts and public discourse but rarely inside the administering institutions themselves.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_rights_organizations, excluded,
    organized, biographical, mobile, national).

% Study the actual variability of sex-linked biological traits, including intersex variation and the incomplete concordance between chromosomes, gonads, hormones, and secondary characteristics. Their empirical findings complicate the binary this reading treats as clean and immutable, without directly ruling in the political dispute.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, clinical_and_developmental_biologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively simple sorting rule for allocating access to spaces and categories understood to require single-sex separation (competitive sport tiers, custodial facilities, intimate-care spaces) using a marker fixed at birth and easy to record on official documents.
% TRANSFER_FUNCTION: Moves continued access to sex-segregated spaces, competitive eligibility, and legal/social recognition as one's felt sex away from trans and some intersex people and toward maintenance of a fixed boundary that cis women's advocacy groups and sports bodies rely on for their organizing rationale.
% ABSENT_VOICES: Trans women and trans rights organizations are largely excluded from the rule-making bodies (sports federations, legislative drafting committees) that set and enforce the biological-marker standard; intersex advocates are rarely consulted despite the standard's direct dependence on their bodies being sortable.
% DISAPPEARANCE_RATIONALE: If the biological-marker standard were abandoned overnight, sex-segregated space administration, sports eligibility categories, prison assignment protocols, and vital-records systems built on it would all require immediate re-specification; some cis women's organizations would lose their primary organizing claim to space allocation on this exact ground, while trans women would gain access to spaces and categories currently closed to them.
% FOUNDING_PROBLEM: Historically, sex classification at birth solved practical problems: assigning children to sex-linked medical care pathways, establishing legal identity, and later, sorting people into spaces and competitions understood to require single-sex separation for privacy or fairness reasons.
% FOUNDING_PROBLEM_CORROBORATION: Clinical and developmental biologists outside the advocacy dispute attest that the underlying biological reality (chromosomes, gonads, hormones, secondary characteristics) does not concord as cleanly as the standard assumes, which undercuts the reading's claim that the sorting problem it solves is as settled as its administration implies; medical historians note the standard's administrative origins predate contemporary disputes over its application to a much wider set of social goods than vital-records-keeping alone.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 at interval end: substantial but not maximal, because the reading does provide genuine, non-fabricated coordination value for some administrative purposes (birth-record consistency, some medical contexts) even as it is deployed extensively to exclude trans women from categories central to social participation and safety. Suppression (0.71) is authored higher than extractiveness because maintaining the boundary against a growing, legally-organized challenge (trans rights litigation, shifting medical consensus, legislative contestation) requires increasingly active enforcement — legal defense funds, verification testing regimes, and legislative campaigns — not passive acceptance. Theater ratio is comparatively low (0.28): most enforcement activity is substantively directed at maintaining the boundary's function (real space-allocation and eligibility decisions), not merely performative, though a growing share of political advocacy activity is more symbolic/mobilizing than administratively necessary. Accessibility collapse (0.62) reflects that alternatives (identity-based or role-based classification) are visible and litigated but not fully available within this reading's own institutions — a trans woman cannot simply opt into the biological category from outside it. Resistance (0.74) is high and rising, consistent with a claimed coordination mechanism meeting substantial organized opposition from trans rights organizations, medical bodies revising practice guidance, and courts in several jurisdictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and intersex people with ambiguous markers are declared victims: the classification rule extracts from them by fixing their category membership at a point (birth) they cannot revisit, foreclosing access to spaces and competitive categories aligned with their lived identity. This yields high directionality (d approaching the full-target end) — they are structurally locked into the excluded category with no exit available through their own action. Cis women in sex-segregated spaces and sports governance bodies are beneficiaries: the former retain access to spaces organized around the boundary, the latter retain administrative authority and legitimacy from being the standard's enforcer. Both sit near the beneficiary end of directionality. Biological essentialist advocacy groups are also beneficiaries in a more diffuse but still real sense — the ongoing dispute is a source of organizational purpose and political capital, not merely a passive good they receive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (needing SOME administrative sorting rule for birth records and sex-linked medical pathways) is genuinely live in narrow contexts (perinatal medicine, some vital-statistics functions) but the reading's status is contested precisely because its scope has expanded far beyond that founding function into sports eligibility, custodial facility assignment, and broad legal-sex determination — domains the founding administrative problem did not originally require settling. The tangled_rope classification captures this: there IS a real coordination function (some administrative sorting purposes are genuinely served by a birth-fixed marker), and there IS asymmetric extraction (trans women and intersex people bear costs the coordination function does not require imposing on them), and it DOES require active, intensifying enforcement to hold against a legally and medically shifting landscape. Classifying this as a pure snare would erase the genuine, narrow administrative coordination value; classifying it as a pure rope would erase the documented, escalating cost borne by excluded groups. The tangled_rope reading holds both facts open at once.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_marker_naturalness_vs_construction,
    'Is the biological-marker sorting rule a discovery of a natural, pre-existing categorical fact, or a constructed administrative convention that has been naturalized through long institutional use?',
    'Comparative analysis of intersex variation rates and the actual discordance between chromosomal, gonadal, hormonal, and phenotypic sex markers; historical analysis of when and why binary vital-records sorting was adopted as policy versus discovered as biological necessity.',
    'If the marker is substantially conventional rather than a clean natural fact, the reading''s claim to describe an immutable, naturally occurring boundary is weakened, and the classification shifts further toward extraction dressed as natural-law description; if the marker tracks a genuinely robust natural regularity for the overwhelming majority of cases, the coordination-function component of the tangled_rope reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_marker_naturalness_vs_construction, empirical, 'Whether the biological marker is a natural-kind fact or a naturalized administrative convention.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where, structurally, do the three sibling readings of the gendered_category_membership kernel actually disagree — is it about what sex/gender IS (an ontological dispute) or about what administrative purposes classification should serve (a policy dispute dressed as ontology)?',
    'Systematic comparison of the three reading-stories'' beneficiary/victim structures and ε values: if the disagreement were purely administrative, the readings would converge on similar ε profiles applied to different named populations; the observed divergence (trans women as victims here, versus cis women''s-space advocates as potential imposers of cost under the gender_identity_reading) suggests the dispute is substantially ontological, not merely administrative.',
    'If the dispute is substantially ontological, no amount of administrative compromise (e.g., context-specific carve-outs) fully resolves the underlying disagreement; if substantially administrative, targeted policy solutions (e.g., differentiated space-allocation rules by context) could resolve most of the practical conflict while leaving the ontological dispute open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s sibling readings disagree about ontology or about administrative scope.').

omega_variable(
    intersex_evidence_weight,
    'How much should the existence of intersex variation (bodies that do not sort cleanly into the binary this reading presupposes) count against the reading''s core premise, versus being treated as a rare edge case that does not undermine the binary''s applicability to the large majority?',
    'Epidemiological data on intersex variation prevalence combined with philosophical analysis of whether a categorization scheme''s validity depends on handling ALL cases cleanly or only the modal case.',
    'If intersex variation is evidentially significant, the reading''s claim to describe an immutable binary is undermined at the conceptual level, not merely at the level of hard cases; if treated as negligible edge-case noise, the reading''s core binary premise survives largely intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_evidence_weight, conceptual, 'How much weight intersex variation carries against the reading''s binary premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__biological_sex_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__biological_sex_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__biological_sex_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__biological_sex_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__biological_sex_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__biological_sex_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__biological_sex_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__biological_sex_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__biological_sex_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__biological_sex_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__biological_sex_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__biological_sex_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__biological_sex_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__biological_sex_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__biological_sex_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__biological_sex_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__biological_sex_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__biological_sex_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gendered_category_membership kernel. biological_sex_reading (this story) authors category membership as fixed at birth by biological markers, yielding trans women and ambiguous-marker intersex people as victims and cis women's-space advocates plus sports governance bodies as beneficiaries. gender_identity_reading authors category membership as grounded in self-declared identity, inverting much of the victim/beneficiary structure. social_role_reading grounds membership in sustained social performance and recognition, producing a third distinct structure emphasizing transition-period liminality and recognition-withholding as the extraction mechanism. Each story carries its own stable ε per the ε-invariance principle; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
