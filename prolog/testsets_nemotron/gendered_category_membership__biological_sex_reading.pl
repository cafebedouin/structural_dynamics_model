% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological-Sex Grounded Category Membership (Woman/Female)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the biological_sex_reading of the
 *   contested kernel 'gendered_category_membership.' It asserts that
 *   membership in the category 'woman' is grounded exclusively in immutable
 *   biological markers — chromosomes (XX), reproductive anatomy at birth, and
 *   gamete production capacity. The reading operates as a tangled rope: it
 *   performs a genuine coordination function (stable, administrable category
 *   boundaries for sex-based rights and single-sex spaces) while
 *   simultaneously extracting from transgender women, non-binary people, and
 *   intersex people through categorical exclusion, denial of legal
 *   recognition, and enforcement of binary boundaries. The constraint has
 *   hardened over the interval 1970-2025: extractiveness rose from 0.35 to
 *   0.78 as the boundary became legally codified and politically weaponized;
 *   suppression requirement climbed from 0.45 to 0.82 as enforcement expanded
 *   from administrative policy to criminal law (e.g., bathroom bills, sports
 *   bans, prison placement statutes); theater ratio grew modestly as the
 *   coordination rationale ('protecting women') became increasingly
 *   performative relative to the exclusionary function. Cisgender women are
 *   positioned as primary beneficiaries but also bear mobilization costs;
 *   transgender women are identity-locked targets; intersex people are
 *   trapped by biological variation that falsifies the premise.
 *
 * KEY AGENTS:
 *   - cisgender_women: Primary beneficiaries (organized/constrained) — gain protected category but pay mobilization costs
 *   - transgender_women: Primary victims (moderate/identity_locked) — categorically excluded, identity denied
 *   - non_binary_people_assigned_male_at_birth: Victims (powerless/identity_locked) — erased by binary logic
 *   - intersex_people_with_atypical_chromosomal_or_anatomical_profiles: Victims (powerless/trapped) — biological variation contradicts premise
 *   - female_sex_based_rights_advocates: Agenda setters (organized/mobile) — author the boundary
 *   - institutional_gatekeepers_of_single_sex_spaces: Agenda setters (institutional/constrained) — enforce the boundary
 *   - gender_identity_rights_advocates: Excluded (organized/mobile) — locked out of boundary-setting
 *   - bioethics_and_human_rights_scholars: Observers (analytical/analytical) — analytical surface
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.78).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.82).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological-Sex Grounded Category Membership (Woman/Female)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, 'c507133c-ee37-4007-9f7b-006b9ae45050').
narrative_ontology:cs_kernel_codification('c507133c-ee37-4007-9f7b-006b9ae45050', distributed).
narrative_ontology:cs_authority_grounding('c507133c-ee37-4007-9f7b-006b9ae45050', extraction).
narrative_ontology:cs_interpretation_layer_present('c507133c-ee37-4007-9f7b-006b9ae45050').
narrative_ontology:cs_reading_relation('c507133c-ee37-4007-9f7b-006b9ae45050', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('c507133c-ee37-4007-9f7b-006b9ae45050', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('c507133c-ee37-4007-9f7b-006b9ae45050', foundational, woman_category_membership_requires_female_biological_sex).
narrative_ontology:cs_axiom_status(woman_category_membership_requires_female_biological_sex, holdable).
narrative_ontology:cs_axiom_grounding('c507133c-ee37-4007-9f7b-006b9ae45050', woman_category_membership_requires_female_biological_sex, deontological).
narrative_ontology:cs_axiom('c507133c-ee37-4007-9f7b-006b9ae45050', secondary, sex_based_rights_are_incoherent_without_immutable_boundary).
narrative_ontology:cs_axiom_status(sex_based_rights_are_incoherent_without_immutable_boundary, holdable).
narrative_ontology:cs_axiom_grounding('c507133c-ee37-4007-9f7b-006b9ae45050', sex_based_rights_are_incoherent_without_immutable_boundary, instrumental).
narrative_ontology:cs_reference_frame('c507133c-ee37-4007-9f7b-006b9ae45050', second_wave_feminist_legal_ontology).
narrative_ontology:cs_drift_state('c507133c-ee37-4007-9f7b-006b9ae45050', contemporary_gender_identity_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c507133c-ee37-4007-9f7b-006b9ae45050', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, female_sex_based_rights_advocates).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, institutional_gatekeepers_of_single_sex_spaces).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, non_binary_people_assigned_male_at_birth).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_people_with_atypical_chromosomal_or_anatomical_profiles).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, sex_binary_is_immutable_and_exhaustive).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, female_sex_based_rights_require_category_boundary_enforcement).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, biological_sex_is_the_sole_legitimate_basis_for_woman_category_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain protected single-sex spaces, sports categories, and legal recognition grounded in biological sex; simultaneously bear costs of ongoing political mobilization to defend the boundary, reputational risk from being labeled exclusionary, and the cognitive load of maintaining a category definition under sustained challenge.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_women, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cisgender_women, payer).

% Are categorically excluded from the 'woman' category and associated single-sex spaces, services, and legal protections; face denial of identity recognition, heightened vulnerability to violence and discrimination, and the psychological burden of being told their self-understanding is invalid. Exit from the constraint is identity_locked because gender identity is constitutive of selfhood — one cannot 'leave' one's gender any more than one can leave one's memory.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_women, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, transgender_women, excluded).

% Fall outside the binary entirely but are forced into the male/man category by the constraint's logic; experience erasure of their gender, denial of access to any gender-affirming category or space, and compounded marginalization from both binary poles.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, non_binary_people_assigned_male_at_birth, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, non_binary_people_assigned_male_at_birth, excluded).

% Biological variation directly contradicts the immutable binary marker premise; are either coercively assigned to a binary category or rendered category-less, with medical histories often used to police boundaries they had no hand in drawing.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_people_with_atypical_chromosomal_or_anatomical_profiles, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, intersex_people_with_atypical_chromosomal_or_anatomical_profiles, excluded).

% Organize politically and legally to enshrine biological sex as the sole basis for the 'woman' category; draft legislation, litigate test cases, and shape institutional policy. Their exit is mobile — they can shift advocacy focus — but their institutional position depends on maintaining the boundary.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, female_sex_based_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Administer prisons, shelters, sports leagues, hospitals, and changing rooms; must implement and enforce the biological boundary or face legal liability and political pressure. Their role is coerced by the constraint's enforcement regime — they are the mechanism, not the author.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, institutional_gatekeepers_of_single_sex_spaces, agenda_setter,
    institutional, biographical, constrained, national).

% Advocate for self-declaration as the basis of category membership; are systematically locked out of the legislative and judicial venues where the biological-sex reading writes the rules. Their exclusion is structural — the constraint's coherence depends on their non-participation in boundary-setting.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_rights_advocates, excluded,
    organized, generational, mobile, global).

% Analyze the constraint's coherence, its human rights implications, and its empirical claims about biological essentialism; provide the analytical surface the engine reads from.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, bioethics_and_human_rights_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social recognition, legal rights, and resource allocation for the class of adult human females by anchoring the category 'woman' in a putatively objective, immutable biological criterion — avoiding the coordination costs of negotiated or subjective membership.
% TRANSFER_FUNCTION: Transfers category membership, access to single-sex spaces, sports eligibility, legal protections, and epistemic authority from transgender women, non-binary people, and intersex people to cisgender women and the institutions that gatekeep the boundary; the extraction is the denial of recognition and access to the excluded.
% ABSENT_VOICES: Transgender women, non-binary people, and intersex people are the primary excluded voices — they would object to the biological essentialism that erases their lived reality, but the constraint's enforcement architecture (legislative, judicial, institutional) is built precisely to keep them from authoring the boundary. Gender identity rights advocates are also excluded from the boundary-setting table.
% DISAPPEARANCE_RATIONALE: If the biological-sex reading vanished overnight, legal frameworks for single-sex spaces would collapse into contestation; sports governing bodies would lose their current eligibility criteria; prison and shelter placement policies would become unresolvable without a new coordination mechanism; the political coalition around 'sex-based rights' would lose its unifying anchor. The world would rearrange — but toward what is the contested terrain of the kernel.
% FOUNDING_PROBLEM: The need for a stable, administrable, and politically defensible boundary for the category 'woman' that could ground sex-based legal protections, single-sex spaces, and affirmative action without relying on subjective self-identification — which was seen as vulnerable to strategic capture and administrative chaos.
% FOUNDING_PROBLEM_CORROBORATION: Second-wave feminist legal scholars (e.g., Catharine MacKinnon, Carole Pateman) and early sex-based rights advocates attest the problem was live and the biological boundary was the chosen solution. Contemporary trans rights advocates, queer theorists, and human rights bodies (e.g., UN Independent Expert on SOGI) attest the founding problem was misdiagnosed — the instability was not in self-identification but in the binary itself — and that the 'solution' created the very exclusion it now treats as natural.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The constraint is claimed as tangled_rope because it has a real coordination function (stable category for sex-based rights) AND asymmetric extraction (trans women, non-binary people, intersex people pay the cost of that stability through exclusion). The metrics are authored independently: extractiveness 0.78 reflects the severe denial of recognition and access; suppression 0.82 reflects the legal and institutional machinery required to maintain the binary against empirical counterevidence (intersex existence, trans persistence) and political contestation; theater_ratio 0.28 reflects that the protective rhetoric is real but increasingly decoupled from the exclusionary mechanism; accessibility_collapse 0.62 reflects that alternatives (self-ID, social role) exist and function in many jurisdictions but are actively suppressed; resistance 0.71 reflects sustained, organized opposition from trans rights movements, human rights bodies, and medical establishments.
 *
 * PERSPECTIVAL GAP:
 *   From the cisgender_women seat (organized, constrained exit), the constraint appears as necessary coordination — the only stable anchor for sex-based rights. From the transgender_women seat (moderate, identity_locked), it appears as a snare — total exclusion enforced by state power. From the institutional_gatekeepers seat (institutional, constrained), it appears as a rope they are forced to administer — they didn't choose the boundary but must enforce it. From the analytical seat, the divergence is the measurement: the same structure computes differently depending on whether the agent's identity is fused with the category (identity_locked) or mobile enough to treat it as instrumental.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: cisgender_women (gain category stability, single-sex spaces), female_sex_based_rights_advocates (gain political coherence), institutional_gatekeepers (gain administrable rules). Victims declared: transgender_women (categorical exclusion, identity denial), non_binary_people_assigned_male_at_birth (erasure), intersex_people (biological falsification of premise). The directionality derivation chain reads: cis women are beneficiaries with constrained exit → d lowered; trans women are victims with identity_locked exit → d raised to near-target; intersex people are victims with trapped exit → d at maximum; institutional gatekeepers are agenda_setters with constrained exit → d moderate (they enforce but don't author). The engine computes per-seat χ from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stable administrable boundary for 'woman') was live in 1970s feminist legal theory. By 2025 it is contested: trans rights advocates argue the problem was misdiagnosed (binary itself is the instability), sex-based rights advocates argue the problem persists and the boundary is the only solution. The constraint persists not because the founding problem is solved but because the coalition that benefits from it has institutionalized the boundary and the cost of dismantling it (legal, political, cultural) is prohibitive for the agenda_setters. This is mandatrophy in the classic sense: the mandate (stable category for sex-based rights) has outlived its consensus, but the constraint remains because the beneficiaries control the enforcement machinery and the victims are identity-locked or trapped.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_binary_empirical_status,
    'Is the sex binary (chromosomes, anatomy, gametes) empirically immutable and exhaustive, or does intersex variation and the biology of sexual differentiation falsify the premise?',
    'Systematic review of intersex prevalence data, developmental biology of sexual differentiation, and the operationalization of ''biological sex'' in the constraint''s enforcement mechanisms (which markers are actually checked, when, and with what error rates).',
    'If the binary is empirically falsified, the constraint''s emerges_naturally claim collapses and its coordination function is revealed as constructed — shifting classification toward snare. If the binary holds empirically, the tangled_rope classification is stabilized (genuine coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_binary_empirical_status, empirical, 'Whether the biological premise is a natural law or a constructed category.').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (stable category for sex-based rights) be achieved without the extraction (exclusion of trans women, non-binary people, intersex people)?',
    'Natural experiment analysis: jurisdictions with self-ID laws (Argentina, Ireland, Malta, Denmark, New Zealand, Switzerland, multiple US states) — assess whether sex-based rights, single-sex spaces, and female sports categories have collapsed or adapted.',
    'If coordination is separable from extraction, the constraint is a snare masquerading as tangled rope — the extraction is gratuitous. If inseparable, the tangled_rope classification holds: the boundary''s stability genuinely requires exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    cis_women_as_beneficiaries_or_co_victims,
    'Are cisgender women net beneficiaries of this constraint, or does the biological essentialism that grounds their category membership also constrain their reproductive autonomy, gender expression, and political solidarity in ways that make them co-victims?',
    'Trace the legislative and judicial cascade: does the same biological-essentialist logic that excludes trans women also undergird abortion restrictions, IVF regulation, maternal-fetal conflict law, and the policing of gender non-conformity among cis women?',
    'If cis women are co-victims, the beneficiary declaration is a false summit — the constraint extracts from them too, just differently. The tangled_rope would need re-authoring with cis women as secondary victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_as_beneficiaries_or_co_victims, conceptual, 'Whether the primary beneficiary group also bears hidden extraction from the same logic.').

omega_variable(
    identity_locked_vs_trapped_for_trans_women,
    'Is the trans women''s exit_options correctly authored as identity_locked, or should some subset be trapped (where exit is structurally blocked but not identity-constitutive)?',
    'Distinguish between trans women for whom gender identity is constitutive of selfhood (identity_locked) and those who might detransition or disidentify under sufficient pressure (trapped) — though the latter is ethically fraught to measure.',
    'If a significant subset is trapped rather than identity_locked, their directionality d would be even higher (closer to 1.0), increasing effective extraction for that seat. The identity_locked assignment assumes gender identity is constitutive — a premise the gender_identity_reading affirms but the biological_sex_reading denies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_vs_trapped_for_trans_women, conceptual, 'Whether the identity_locked assignment correctly captures the exit structure for all trans women.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1970, gendered_category_membership__biological_sex_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(gend_tr_t1985, gendered_category_membership__biological_sex_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(gend_tr_t2000, gendered_category_membership__biological_sex_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__biological_sex_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(gend_tr_t2015, gendered_category_membership__biological_sex_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(gend_tr_t2020, gendered_category_membership__biological_sex_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(gend_tr_t2025, gendered_category_membership__biological_sex_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(gend_be_t1970, gendered_category_membership__biological_sex_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(gend_be_t1985, gendered_category_membership__biological_sex_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(gend_be_t2000, gendered_category_membership__biological_sex_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__biological_sex_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(gend_be_t2015, gendered_category_membership__biological_sex_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__biological_sex_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(gend_be_t2025, gendered_category_membership__biological_sex_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1970, gendered_category_membership__biological_sex_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(gend_su_t1985, gendered_category_membership__biological_sex_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(gend_su_t2000, gendered_category_membership__biological_sex_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__biological_sex_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(gend_su_t2015, gendered_category_membership__biological_sex_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__biological_sex_reading, suppression_requirement, 2020, 0.81).
narrative_ontology:measurement(gend_su_t2025, gendered_category_membership__biological_sex_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, single_sex_space_access__biological_sex_enforcement).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, sports_eligibility__chromosomal_testing).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, prison_placement__birth_sex_assignment).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, legal_gender_recognition__biological_certificate_requirement).

% DUAL FORMULATION NOTE:
% This constraint is one of three in the gendered_category_membership kernel family. The biological_sex_reading anchors the category in immutable biology; the gender_identity_reading anchors it in self-declaration; the social_role_reading anchors it in sustained social recognition. Their ε values differ substantially: biological_sex_reading ε=0.78 (high extraction via exclusion), gender_identity_reading ε≈0.15 (low extraction, coordination via self-ID), social_role_reading ε≈0.35 (moderate extraction via performative demands). They are linked here as structural dependents — the biological reading's legal codification creates the enforcement baseline the others must contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
