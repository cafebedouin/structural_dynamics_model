% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological-Marker Gate on Gendered Category Membership (biological sex reading)
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'gendered
 *   category membership': the biological sex reading, under which membership
 *   in 'woman'/'man' is fixed by immutable markers — chromosomes and
 *   reproductive anatomy recorded at birth — and gates to women's spaces,
 *   female sporting categories, and legal sex follow from that record. The
 *   arrangement is actively enforced through statute, registry practice,
 *   federation eligibility codes, and litigation; it delivers genuine
 *   protective and administrative coordination to many cis women while
 *   imposing identity-suppression costs on trans people and managing intersex
 *   bodies as standing exceptions. Per the epsilon-invariance principle, the
 *   sibling readings (gender_identity_reading, social_role_reading) are
 *   separate constraints in separate files; nothing about them is averaged
 *   into this story. The epsilon referent is the standing marker-gated
 *   arrangement itself, assessed as it operates — not the inclusive
 *   arrangement this reading's opponents would install, and not a hedged
 *   average across readings. The claim/metric gap is deliberate: the reading
 *   is CLAIMED as tangled_rope (real coordination plus real asymmetric cost)
 *   while the metrics describe the actual operation, including high
 *   extraction concentrated on identity-locked payer seats; the engine
 *   measures that divergence per seat.
 *
 * KEY AGENTS:
 *   - statutory_sex_definition_authorities: agenda setter (institutional/arbitrage) — codifies and administers the marker criterion, can rewrite it at will
 *   - cis_women_service_users: primary beneficiary (moderate/constrained) — receive protected-space assurance in refuges, prisons, wards
 *   - female_category_athletes: beneficiary (moderate/constrained) — receive protected competitive category and records
 *   - gender_critical_campaign_organizations: beneficiary (organized/mobile) — collect standing, funding, media access, and agenda power from boundary maintenance
 *   - trans_women: primary target (powerless/identity_locked) — bear exclusion from the category and the suppression costs of enforcement
 *   - trans_men: target (powerless/identity_locked) — assigned to the natal category, documentation and placement mismatch
 *   - intersex_people: target (powerless/trapped) — bodies fail the binary marker test; historic medicalization, contemporary eligibility scrutiny
 *   - gender_nonconforming_children: excluded voice (powerless/trapped) — bound by school rules without a seat in the conversation
 *   - international_human_rights_bodies: observer (institutional/analytical) — shifts legitimacy conditions, binds no legislature
 *   - philosophy_of_gender_scholars: analytical observer — supplies the frameworks every seat borrows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.72).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.75).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological-Marker Gate on Gendered Category Membership (biological sex reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '471660c8-bc18-4b2a-bb23-1247e7c69461').
narrative_ontology:cs_kernel_codification('471660c8-bc18-4b2a-bb23-1247e7c69461', formalized).
narrative_ontology:cs_authority_grounding('471660c8-bc18-4b2a-bb23-1247e7c69461', lineage).
narrative_ontology:cs_interpretation_layer_present('471660c8-bc18-4b2a-bb23-1247e7c69461').
narrative_ontology:cs_reading_relation('471660c8-bc18-4b2a-bb23-1247e7c69461', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('471660c8-bc18-4b2a-bb23-1247e7c69461', gendered_category_membership__social_role_reading, forecloses).
narrative_ontology:cs_axiom('471660c8-bc18-4b2a-bb23-1247e7c69461', foundational, sex_is_immutable_and_determinate_at_birth).
narrative_ontology:cs_axiom_status(sex_is_immutable_and_determinate_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('471660c8-bc18-4b2a-bb23-1247e7c69461', sex_is_immutable_and_determinate_at_birth, empirically_contingent).
narrative_ontology:cs_axiom('471660c8-bc18-4b2a-bb23-1247e7c69461', secondary, single_sex_provision_requires_third_person_verifiable_criterion).
narrative_ontology:cs_axiom_status(single_sex_provision_requires_third_person_verifiable_criterion, holdable).
narrative_ontology:cs_axiom_grounding('471660c8-bc18-4b2a-bb23-1247e7c69461', single_sex_provision_requires_third_person_verifiable_criterion, instrumental).
narrative_ontology:cs_reference_frame('471660c8-bc18-4b2a-bb23-1247e7c69461', birth_registered_binary_sex_classification).
narrative_ontology:cs_drift_state('471660c8-bc18-4b2a-bb23-1247e7c69461', contemporary_self_id_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('471660c8-bc18-4b2a-bb23-1247e7c69461', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women_service_users).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, female_category_athletes).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, gender_critical_campaign_organizations).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_men).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and amend the statutory and regulatory texts that define legal sex and gate single-sex provision; operate birth registries, prison-placement rules, and sports eligibility codes; can redefine the criterion by ordinary legislative or administrative act, and face electoral and lobbying pressure from every other seat whenever they move it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, statutory_sex_definition_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Use refuges, prisons, hospital wards, and changing rooms designated for women; receive the assurance that admission turns on birth-recorded sex rather than declaration; individually hold little leverage over policy and rely on providers and campaigning groups to keep the admission rule they were promised.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women_service_users, beneficiary,
    moderate, biographical, constrained, national).

% Compete in female categories whose eligibility is set by natal sex markers; receive protected podium places, rankings, and records; careers are short and category integrity is framed as their principal professional interest; they do not set the eligibility rules they compete under.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, female_category_athletes, beneficiary,
    moderate, biographical, constrained, global).

% Campaign for statutory definitions of sex as biological; convert boundary maintenance into memberships, donations, media access, and advisory relationships with legislators; can redirect staff and donors to adjacent issues if this one closes, so leaving the contest is comparatively cheap for them.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_critical_campaign_organizations, beneficiary,
    organized, biographical, mobile, national).

% Live under definitions that assign them to the male category regardless of transition, documentation, or lived social reality; lose access to women's spaces, female sporting categories, and in some jurisdictions legal recognition itself; the category at stake is their own person, so no relocation or reframing leaves them intact; bear screening, disclosure, and confrontation costs wherever the boundary is actively checked.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Are assigned to the female category by the same marker rule; face placement in women's facilities and competitions that misdescribes them, or paperwork that cannot represent them at all; the criterion governs their bodies rather than their choices, and no change of jurisdiction reliably removes the mismatch.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_men, payer,
    powerless, biographical, identity_locked, national).

% Have chromosomes, gonads, or anatomy that fail the binary marker test; many were subjected to nonconsensual infant normalization surgery so the registry could record a binary sex; eligibility testing in elite sport has publicly scrutinized and disqualified some; their existence is the standing counterexample the marker criterion must continually manage.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_people, payer,
    powerless, biographical, trapped, global).

% Are bound by school sports and facility rules written by boards and legislatures; too young to vote, lobby, or litigate; encounter the boundary as playground and locker-room policing long before anyone asks them what it means for them.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_nonconforming_children, excluded,
    powerless, immediate, trapped, local).

% Issue treaty-body conclusions and special-procedure reports on the treatment of trans and intersex persons; take testimony from every other seat; their findings shift legitimacy conditions and supply litigation arguments but bind no legislature directly.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% Analyze what the category 'woman' is — its constitution, its history, its costs and functions; produce the frameworks the other seats borrow in testimony and statute; answerable to disciplinary standards rather than to any party in the contest.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, philosophy_of_gender_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, gender_critical_campaign_organizations).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, third-party-verifiable criterion for allocating access to sex-segregated intimate spaces (refuges, prisons, wards, changing rooms), single-sex competitive categories, and sex-based legal records; solves the administration problem of deciding who may enter which space and which category without case-by-case adjudication of identity claims.
% TRANSFER_FUNCTION: Moves categorical recognition and physical access: reserves women's spaces, female categories, and women's shortlists for birth-recorded females; assigns trans women to the male category and trans men to the female category regardless of transition; concentrates definitional authority in legislatures, registrars, and federation rules; imposes identity-suppression, documentation-mismatch, and screening costs on trans and intersex people.
% ABSENT_VOICES: Intersex adults and their advocates were absent when binary statutory definitions were drafted — their bodies falsify the marker test, yet no seat was provided in the drafting processes; trans people were minimally consulted in several jurisdictions that codified exclusions; gender-nonconforming children bound by school rules have no representative in board or legislative deliberation.
% DISAPPEARANCE_RATIONALE: If the marker criterion vanished overnight, facility admission rules, prison placement, sports eligibility, and documentation regimes all lose their operative test at once; institutions would redraft around self-declaration, role recognition, or mixed criteria; refuge providers, federations, and registries would rebuild intake procedures, and the current litigation map would dissolve into a different contest over the replacement criterion.
% FOUNDING_PROBLEM: Securing women's intimate spaces and competitive opportunities against male violence and male-puberty physiological advantage, and supplying medicine, law, and official statistics with a stable, externally verifiable sex classification recorded at birth.
% FOUNDING_PROBLEM_CORROBORATION: Sports-science reviews conducted with independent physiologists (for example the World Rugby 2019-2020 eligibility process) attest the performance-gap problem; refuge-sector incident and demand data attests the need for single-sex crisis provision; national statistical offices attest the administrative need for stable birth-registered classification. None of these sources attests that the biological gate is the only remedy — trans-led organizations and inclusion advocates dispute the remedy and its costs, not always the underlying problems — so corroboration covers the founding problem's existence, not the necessity of this particular solution.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.72 at interval end) because the marker gate strips category recognition, space access, and sporting eligibility from a defined population while delivering those goods exclusively to another; the cost is not incidental but constitutive of the arrangement. Suppression (0.75) reflects active enforcement machinery — statutory definitions, eligibility testing, facility checking, litigation defense — rather than mere convention; roughly seventy percent of it is structural (legal bars, documentation regimes) and thirty percent internalized (anticipated rejection, concealment pressure that persists where no rule is checked). Theater ratio (0.32) is moderate: facility provision and category administration are real functions, but a growing share of activity is symbolic definitional combat whose audience is the culture war rather than any facility door. Accessibility collapse is moderate-low (0.45): alternatives persist — jurisdictions with self-ID statutes, community recognition outside formal categories, federations with divergent eligibility rules — so the constraint does not close the possibility space the way a natural limit would. Resistance is high (0.7): sustained activism, litigation in both directions, and repeated legislative reversal attempts. The measurement series run on one shared grid (2015-2025, six points, all three metrics at every point) so no metric row is backfilled from another's scalar. Coalition note: the payer seats overlap imperfectly — trans and intersex constituencies have distinct priorities and histories — so coalition power among the powerless seats exists but is diluted by divergent aims.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural facts. From the trans_women and trans_men seats the arrangement operates as enforced exclusion with no exit that preserves the self — the identity_locked condition amplifies effective extraction toward the full-target end. From the cis_women_service_users and athlete seats the same rules deliver a valued guarantee they did not bargain for individually and cannot maintain alone — a subsidized good. The campaign organizations experience the boundary as a resource: issue salience converts to members, donors, and advisory access. The statutory authorities experience it as a governance duty under cross-pressure from every other seat. The engine computes these divergences from the declared roles, power, and exit options; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women, trans men, and intersex people are declared victims and derive directionality near the full-target end; identity_locked and trapped exit options push them further toward full exposure than a mobile target would sit. Cis women service users and female athletes are declared beneficiaries and derive low directionality — the arrangement subsidizes them — though their constrained (not arbitrage) exit keeps them dependent on the enforcement they benefit from. Campaign organizations are beneficiaries with mobile exit: they can leave the contest cheaply, which marks them as the clearest rent-collecting seat rather than a captive dependent. The statutory authorities are agenda setters rather than declared beneficiaries; they collect institutional authority from operating the boundary and would retain office under a rewritten criterion, placing them mid-range rather than at either pole. Observers are analytical and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing women's intimate spaces and competitive fairness, and maintaining a stable administrative classification — remains plausibly live, so the mandate has not outlived its function and mandatrophy is not resolved; no sunset clause exists or would be honest. The tangled_rope claim is what prevents mislabeling in both directions: a pure-extraction reading would erase the genuine protective function that many cis women experience as indispensable and that predates the current contest; a pure-coordination reading would erase the asymmetric, identity-targeted costs that fall entirely on populations with no exit. The temporal series shows extraction and enforcement intensifying together while theater grows — accumulation dynamics worth watching for drift toward a harder type, but the coordination core has not atrophied, so the claim stands as tangled_rope rather than piton or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the kernel gendered_category_membership — how much of the measured structure depends on the choice of reading rather than on the underlying category practices?',
    'Cross-reading comparison once the sibling files (gender_identity_reading, social_role_reading) are compiled: seats, victim sets, and computed types that are invariant across readings belong to the kernel; those that flip belong to the reading.',
    'If most structure is reading-relative, the corpus should treat the three files as one contested surface rather than three independent constraints; if invariant, the kernel itself carries the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: reading-choice versus kernel-level structure.').

omega_variable(
    intersex_marker_failure,
    'Does the immutable-marker criterion survive its own edge cases — the population whose chromosomes, gonads, or anatomy do not resolve to a clean binary at birth?',
    'Audit of how eligibility and registry rules actually dispose of intersex cases: mandatory sex testing outcomes in sport, infant normalization surgery rates, documentation practice for DSD individuals.',
    'If the criterion requires ongoing surgical and administrative management of exceptions, part of its apparent simplicity is enforced at intersex people''s expense and the extraction attributable to boundary maintenance rises; if dispositions are rare and consensual, the criterion''s determinacy claim strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_marker_failure, empirical, 'Whether the binary marker test is self-consistent on intersex bodies.').

omega_variable(
    transition_physiology_dispute,
    'How large is the residual male-puberty physiological advantage in female categories after transition and hormone therapy, and does it justify marker-based eligibility rather than performance-based thresholds?',
    'Longitudinal performance studies of transitioned athletes and federation-commissioned reviews with published protocols; comparison of marker-gated versus threshold-gated eligibility outcomes.',
    'A large irreducible advantage supports the sports arm of the coordination function and lowers its extraction share; a small or manageable advantage shifts the sports exclusion toward pure extraction riding on the wider boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_physiology_dispute, empirical, 'Empirical magnitude of post-transition athletic advantage.').

omega_variable(
    dilution_harm_evidence,
    'Do cis women suffer measurable harms from inclusive category definitions — the ''category dilution'' position this reading asserts — at rates that would substantiate the victim-positioning of cis women under alternative readings?',
    'Incident and outcome data from jurisdictions that adopted self-ID: refuge usage patterns, safeguarding reports, competitive results, compared against matched marker-gated jurisdictions with confound controls.',
    'Demonstrated dilution harm would add a second victim class under sibling readings and strengthen this reading''s coordination claim; absence of measurable harm would locate the asserted harm in anticipation rather than occurrence and reduce the coordination function''s scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dilution_harm_evidence, empirical, 'Whether asserted harms to cis women from inclusive definitions materialize.').

omega_variable(
    suppression_internalization_split,
    'Is the suppression borne by the payer seats predominantly structural (legal bars, documentation regimes, eligibility tests) or internalized (anticipated rejection, concealment, self-policing that persists where no rule is checked)?',
    'Post-relaxation trajectory: compare disclosure rates, facility usage, and mental-health indicators in jurisdictions that removed marker gates against matched jurisdictions that retained them; persistence of avoidance behavior after barrier removal indicates internalized carryover.',
    'If largely internalized, removing the statutory machinery understates remaining costs and effective suppression exceeds the structural measure; if largely structural, statutory reform captures most of the recoverable welfare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized composition of payer-seat suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t2015, gendered_category_membership__biological_sex_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(gend_tr_t2017, gendered_category_membership__biological_sex_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(gend_tr_t2019, gendered_category_membership__biological_sex_reading, theater_ratio, 2019, 0.24).
narrative_ontology:measurement(gend_tr_t2021, gendered_category_membership__biological_sex_reading, theater_ratio, 2021, 0.27).
narrative_ontology:measurement(gend_tr_t2023, gendered_category_membership__biological_sex_reading, theater_ratio, 2023, 0.3).
narrative_ontology:measurement(gend_tr_t2025, gendered_category_membership__biological_sex_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(gend_be_t2015, gendered_category_membership__biological_sex_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(gend_be_t2017, gendered_category_membership__biological_sex_reading, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement(gend_be_t2019, gendered_category_membership__biological_sex_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(gend_be_t2021, gendered_category_membership__biological_sex_reading, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement(gend_be_t2023, gendered_category_membership__biological_sex_reading, base_extractiveness, 2023, 0.69).
narrative_ontology:measurement(gend_be_t2025, gendered_category_membership__biological_sex_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t2015, gendered_category_membership__biological_sex_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(gend_su_t2017, gendered_category_membership__biological_sex_reading, suppression_requirement, 2017, 0.63).
narrative_ontology:measurement(gend_su_t2019, gendered_category_membership__biological_sex_reading, suppression_requirement, 2019, 0.66).
narrative_ontology:measurement(gend_su_t2021, gendered_category_membership__biological_sex_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(gend_su_t2023, gendered_category_membership__biological_sex_reading, suppression_requirement, 2023, 0.73).
narrative_ontology:measurement(gend_su_t2025, gendered_category_membership__biological_sex_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, social_role_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what makes someone a woman/man for law and policy' decomposes into three structurally distinct constraints — biological_sex_reading (this file), gender_identity_reading, and social_role_reading — because measuring membership by chromosomes/anatomy, by self-declaration, or by sustained recognition yields different epsilon values, different victim sets, and different enforcement structures. The upstream reading (biological markers) is the historically established criterion and is cited as the baseline the downstream readings amend or replace; each file links the others via network.affects_constraints. No single file may average across readings — that would violate epsilon invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
