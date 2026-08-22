% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Self-Identification Criterion for the Woman/Female Category (Gender-Identity Reading)
 *   domain: political philosophy/bioethics/gender studies/law
 *
 * SUMMARY:
 *   The question 'who counts as a woman/female' is a contested kernel with
 *   three live readings: membership by self-identification (this story),
 *   membership by biology, and context-indexed membership. This file
 *   instantiates the gender-identity reading alone as a clean,
 *   epsilon-invariant constraint. The standing arrangement under assessment
 *   is the self-declaration criterion as it operates in self-ID jurisdictions
 *   and in the institutions that apply it — not any rival arrangement the
 *   reading would prefer. Assessed by the reading's own lights (the
 *   dignity/recognition currency gender-identity theory itself uses), the
 *   arrangement delivers formal recognition and access while its operation
 *   imposes dignity and recognition costs on the category's own members: an
 *   internal criterion is unfalsifiable from outside, which invites permanent
 *   contest, and the costs of that contest — scrutiny, justification demands,
 *   the gap between formal and effective recognition, occupancy risk — land
 *   on the category's members rather than on the institutions that administer
 *   the rule. The exposed edge is trans women in shelters, prisons, hospital
 *   wards, and changing rooms, who absorb hostility and exclusion attempts
 *   inside the very spaces the rule opens to them. The declared beneficiaries
 *   (transgender individuals seeking identity-based protections) and the
 *   declared victims (the category's members, including trans women in
 *   female-only spaces) therefore overlap: the same population is recognized
 *   and cost-burdened by the same structure. Sibling readings are separate
 *   constraints in separate files, linked through
 *   network.affects_constraints; their victim sets and epsilon values differ
 *   because epsilon is a property of a reading, not of the topic. The claim
 *   (tangled_rope) and the metrics were authored independently.
 *
 * KEY AGENTS:
 *   - self_id_legal_regimes: agenda-setter (institutional/constrained) — enacts and maintains the criterion
 *   - institutional_space_operators: operational enforcer (institutional/constrained) — applies the criterion at admission and placement decisions
 *   - transgender_individuals: primary beneficiary (moderate/identity_locked) — receives recognition and legal protection
 *   - trans_women_in_female_only_spaces: exposed edge of the beneficiary class (powerless/trapped) — bears occupancy costs inside contested spaces
 *   - self_identified_women: category members bearing the contest costs (moderate/identity_locked)
 *   - gender_identity_advocacy_organizations: accrual seat (organized/mobile) — collects resources and authority from the rule's operation and defense
 *   - gender_critical_dissenters: excluded voice (organized/constrained) — contests the criterion from outside the rule-maintaining conversations
 *   - courts_and_equality_regulators: analytical observer (institutional/analytical) — adjudicates scope disputes without collecting from the rule's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.71).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.6).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Self-Identification Criterion for the Woman/Female Category (Gender-Identity Reading)").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political philosophy/bioethics/gender studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, 'a33a0059-3afe-4914-a0a9-69ccbb46c464').
narrative_ontology:cs_kernel_codification('a33a0059-3afe-4914-a0a9-69ccbb46c464', formalized).
narrative_ontology:cs_authority_grounding('a33a0059-3afe-4914-a0a9-69ccbb46c464', self_enforcing).
narrative_ontology:cs_reading_relation('a33a0059-3afe-4914-a0a9-69ccbb46c464', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('a33a0059-3afe-4914-a0a9-69ccbb46c464', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('a33a0059-3afe-4914-a0a9-69ccbb46c464', foundational, category_membership_tracks_self_identification).
narrative_ontology:cs_axiom_status(category_membership_tracks_self_identification, holdable).
narrative_ontology:cs_axiom_grounding('a33a0059-3afe-4914-a0a9-69ccbb46c464', category_membership_tracks_self_identification, deontological).
narrative_ontology:cs_axiom('a33a0059-3afe-4914-a0a9-69ccbb46c464', secondary, biological_sex_irrelevant_to_category_membership).
narrative_ontology:cs_axiom_status(biological_sex_irrelevant_to_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('a33a0059-3afe-4914-a0a9-69ccbb46c464', biological_sex_irrelevant_to_category_membership, deontological).
narrative_ontology:cs_reference_frame('a33a0059-3afe-4914-a0a9-69ccbb46c464', self_determined_identity_recognition).
narrative_ontology:cs_drift_state('a33a0059-3afe-4914-a0a9-69ccbb46c464', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a33a0059-3afe-4914-a0a9-69ccbb46c464', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, self_identified_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, trans_women_in_female_only_spaces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, trans_women_in_female_only_spaces).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, institutional_space_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under legal regimes that recognize category membership by self-declaration: they obtain identity documents, anti-discrimination coverage, and access to identity-matched services without medical certification. What flows to them is recognition and legal protection; what flows from them is reliance on the rule's continued operation for their legal status. Leaving the arrangement is not a realistic option — their recognized status exists only inside it, and reverting to prior regimes would mean losing documents and protections they currently hold.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Occupy shelters, prisons, hospital wards, and changing rooms designated for women under the self-declaration rule. They receive the rule's core benefit — admission to spaces matching their identity — and they also absorb the costs of occupying contested space: scrutiny of their presence, demands for justification, hostility from other occupants and staff, exclusion campaigns, and safety risk while inside. Exit would mean surrendering access to the spaces they depend on; for incarcerated and shelter-dependent women the space itself is not optional.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, trans_women_in_female_only_spaces, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, trans_women_in_female_only_spaces, beneficiary).

% The full membership of the category as this reading defines it. They hold the category's protections and services, and they carry the costs of the category's contested standing: the boundary of 'woman' is permanently disputed in public discourse, so members face recurring demands to defend or explain the category's definition, absorb the reputational spillover of each contest episode, and navigate institutions whose rules shift as contests are decided. Membership is constitutive of identity — exiting the category is not a meaningful option.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, self_identified_women, payer,
    moderate, biographical, identity_locked, global).

% Legislatures, ministries, and registries that enacted self-declaration statutes and maintain the legal definition of the category. They set the criterion, issue guidance, and amend or defend it when courts and campaigns challenge it. What flows to them is administrative simplicity — self-declaration is cheaper to operate than medical certification — and international standing; what flows from them is the legal force that keeps the criterion operative. Changing course is constrained by precedent, treaty commitments, and the status of already-recognized individuals.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, self_id_legal_regimes, agenda_setter,
    institutional, generational, constrained, national).

% Prisons, shelter networks, sports governing bodies, hospitals, and universities that apply the criterion to admission and placement decisions. They enforce the rule at the operational level — admitting by self-declaration, handling complaints, managing disputes between occupants. They collect reputational value from operating an inclusive rule and bear litigation and incident risk when individual decisions become public controversies. They must apply whatever criterion the legal regime sets, so their exit is bounded by the regime's choices.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, institutional_space_operators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, institutional_space_operators, beneficiary).

% Advocacy, legal, and lobbying organizations that campaign for the rule's adoption and defend it against challenge. What flows to them is membership, funding, salience, and moral authority, which scale with both the rule's operation and the intensity of contest over it; what flows from them is the defense work that keeps the rule in place. They can redirect effort to adjacent campaigns if this one closes, so their position is not locked.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% Campaigners, academics, and litigants who hold that category membership should track biological sex and contest the self-declaration criterion. They would reshape the rule's criterion and scope if seated in the legislative and institutional conversations that maintain it, but they operate largely outside those conversations: institutional enforcement penalizes non-recognition in the workplace, platforms moderate their framing, and legislative hearings on the rule rarely seat their witnesses. Their channels are litigation, elections, and parallel institutions.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_dissenters, excluded,
    organized, biographical, constrained, global).

% Courts and equality bodies that adjudicate disputes over the rule's application and scope — placement decisions, service exclusions, the meaning of statutory terms. They take evidence from the other seats, issue rulings that expand or contract the rule's reach, and do not themselves collect from its operation. Their position is analytical: they observe the full structure and their rulings reshape it.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, courts_and_equality_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, self-administered criterion for category membership where legal and administrative systems otherwise need one: identity documents, anti-discrimination coverage, service admission, and data collection all key off one declaration instead of per-institution medical adjudication. It replaces a heterogeneous gatekeeping apparatus with a uniform rule.
% TRANSFER_FUNCTION: Moves recognition and access — documents, legal protection, admission to identity-matched spaces and services — to anyone who declares membership. Moves the costs of the category's contested standing — scrutiny, justification burdens, occupancy risk in contested spaces, reputational spillover — onto the category's members, with the heaviest load on those occupying contested spaces. Moves administrative simplicity and international standing to the regimes that operate the rule, and resourcing and authority to the organizations that defend it.
% ABSENT_VOICES: Gender-critical campaigners and sex-based-rights advocates are structurally outside the conversations that maintain the rule: legislative hearings on self-declaration statutes rarely seated witnesses framing membership biologically, institutional enforcement penalizes non-recognition at work, and platform moderation narrows their reach. Detransitioned individuals' testimony about the rule's low-friction access is contested and largely absent from policy fora. Their objection — that a criterion invisible from outside cannot do boundary-work for spaces organized around shared vulnerability — is voiced through litigation and parallel institutions rather than inside the rule-making rooms.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, every self-declared recognition issued under it would lose its legal basis: documents would revert to medical-certification or biological criteria, admission rules at shelters, prisons, wards, and changing rooms would flip to whatever prior or successor rule each institution holds, anti-discrimination coverage keyed to the category would contract for those recognized only by declaration, and the contest would reorganize around the successor criterion rather than dissolve.
% FOUNDING_PROBLEM: Gender recognition was previously gated by medical authority: applicants needed diagnosis, sustained treatment, and — in several European regimes into the 2010s — sterilization or other bodily modification before the state would recognize their category membership. The rule was built to remove medical gatekeeping and make recognition a matter of self-declaration, treating identity as self-determined rather than medically certified.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: European Court of Human Rights case law (Goodwin v UK, 2002) established the recognition problem before self-declaration existed; parliamentary records document the sterilization prerequisites in Sweden (repealed 2013), Norway, and other regimes; UN human-rights mechanisms and the Council of Europe's commissioner have attested the harms of medical gatekeeping. Within self-declaration jurisdictions the founding problem is solved on its own terms; the live question globally is that most jurisdictions still gate recognition medically.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71) on the reading's own currency: the criterion's unfalsifiability makes membership permanently contestable, and the costs of that contest land on members rather than on administering institutions. Suppression (0.60) is the enforcement layer that keeps non-recognition costly — institutional discipline, litigation exposure, platform moderation — substantial but incomplete, because the contest is legal, organized, and winning ground in some jurisdictions. Theater (0.42) is moderate and rising: the legal function is real (documents, coverage, admissions do operate), while a growing share of activity is performative compliance — statements, training, symbolism — that does not change members' practical position. Accessibility collapse (0.55): biological and context-indexed criteria remain visible in other jurisdictions and litigable at home, so alternatives are narrowed but not erased. Resistance (0.70) is high: sustained litigation, legislative reversal campaigns, and institutional reversals in sport. The three measurement series run on one shared grid (2012, 2015, 2018, 2021, 2024, 2026) so every tracked metric is authored at every examined point; contest waves are absorbed into the rising trend rather than modeled as cycles. Suppression is authored as a raw structural property — only extractiveness is scaled by directionality and scope downstream. The claim and metrics were authored independently: the claim from structure (genuine coordination function plus real asymmetric cost-bearing plus active enforcement), the metrics from descriptive operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the member seats compute differently from the same structure. From the legal regime's seat, the arrangement is administrative coordination it built and maintains at low operating cost; from the space-occupying member's seat, the same structure is scrutiny, justification burden, and physical risk; from the advocacy seat it is a mandate that resources its own defense. Same-level divergence: transgender_individuals and trans_women_in_female_only_spaces share an identity class and nominal position but differ in exit options — the broad class holds portable recognition, while the space-occupying edge is trapped inside the spaces whose contested status generates the costs. Inter-institutional divergence: legal regimes set the criterion while institutional operators bear its incident risk, so the same rule is cheap for its authors and expensive for its enforcers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (transgender_individuals, gender_identity_advocacy_organizations) derive low directionality — the rule subsidizes recognition for the first and resourcing for the second. Victim declarations (self_identified_women, trans_women_in_female_only_spaces) derive high directionality — the rule's contest costs land on them. One override: the derivation would read trans_women_in_female_only_spaces as near-full targets (~0.85) from the victim declaration and trapped exit alone, but they are also the rule's intended beneficiaries — they receive the admission and recognition the rule exists to deliver. The override sets the powerless seat to 0.65, reflecting genuine dual positioning: real benefit received, real dignity and safety costs borne. self_identified_women keep the derived high directionality: under this reading's lights they do bear the category's contest costs heavily, and their incidental share of the category's protections does not offset it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two mislabelings apart. Reading the arrangement as pure coordination would erase the members' cost-bearing: the scrutiny, occupancy risk, and formal-versus-effective recognition gap are real and structural, invited by the criterion's unfalsifiability. Reading it as pure extraction would erase the founding function: the rule replaced medical gatekeeping — diagnosis requirements, waiting regimes, and historically sterilization prerequisites — with a uniform self-administered criterion, a genuine coordination gain its beneficiaries hold. The tangled-rope reading holds both: the same structure that delivers recognition also generates the contest whose costs its members absorb. The founding problem (recognition without medical gatekeeping) is live globally — most jurisdictions still gate recognition medically — so the mandate has not outlived its function and no mandatrophy is declared. The drift risk runs the other way: the rising suppression and theater series indicate the arrangement hardening into enforcement-first operation, which is the direction in which this structure degrades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the gender_identity_reading of the woman_female_category kernel — what structurally changes under the sibling readings (sex_biology_reading, hybrid_contextual_reading)?',
    'The sibling stories author their own beneficiary/victim sets and epsilon over the same kernel; comparing the three stories'' computed types and victim sets locates the disagreement''s structural consequences.',
    'Under sex_biology_reading the victim set inverts (trans individuals excluded from identity-matched provision become the cost-bearers); under hybrid_contextual_reading the single epsilon here splits into per-context values (sports/medical vs social/legal).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: this story is one of three readings of the category-membership kernel.').

omega_variable(
    disagreement_location_criterion_vs_function,
    'Is the inter-reading disagreement located in the membership criterion itself (who decides membership) or in what the category is for (which functions the category must serve)?',
    'Conceptual analysis: test whether hybrid_contextual_reading''s context-split satisfies each sibling''s core premise in its disfavored domain; if each sibling could accept the hybrid in the other''s domain, the dispute is functional; if neither yields, it is criterial.',
    'If functional, the kernel resolves into context-indexed constraints and this reading''s universal scope claim is the contested element; if criterial, the readings are incommensurable and the foreclosure edges are the true structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_criterion_vs_function, conceptual, 'Locates where the three readings actually disagree.').

omega_variable(
    extraction_currency_attribution,
    'Is the high epsilon on dignity/recognition harms generated by the arrangement''s own structure (an unfalsifiable membership criterion that structurally invites permanent contest), or by surrounding hostility the arrangement cannot control?',
    'Cross-jurisdiction comparison of member-reported dignity and recognition outcomes at equal formal rule but different contest intensity; natural experiments where contest intensity shifts (post-2025 rulings) at unchanged formal rule.',
    'If contest-generated, the arrangement''s own epsilon falls and the seat classifications drift coordination-dominant; if structure-generated, epsilon stays high and the extraction is attributed to the criterion itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_currency_attribution, empirical, 'Attribution of member-borne dignity/recognition harms to arrangement structure vs surrounding contest.').

omega_variable(
    enforcement_durability_across_regimes,
    'Is the enforcement apparatus that makes non-recognition costly durable across jurisdictions and political cycles, or does it decay where contest prevails?',
    'Cross-jurisdiction tracking of enforcement intensity (institutional discipline, litigation outcomes, platform moderation) against contest outcomes over time.',
    'Where enforcement decays, the suppression trajectory reverses and the arrangement drifts toward contested coexistence; where it hardens, the rising suppression series continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_durability_across_regimes, empirical, 'Durability of the enforcement layer across regimes and political cycles.').

omega_variable(
    kernel_framing_undetermination,
    'Is the kernel the statutory term ''woman/female'' in legal texts, the social category, or the linguistic term — and does the framing change which institutions count as adjudicating readings?',
    'Compare classification under each framing: a statutory kernel routes adjudication to courts; a social-category kernel routes it to practice and usage; a linguistic kernel distributes it across speakers.',
    'Under a social-category kernel the authority structure shifts away from self-enforcing declaration and the drift assessment changes; under a statutory kernel the reading set narrows to legal interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_undetermination, conceptual, 'Framing under-determination in what the kernel itself is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 2012, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2012, woman_female_category__gender_identity_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__gender_identity_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(woma_tr_t2018, woman_female_category__gender_identity_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(woma_tr_t2021, woman_female_category__gender_identity_reading, theater_ratio, 2021, 0.34).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__gender_identity_reading, theater_ratio, 2024, 0.39).
narrative_ontology:measurement(woma_tr_t2026, woman_female_category__gender_identity_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(woma_be_t2012, woman_female_category__gender_identity_reading, base_extractiveness, 2012, 0.45).
narrative_ontology:measurement(woma_be_t2015, woman_female_category__gender_identity_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(woma_be_t2018, woman_female_category__gender_identity_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(woma_be_t2021, woman_female_category__gender_identity_reading, base_extractiveness, 2021, 0.64).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__gender_identity_reading, base_extractiveness, 2024, 0.69).
narrative_ontology:measurement(woma_be_t2026, woman_female_category__gender_identity_reading, base_extractiveness, 2026, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2012, woman_female_category__gender_identity_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(woma_su_t2015, woman_female_category__gender_identity_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(woma_su_t2018, woman_female_category__gender_identity_reading, suppression_requirement, 2018, 0.48).
narrative_ontology:measurement(woma_su_t2021, woman_female_category__gender_identity_reading, suppression_requirement, 2021, 0.54).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__gender_identity_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement(woma_su_t2026, woman_female_category__gender_identity_reading, suppression_requirement, 2026, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'who counts as a woman' covers three structurally distinct category rules. Per the epsilon-invariance principle the family is authored as three stories: sex_biology_reading (criterion = biology), hybrid_contextual_reading (criterion = context-indexed), and this story (criterion = self-identification). Each carries its own victim set, beneficiaries, and reading-indexed epsilon over the same referent; the sibling stories link back here. The edges record constraint-family membership and mutual citation in the contest, not empirical causal priority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
