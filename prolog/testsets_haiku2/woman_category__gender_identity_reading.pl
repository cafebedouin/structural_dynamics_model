% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender Identity as Woman-Category Membership Criterion
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This story instantiates the gender-identity reading of the contested
 *   woman-category kernel: category membership is determined by internal
 *   gender identity, such that a person who identifies as a woman is a woman
 *   regardless of sex assigned at birth. This reading sits in direct
 *   structural tension with the sex-biology reading (which grounds membership
 *   in chromosomal, anatomical, or reproductive facts) and the
 *   intersex-accommodation reading (which treats biological sex as a
 *   non-binary spectrum). The tension manifests in four domains: identity
 *   documents (moderate extractiveness), sports eligibility (high
 *   extractiveness for female athletes), sex-segregated spaces (high
 *   extractiveness for exclusion-seeking populations), and reproductive
 *   healthcare (moderate extractiveness depending on jurisdiction). This
 *   story describes the identity reading's operation as a legal/policy
 *   constraint; the sibling readings are other constraints instantiating the
 *   same kernel under different readings.
 *
 * KEY AGENTS:
 *   - transgender_women: seek recognition of gender identity as legal category membership; face variable social and institutional recognition depending on jurisdiction
 *   - non_binary_people_claiming_woman_identity: occupy an identity position the sex-biology reading does not accommodate; benefit from identity-based membership but face high suppression in enforcement contexts
 *   - civil_rights_advocates_for_gender_identity: institutional seats (advocacy organizations, legal scholars, some judicial bodies) advancing the reading's legitimacy through litigation and legislative change
 *   - people_in_sex_segregated_spaces_seeking_exclusion: experience the constraint as foreclosing their ability to maintain sex-based boundaries; bear the extractive cost of the identity reading's enforcement
 *   - female_athletes_competing_in_sex_based_categories: face a bifurcated eligibility picture: identity-based membership rules expand the pool, creating competitive asymmetry; the constraint extracts athletic advantage from this asymmetry
 *   - people_relying_on_sex_based_protections: reproductive healthcare access, sexual violence prevention services, workplace protections tied to sex category; the identity reading's enforcement changes the beneficiary set and may reduce coverage specificity
 *   - sex_biology_reading_adherents: institutional seats (some sports bodies, biological sciences, religious traditions) maintaining the competing reading; suppressed by the identity reading's enforcement
 *   - legal_systems_and_record_keepers: implement the reading operationally; the suppression metric reflects enforcement intensity required to maintain category coherence across administrative systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.62).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.71).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender Identity as Woman-Category Membership Criterion").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, 'e8413dc1-918d-47f9-b58c-ef752611c9e6').
narrative_ontology:cs_kernel_codification('e8413dc1-918d-47f9-b58c-ef752611c9e6', fixed_text).
narrative_ontology:cs_authority_grounding('e8413dc1-918d-47f9-b58c-ef752611c9e6', distributed).
narrative_ontology:cs_reading_relation('e8413dc1-918d-47f9-b58c-ef752611c9e6', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8413dc1-918d-47f9-b58c-ef752611c9e6', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('e8413dc1-918d-47f9-b58c-ef752611c9e6', foundational, gender_identity_determines_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('e8413dc1-918d-47f9-b58c-ef752611c9e6', gender_identity_determines_category_membership, deontological).
narrative_ontology:cs_axiom('e8413dc1-918d-47f9-b58c-ef752611c9e6', foundational, identity_based_membership_enables_dignity_and_recognition).
narrative_ontology:cs_axiom_status(identity_based_membership_enables_dignity_and_recognition, holdable).
narrative_ontology:cs_axiom_grounding('e8413dc1-918d-47f9-b58c-ef752611c9e6', identity_based_membership_enables_dignity_and_recognition, deontological).
narrative_ontology:cs_reference_frame('e8413dc1-918d-47f9-b58c-ef752611c9e6', identity_based_woman_category).
narrative_ontology:cs_drift_state('e8413dc1-918d-47f9-b58c-ef752611c9e6', contemporary_jurisdictional_fragmentation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e8413dc1-918d-47f9-b58c-ef752611c9e6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, non_binary_people_claiming_woman_identity).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, civil_rights_advocates_for_gender_identity).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, people_in_sex_segregated_spaces_seeking_exclusion).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_athletes_competing_in_sex_based_categories).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, people_relying_on_sex_based_protections).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, female_athletes_competing_in_sex_based_categories).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, people_relying_on_sex_based_protections).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, non_binary_people_claiming_woman_identity).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, internal_gender_identity_is_self_determined).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_identity_deserves_legal_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek recognition that internal gender identity determines woman-category membership. This reading directly affirms their identity; they receive legal recognition, institutional access (healthcare, documentation, facilities aligned with gender), and social legitimacy through the constraint's enforcement. Their exit would require denying their own identity, making exit identity-locked. They face suppression in jurisdictions enforcing the competing sex-biology reading.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    powerless, biographical, identity_locked, global).

% Occupy an identity position (neither fully binary woman nor man) that the sex-biology reading does not accommodate. The identity reading provides category membership and institutional access. They pay a diffuse cost: in sex-segregated spaces and competitive contexts, they may face residual suspicion or exclusion despite the reading's enforcement, suggesting the suppression required to maintain category coherence is imperfect. They are identity-locked—exit would require identity denial.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, non_binary_people_claiming_woman_identity, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, non_binary_people_claiming_woman_identity, payer).

% Institutional seats (legal advocacy organizations, some judicial bodies, civil-rights commissions, academic disciplines treating gender identity as primary) advancing the reading's legitimacy through litigation, legislation, policy guidance, and cultural change. They set the agenda for category membership rules; they benefit from the reading's adoption (professional authority, institutional power, ideological alignment). They have arbitrage options: if the reading loses political support, they can shift focus to other domains or retrench into progressive jurisdictions.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, civil_rights_advocates_for_gender_identity, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, civil_rights_advocates_for_gender_identity, beneficiary).

% Experience the constraint as foreclosing their ability to maintain sex-based boundaries in bathrooms, shelters, prisons, changing facilities, and other spaces organized around sex. They want membership criteria based on sex biology; the identity reading actively suppresses that operationalization. They face suppression in the form of institutional rules, legal liability, and social stigma if they enforce sex-based exclusion. Their exit options are constrained: they cannot simply opt out of using these spaces, and jurisdiction-shopping is costly.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, people_in_sex_segregated_spaces_seeking_exclusion, payer,
    moderate, biographical, constrained, global).

% Face bifurcated eligibility rules where the identity reading expands the pool of women competitors, creating competitive asymmetry for athletes who entered competition under sex-based membership. They receive coordination benefit (unified category, administrative coherence) but pay an extractive cost (competitive disadvantage from expanded pool, potential harm from athletes with different strength profiles). They are constrained: leaving the sport means forgoing career, and most jurisdictions are converging toward the identity reading.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_athletes_competing_in_sex_based_categories, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, female_athletes_competing_in_sex_based_categories, beneficiary).

% Access reproductive healthcare, sexual violence prevention services, and workplace protections designed around sex. The identity reading changes the beneficiary set for these protections (now extends to all who identify as woman, not just those with female reproductive biology), which can reduce protection specificity (e.g., reproductive healthcare access becomes blurred when the category includes people with male reproductive anatomy). They rely on these protections institutionally and cannot easily exit.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, people_relying_on_sex_based_protections, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, people_relying_on_sex_based_protections, beneficiary).

% Institutional seats (some sports bodies, biological sciences, religious traditions, some judicial interpretations) maintaining the sex-biology reading are actively suppressed by the identity reading's enforcement. Their reading is marginalized or prohibited in institutional contexts advancing the identity reading. They are trapped: their core institutional commitments (sports eligibility structures, reproductive biology frameworks) are directly threatened by the identity reading's enforcement, and they lack exit options within their own institutional domains.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_biology_reading_adherents, excluded,
    institutional, generational, trapped, global).

% Implement the reading operationally: change identity documents to reflect gender identity, update eligibility criteria in services tied to woman-category membership, enforce category coherence across administrative systems. This role requires active suppression (preventing document standards from reverting to sex-based systems, enforcing uniformity across agencies). The high suppression metric (0.71) reflects the enforcement intensity required to maintain category coherence when the kernel is contested.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, legal_systems_and_record_keepers, agenda_setter,
    institutional, generational, analytical, national).

% External position from which to measure the constraint's operation and structure. Can assess whether the reading achieves its stated coordination goals, what costs it imposes, and how it structurally relates to the sibling readings. This seat sees the entire kernel contest and the three competing instantiations.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, civil_rights_advocates_for_gender_identity).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves unified legal recognition for people who identify as women, regardless of sex assigned at birth. Solves the coordination problem of administrative coherence: if a person is recognized as a woman for identity documents, they are recognized across institutional domains (healthcare, facilities, legal status, employment). Without unified membership, the same person would face contradictory rules (woman for identity purposes, not-woman for healthcare, not-woman for facilities, etc.).
% TRANSFER_FUNCTION: Transfers institutional access and legal recognition from a category bounded by sex biology to a category bounded by gender identity. The constraint moves recognition, status, and institutional benefits from seats that maintain sex-based boundaries to seats that accept identity-based membership. It also moves suppressive power from exclusion-seeking populations (who lose the ability to enforce sex-based boundaries) to civil-rights advocates (who gain authority to enforce identity-based membership).
% ABSENT_VOICES: Sex-biology-reading adherents and people seeking sex-based exclusion are structurally excluded from advancing their reading in contexts where the identity reading dominates. Their voices are not absent in the full kernel contest (they maintain competing readings in other jurisdictions and institutions), but they are excluded from advancing their position in jurisdictions enforcing the identity reading. They would argue that biology-based membership preserves sex-specific protections and boundaries; they are kept out by the identity reading's enforcement framework.
% DISAPPEARANCE_RATIONALE: The identity-reading adherents assert that if the identity reading disappeared overnight, the world would rearrange: people would lose legal recognition, institutional access would collapse, and administrative systems would revert to sex-based categorization. The sex-biology-reading adherents and exclusion-seeking populations would argue the world would rearrange the other way: sex-based protections and boundaries would restore themselves. The contestation is fundamental—different seats have genuinely opposed interests in whether the constraint persists.
% FOUNDING_PROBLEM: The founding problem, as articulated by the identity reading, is that legal and institutional category membership grounded in sex assigned at birth fails to recognize people whose internal gender identity diverges from that assignment, creating legal invisibility, institutional erasure, and denial of services and recognition to transgender and non-binary people.
% FOUNDING_PROBLEM_CORROBORATION: Identity-reading advocates and many civil-rights organizations attest the founding problem is live and urgent. Clinical psychology and psychiatry bodies (APA, AMA, WHO-DSM-5) have shifted toward recognizing gender identity as central to wellbeing. However, sex-biology-reading adherents contest the problem statement itself: they argue that biological sex is immutable and that building legal categories on subjective identity creates different problems (confusion, loss of sex-based protections, contradictory institutional rules). There is no external corroboration of the problem's existence that would satisfy both readings—the founding problem is part of the kernel contest. Corroboration from outside the benefiting parties exists: jurisdictions that have adopted identity-based membership report reduced legal conflicts and improved health outcomes for transgender people (medical literature); jurisdictions that have rejected it report concerns about protection specificity and boundary collapse (conservative policy literature). Neither corroborates the problem in a reading-neutral way.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint shifts resources or access from one seat to another beyond coordination cost. At time 0 (early adoption in progressive jurisdictions), ε is low (0.38) because the reading is embraced by beneficiaries and enforcement is light—the constraint feels like coordination for the seats advancing it. As time progresses and adoption widens to mixed-position jurisdictions, extractiveness rises. By time 25, it stabilizes around 0.62 because: (1) in high-stakes domains (sports, sex-segregated spaces), the identity reading's enforcement visibly forecloses the sex-based reading, creating identifiable victims; (2) in lower-stakes domains (identity documents), the coordination function is clearer and ε drops. Theater ratio starts low and rises moderately (0.28 at end) because enforcement rhetoric emphasizes inclusivity and dignity while enforcement action focuses on category-boundary maintenance against exclusion-seeking populations—a divergence consistent with a tangled rope's partial performative maintenance. Suppression rises throughout because achieving category coherence under the identity reading requires actively suppressing the sex-biology reading's operationalization in overlapping domains.
 *
 * PERSPECTIVAL GAP:
 *   This constraint should compute to markedly different types from different seats. From the beneficiary seats (transgender women, civil-rights advocates, institutional bodies advancing the reading), it computes as rope or scaffold: genuine coordination achieving recognition and legal coherence with minimal enforcement overhead and no identifiable victims. From the exclusion-seeking seats, it computes as snare: identifiable victims (exclusion seekers), high suppression, and persistence dependent on actively defending category boundaries against the competing sex-biology reading. From the female-athlete and sex-based-protection seats, it should compute as tangled rope: genuine coordination (unified category, legal coherence) yoked to asymmetric extraction (competitive or protective harm). The gap between the beneficiary and victim seats is structural: the readings themselves are incompatible on the core question of what grounds membership. The engine's per-seat computation reveals this incompatibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs steeply across seats. Transgender women and non-binary people claiming woman identity (beneficiaries) sit at d near 0.0 (full beneficiary end): the constraint directly recognizes their identity, opens institutional access, and incurs minimal cost for them. Civil-rights advocates (powerful/institutional beneficiaries) sit similarly low—the reading advances their legal and philosophical project. People seeking sex-based exclusion (victims) sit near d = 1.0 (full target end): they cannot exit the constraint's domain (sex-segregated spaces, sports, reproductive healthcare remain jurisdictionally required), their exclusionary category membership is actively suppressed, and they receive nothing from the constraint's operation. Female athletes sit at intermediate d (~0.65): they receive some coordination benefit (unified eligibility criteria) but face extractive pressure (competitive asymmetry from the expanded eligibility pool). People relying on sex-based protections sit at d ~0.70 (mostly targets): they experience category collapse (protection specificity is reduced when 'woman' includes people with a broader range of biological histories), but some benefit from expanded alliance-building within the unified category. The engine derives these d values from the structural data; they are not authored directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophic in the sense of being a degraded former function. Rather, it is a *live reading of a persistent kernel* where the mandate (to determine who counts as a woman) remains contested. The sex-biology reading would say the identity reading's mandate is obsolete (biology is the mandate; identity is a subjective add-on). The identity reading would say its mandate is foundational (identity is the true ground; biology is a contingent property some women have). Mandatrophy language ('founding problem is dead, constraint persists') does not fit. What fits is: the three readings represent three competing operative answers to a single kernel question; all three remain live; the identity reading's operation enforces its answer by suppressing the others. This is not inertia or theater—it is active interpretive competition within a commitment system (the kernel).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_definition,
    'What constitutes ''internal gender identity'' as a criterion for category membership—subjective self-report, persistent psychological claim, clinical diagnosis, or lived social expression?',
    'Jurisprudential coherence across jurisdictions: different readings produce different legal tests (self-identification-only vs. clinical-gatekeeping vs. lived-history thresholds). The specificity chosen determines ε by changing who counts as a victim or beneficiary.',
    'A pure self-report reading (identity = reported identity) produces lower suppression but higher victim-count (exclusion-seekers bear higher costs). A clinical-gatekeeping reading produces higher suppression (psychology professionals retain veto power) and narrower victim/beneficiary sets. This reading-internal ambiguity is the primary structural uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_definition, conceptual, 'Which operationalization of ''internal gender identity'' grounds this reading—self-report, clinical, social-practice, or some hybrid.').

omega_variable(
    sex_based_protections_incompatibility,
    'Can sex-based legal protections (in sports, sex-segregated spaces, reproductive healthcare) coexist with gender-identity-as-membership in a single framework, or does the identity reading structurally foreclose sex-based protections?',
    'Three-layer test: (1) logical incompatibility—can a framework state both ''woman = identity'' AND ''this domain applies sex-based rules''? (2) Institutional precedent—how courts and legislatures treat conflicts between these readings in actual cases. (3) Theoretical coherence—whether a party can endorse identity-membership while carving out domain-specific sex-based exceptions without internal contradiction.',
    'If logically compatible, the readings coexist (coexists_with relation holds). If logically incompatible, the gender-identity reading forecloses sex-based protections reading in that domain (forecloses relation applies). If institutionally separated (different jurisdictions adopt different answers), both remain live but influence each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_protections_incompatibility, conceptual, 'Whether gender-identity membership and sex-based legal protections are logically compatible or structurally foreclosing.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) of exclusion-seeking agents structural (legal barriers, institutional rules, resource access restrictions) or internalized (social stigma, identity-fusion barriers, epistemic isolation)?',
    'Post-exit trajectory: if exclusion-seeking agents move to jurisdictions with sex-based enforcement and suppression persists (social/institutional exile), reclassify as partially internalized. If suppression drops (structural removal resolves it), classify as purely structural.',
    'If internalized: effective suppression is higher than the structural measure (targets carry suppression with them across jurisdictions), and identity-fusion mechanisms are the enforcing infrastructure, not just legal rules. If structural: the measure is accurate as a raw descriptor of enforcement intensity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of exclusion-seeking agents operates via structural barriers or internalized identity-fusion.').

omega_variable(
    extractive_vs_coordinative_core,
    'Does the gender-identity reading primarily solve a coordination problem (unified category membership across domains for legal consistency and dignity) or primarily extract exclusionary power (closing off alternative categorizations that would accord sex-based protections)?',
    'Comparative institutional analysis: compare jurisdictions where identity-based membership is enforced (coordination story: enables legal coherence, reduces administrative complexity, unifies protection scope) versus those where sex-based and identity-based coexist with domain-specific rules (hybrid story: shows coordination is possible without full exclusion, suggesting the enforcement is extracting something beyond coordination).',
    'Pure coordination → rope or scaffold. Coordination with asymmetric exclusionary effect → tangled rope. Pure extraction dressed as coordination → snare. The measured extractiveness (0.62) suggests tangled rope, but the underlying structural intent (whether the coordination could be achieved without foreclosing sex-based protections) determines if this is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_vs_coordinative_core, conceptual, 'Whether the reading''s primary structural function is coordination (unified membership) or extraction (exclusion of alternative framings).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t5, woman_category__gender_identity_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(woma_tr_t5, observed).
narrative_ontology:measurement(woma_tr_t10, woman_category__gender_identity_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(woma_tr_t10, observed).
narrative_ontology:measurement(woma_tr_t15, woman_category__gender_identity_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(woma_tr_t15, observed).
narrative_ontology:measurement(woma_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(woma_tr_t20, observed).
narrative_ontology:measurement(woma_tr_t25, woman_category__gender_identity_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(woma_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t5, woman_category__gender_identity_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(woma_be_t5, observed).
narrative_ontology:measurement(woma_be_t10, woman_category__gender_identity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(woma_be_t10, observed).
narrative_ontology:measurement(woma_be_t15, woman_category__gender_identity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(woma_be_t15, observed).
narrative_ontology:measurement(woma_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(woma_be_t20, observed).
narrative_ontology:measurement(woma_be_t25, woman_category__gender_identity_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(woma_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t5, woman_category__gender_identity_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement_basis(woma_su_t5, observed).
narrative_ontology:measurement(woma_su_t10, woman_category__gender_identity_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(woma_su_t10, observed).
narrative_ontology:measurement(woma_su_t15, woman_category__gender_identity_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(woma_su_t15, observed).
narrative_ontology:measurement(woma_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(woma_su_t20, observed).
narrative_ontology:measurement(woma_su_t25, woman_category__gender_identity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(woma_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the woman_category kernel family. All three readings (gender_identity_reading, sex_biology_reading, intersex_accommodation_reading) share a kernel but instantiate different constraints with different ε values, beneficiary/victim sets, and per-seat classifications. The gender_identity_reading enforces unified membership by identity, which structurally influences the sex_biology_reading (by making sex-based membership harder to operationalize in overlapping domains) and coexists with the intersex_accommodation_reading (both accommodate gender-identity-adjacent categories, but differ on biological variation treatment). Each reading is a separate constraint story; the network links show their kernel kinship and structural interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
