% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Biological-Sex Category Boundary (Biology Reading)
 *   domain: social/legal/political
 *
 * SUMMARY:
 *   A legal and institutional regime classifies every person into exactly two
 *   sexes at birth on the basis of chromosomes and visible anatomy, and
 *   treats that classification as fixed for life: it drives identity
 *   documents, single-sex provision, sports eligibility, and medical
 *   administration. The regime presents itself as record-keeping about a
 *   natural dichotomy, yet it runs on active enforcement (registry rules,
 *   eligibility testing, documentation checks, corrective surgery) and
 *   produces sharply asymmetric outcomes: transgender people can never hold a
 *   matching legal sex, intersex infants are surgically fitted to a slot, and
 *   the protective goods of the category accrue to those classified female at
 *   birth. This file instantiates ONE reading of the contested
 *   sex_gender_category kernel, the biology_reading, under which the boundary
 *   is fixed by immutable reproductive biology. The sibling readings
 *   (identity_reading, hybrid_reading) are separate constraints with their
 *   own epsilon values and victim sets, linked through the network section;
 *   they are not averaged into this file. The epsilon referent is the
 *   standing biology-keyed classification arrangement itself, assessed by
 *   this reading's own lights, which concede enforcement costs and intersex
 *   correction harms while treating the core binary as descriptive fact.
 *   Claimed type and metrics are authored independently: the claim is
 *   tangled_rope on structural grounds (a real coordination function, an
 *   asymmetric payer set, active enforcement); the metrics describe observed
 *   operation.
 *
 * KEY AGENTS:
 *   - - cis_women_protected_class: Protected class and partial cost-bearer (organized/constrained) — receives boundary-derived protections, bears testing and policing inside the category
 *   - - transgender_people: Primary target (moderate/trapped) — excluded from the matching category, legal sex uncorrectable
 *   - - intersex_individuals: Primary target (powerless/trapped) — forced into the binary in infancy, re-tested in adulthood
 *   - - intersex_normalization_medical_complex: Concentrated collector (institutional/constrained) — converts the binary's rigidity into a correction pathway
 *   - - gender_critical_advocacy_organizations: Organized defender-beneficiary (organized/mobile) — mandate depends on keeping the definition contested
 *   - - traditional_family_institutions: Doctrinal beneficiary (institutional/constrained) — the immutable binary anchors role doctrine
 *   - - civil_registration_authorities: Agenda setter (institutional/arbitrage) — owns the records and the amendment criteria
 *   - - sports_governing_bodies: Agenda setter (institutional/arbitrage) — owns eligibility lines and the testing that patrols them
 *   - - nonbinary_people: Excluded voice (powerless/trapped) — no slot, no seat in the conversation
 *   - - international_human_rights_monitors: Analytical observer (institutional/analytical) — documents costs without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.52).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.68).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Biological-Sex Category Boundary (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social/legal/political").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '635352c6-c36a-4c96-815e-66058ad63137').
narrative_ontology:cs_kernel_codification('635352c6-c36a-4c96-815e-66058ad63137', formalized).
narrative_ontology:cs_authority_grounding('635352c6-c36a-4c96-815e-66058ad63137', expertise).
narrative_ontology:cs_interpretation_layer_present('635352c6-c36a-4c96-815e-66058ad63137').
narrative_ontology:cs_reading_relation('635352c6-c36a-4c96-815e-66058ad63137', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('635352c6-c36a-4c96-815e-66058ad63137', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('635352c6-c36a-4c96-815e-66058ad63137', foundational, category_membership_fixed_by_immutable_biology).
narrative_ontology:cs_axiom_status(category_membership_fixed_by_immutable_biology, holdable).
narrative_ontology:cs_axiom_grounding('635352c6-c36a-4c96-815e-66058ad63137', category_membership_fixed_by_immutable_biology, empirically_contingent).
narrative_ontology:cs_axiom('635352c6-c36a-4c96-815e-66058ad63137', secondary, sex_based_protections_require_biological_boundary).
narrative_ontology:cs_axiom_status(sex_based_protections_require_biological_boundary, holdable).
narrative_ontology:cs_axiom_grounding('635352c6-c36a-4c96-815e-66058ad63137', sex_based_protections_require_biological_boundary, instrumental).
narrative_ontology:cs_reference_frame('635352c6-c36a-4c96-815e-66058ad63137', immutable_binary_dimorphism).
narrative_ontology:cs_drift_state('635352c6-c36a-4c96-815e-66058ad63137', contemporary_post_self_id_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('635352c6-c36a-4c96-815e-66058ad63137', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_protected_class).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, gender_critical_advocacy_organizations).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, traditional_family_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, intersex_normalization_medical_complex).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, transgender_people).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, cis_women_protected_class).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, gametic_dimorphism_doctrine).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, complementary_sex_roles_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classified female at birth and permanently so under this rule. Protective goods flow to them on the strength of that classification: refuge and prison placement keyed to female-pattern violence, maternity services, womens sports categories built on pre-pubertal male physiology, and standing to claim sex-based discrimination. Costs also travel along the same classification: mandatory eligibility testing in elite sport lands almost entirely on them, gender-presentation policing inside the category harasses masculine and racialized women first, and the historical subordination that moved through this classification is theirs. Leaving the class is not an option available to anyone in it.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_protected_class, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, cis_women_protected_class, payer).

% Campaign organizations whose mandate, funding, media access, and volunteer base consist in defending the biological definition of the categories. Litigation, lobbying, and publishing are their operating activity; a legislatively settled rival definition would leave them without a purpose, so they work continuously to keep the definition contested terrain rather than settled fact.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_critical_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Religious and customary bodies whose doctrines of complementary roles, marriage, ordination, and inheritance presuppose a fixed two-sex order given by nature. The immutable reading anchors those rules without further argument; a self-declaration criterion would force doctrinal revision they regard as unacceptable.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, traditional_family_institutions, beneficiary,
    institutional, generational, constrained, global).

% Pediatric endocrinology, urology, and psychology services that manage infants born with ambiguous anatomy under the binary: assign a sex, normalize the body to the assignment with surgery and hormones, and follow the patient for life. Fees, publications, and professional standing flow from the correction pathway; the rigidity of the two-slot rule is what makes normalization the default response to variation.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_normalization_medical_complex, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, intersex_normalization_medical_complex, agenda_setter).

% Vital-statistics offices, registrars, and courts that record sex at birth and control every subsequent amendment. They author the documentary criteria, decide what evidence can change a record, and administer the penalties for mismatched papers. Their filing systems and forms are the enforcement backbone; changing the classification basis means re-engineering them.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, civil_registration_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Athletics federations that set womens-category eligibility, commission the testing that polices the boundary (chromosome screening in earlier decades, testosterone thresholds now), and absorb the litigation and reputational fallout when tests flag athletes. The flagged athletes are overwhelmingly women with intersex variations from poorer countries, and the testing exists to defend a line the bodies themselves keep having to redraw.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Assigned a sex at birth that their identity contradicts, and under this rule unable ever to correct it in law. Toilets, shelters, prisons, sport, and health services all read them as their assigned sex; using facilities matching their identity is treated as deception or intrusion. What the rule offers as an exit is permanent concealment or a lifetime effort to conform to the assigned role, at the cost of the identity they actually hold.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, transgender_people, payer,
    moderate, biographical, trapped, global).

% Born with chromosomal, gonadal, or anatomical variation the two-slot scheme has no place for. The rule forces assignment, historically carried out through nonconsensual infant surgery and hormone regimens meant to produce unambiguous bodies, and then tests them again in adulthood when their bodies fail eligibility screens or document checks. Medical secrecy and instructed shame kept their testimony out of the record for decades.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).

% Fit neither slot and are therefore invisible to the entire apparatus: not a category to join, not a constituency consulted, not a line on the form. They would contest the premise that every person sorts into exactly two sexes, but the registration systems, eligibility rules, and legislative debates all proceed as though the only open question were where to draw the single line.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, nonbinary_people, excluded,
    powerless, biographical, trapped, global).

% UN treaty bodies, special rapporteurs, and national bioethics commissions that document nonconsensual infant surgery, athlete sex testing, and legal identity fixity, and publish findings the enforcing institutions are free to ignore. They hold no enforcement power over the arrangement; their product is the record.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, international_human_rights_monitors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, intersex_normalization_medical_complex).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a single two-slot classification that solves real problems once, centrally: sex-specific medical dosing and screening, demographic and epidemiological statistics, provision targeted at female-pattern violence, and sports categories built on pre-pubertal male physiology.
% TRANSFER_FUNCTION: Moves category membership and everything attached to it: assigns every person to one of two legal sexes at birth; delivers the protective goods of the female category to those classified female at birth; moves enforcement burdens (document checks, eligibility testing, corrective surgery) onto those whose bodies or identities do not match their assignment.
% ABSENT_VOICES: Nonbinary people are absent from the conversation entirely because the two-slot ontology gives them no slot and therefore no seat. Intersex adults operated on as infants were kept absent for decades by medical secrecy and instructed shame, and speak now mainly through survivor organizations and treaty-body submissions. Transgender people are present only where jurisdictions grant them standing; under this reading they enter the debate as objects of the rule, not parties to it.
% DISAPPEARANCE_RATIONALE: If the biology-keyed classification vanished overnight, every jurisdiction would have to re-found legal sex on some other basis (self-declaration, hybrid gatekeeping, or abolition of the category), reissue identity documents, rewrite sports eligibility, and re-justify or dismantle single-sex provision. Medicine and statistics would rebuild their categories on explicit clinical or statistical criteria. Nothing about the surrounding world stays put: the classification is load-bearing for registration law, sport, corrections, and shelter systems.
% FOUNDING_PROBLEM: An administrable two-slot population sort keyed to perceived reproductive role: states needed to count, conscript, tax, educate, and regulate a population through a simple binary recorded at birth, and later sex-based anti-discrimination law needed an evidentiary category to protect.
% FOUNDING_PROBLEM_CORROBORATION: Historical demography and vital-registration scholarship corroborates the census-and-administration origin; public-health and clinical literature corroborates the continuing medical and statistical uses. Intersex survivor testimony and UN treaty-body findings corroborate the costs from outside every benefiting seat. No corroborating source outside the beneficiary set attests that the founding problem required excluding transgender people from the matching category or surgically fitting intersex infants to a slot; those features entered through enforcement, not design.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.52 because the arrangement genuinely delivers protective and medical coordination while imposing severe, concentrated costs on two small classes: uncorrectable legal identity for transgender people and nonconsensual normalization for intersex infants. Because epsilon here is indexed to the biology reading's own lights, it is materially lower than an identity-reading authoring of the same arrangement would be: the reading counts the boundary as tracking reality and prices only the enforcement and edge-case harms it concedes. Suppression (0.68) is a raw structural figure, unscaled by power or scope: legal sex is fixed at birth with no self-declaration route, infant surgery proceeds without consent, and document checks police the line. Theater ratio (0.42) reflects enforcement that increasingly performs vigilance rather than achieving its stated aim: decades of athlete sex testing caught essentially no cisgender cheats while humiliating intersex women, and facility-policing campaigns target a vanishingly rare event. Accessibility collapse is low (0.35): rival criteria are not hypothetical, since self-ID statutes and third markers operate in multiple jurisdictions, so alternatives remain visibly available. Resistance is high (0.70): sustained litigation (the Semenya caseline), legislative contests over self-ID, and intersex survivor movements press the arrangement continuously. All three temporal series share one grid (points 0, 12, 25, 37, 50, 62, 74) and terminate at the scalar values above. The trajectories show an enforcement ratchet rather than a cycle: extraction, theater, and suppression requirement all rise as surgical and hormonal transition made the supposedly immutable boundary contestable, forcing enforcement to thicken to hold it. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity growth, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the payer seats (transgender_people, intersex_individuals) the arrangement is a closed trap: every sex-keyed institution reads them against their identity or their body, and the exit offered is concealment. From the protected-class seat the same structure is a shield with a tax attached: protections received, testing and policing endured. From the agenda-setter seats it is routine administration: forms, thresholds, appeals. From the collector seat (the medical complex) it is a referral pipeline. The engine derives these per-seat classifications from the declared roles, power, and exit options; nothing in the claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure maps to directionality as follows. cis_women_protected_class declares beneficiary with a secondary payer position: the derivation should seat them well short of the pure-beneficiary pole, near symmetric-leaning-beneficiary, because protections flow in while testing, policing, and the category's historical burdens flow out; no override is needed because the secondary_role carries the correction. gender_critical_advocacy_organizations and traditional_family_institutions are clean beneficiaries (d near 0): the arrangement subsidizes their mandate and doctrine at little direct cost to them. intersex_normalization_medical_complex is a beneficiary that also administers part of the machinery (secondary agenda_setter), collecting fees and professional standing from the correction pathway. transgender_people and intersex_individuals are the declared victims with trapped exits: full-target directionality, amplified by the absence of any arbitrage-grade exit, since chromosomes cannot be changed and records cannot be amended under this rule. nonbinary_people are excluded rather than seated: they stand outside the conversation the constraint structures. international_human_rights_monitors take the analytical seat. Scope amplification applies modestly: the arrangement operates globally, so verification of fair enforcement is harder and effective extraction scales up somewhat on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, an administrable two-slot population sort keyed to reproductive role, is partially live: sex-specific medicine, demographic statistics, and provision for female-pattern violence still consume the classification. But the enforcement apparatus has thickened well past that need: eligibility testing, document policing, and corrective surgery defend the boundary itself rather than the services it was built to deliver. The tangled_rope classification prevents two opposite mislabels. Reading the arrangement as pure extraction ignores the protective and medical coordination that keeps the protected-class seat, health systems, and sports categories attached to it; reading it as pure coordination ignores that its costs land on two classes who cannot exit and did not consent. Hence tangled_rope rather than snare or rope. founding_problem_status is contested rather than dead, so the mismatch consumer should not fire a zombie flag, but the rising theater and suppression series mark the enforcement layer as the part drifting away from the founding function. mandatrophy_resolved is deliberately not declared: the mandate has decayed at the enforcement margin, not expired overall.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_vs_discovered_binary,
    'Is the two-slot classification a discovery about a natural discontinuity, or an administrative construction that borrows biology''s authority while its slot boundaries track state needs?',
    'Comparative history of registration systems, the anthropological record of societies maintaining more than two gender slots, and analysis of whether legal cut-points track biological clustering or administrative convenience.',
    'If constructed, mountain certification fails outright and the constraint computes as built coordination-plus-extraction; if discovered, part of the enforcement burden is the irreducible price of tracking a real structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_discovered_binary, conceptual, 'Natural-law versus constructed status of the binary the reading defends.').

omega_variable(
    intersex_correction_harm_scope,
    'How many intersex people have undergone nonconsensual normalization interventions, and what is the measured long-term harm?',
    'Clinical registries and longitudinal outcome studies of former pediatric patients, plus litigation records from jurisdictions that have restricted infant surgery.',
    'Verified large-scale harm raises effective extraction on the intersex payer seat and could push that seat''s computed type toward snare; negligible verified harm would support the medical complex''s framing of correction as treatment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_correction_harm_scope, empirical, 'Scale and severity of forced binarization harms.').

omega_variable(
    protection_extraction_separability,
    'Are the protective functions the binary serves (single-sex provision, sex-specific medicine, sports categories) separable from the exclusionary enforcement that maintains the boundary?',
    'Jurisdictional natural experiments: outcomes for shelters, prisons, and sport in self-ID versus biology-keyed jurisdictions, controlling for funding levels and incidence rates.',
    'If separable, the exclusion layer is pure extraction riding on real coordination and the tangled_rope reading overstates the coordination defense; if inseparable, part of the measured extraction is the price of the protection itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_extraction_separability, conceptual, 'Whether the coordination and enforcement components are structurally separable.').

omega_variable(
    kernel_criterion_underdetermination,
    'This constraint is one reading of the sex_gender_category kernel; the sibling readings (identity_reading, hybrid_reading) locate the membership criterion elsewhere. What would adopting a sibling change structurally, and is the disagreement resolvable within one framework?',
    'No empirical resolution: the readings disagree on who fixes the boundary and on what evidence. Resolution arrives only through which reading a jurisdiction codifies; the structural deltas are tracked in the sibling files.',
    'Under identity_reading, transgender_people leave this constraint''s victim set and cis_women_protected_class loses its boundary-derived protections; under hybrid_reading a gated intermediate seat appears and enforcement costs shift to medical gatekeeping. This file''s classification is invariant to the choice, but corpus-level comparison across the kernel is not.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_criterion_underdetermination, conceptual, 'Committer-frame routing: one reading of a contested kernel; siblings are separate constraints.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression borne by transgender and intersex people structural (legal fixity, documentation barriers, surgical coercion) or internalized (shame and secrecy instilled by medical management, anticipated rejection)?',
    'Post-liberalization trajectories: jurisdictions that moved to self-ID or banned infant surgery. If suppression measures fall with the barrier, it was structural; if they persist, a substantial fraction is internalized.',
    'Internalized suppression travels with the target after exit, raising the constraint''s effective suppression above the structural measure and worsening the payer seats'' computed position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of measured suppression on the payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_bio_tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(sgc_bio_tr_t0, observed).
narrative_ontology:measurement(sgc_bio_tr_t12, sex_gender_category__biology_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(sgc_bio_tr_t12, observed).
narrative_ontology:measurement(sgc_bio_tr_t25, sex_gender_category__biology_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement_basis(sgc_bio_tr_t25, observed).
narrative_ontology:measurement(sgc_bio_tr_t37, sex_gender_category__biology_reading, theater_ratio, 37, 0.26).
narrative_ontology:measurement_basis(sgc_bio_tr_t37, observed).
narrative_ontology:measurement(sgc_bio_tr_t50, sex_gender_category__biology_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(sgc_bio_tr_t50, observed).
narrative_ontology:measurement(sgc_bio_tr_t62, sex_gender_category__biology_reading, theater_ratio, 62, 0.38).
narrative_ontology:measurement_basis(sgc_bio_tr_t62, observed).
narrative_ontology:measurement(sgc_bio_tr_t74, sex_gender_category__biology_reading, theater_ratio, 74, 0.42).
narrative_ontology:measurement_basis(sgc_bio_tr_t74, observed).

% Extraction over time
narrative_ontology:measurement(sgc_bio_be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(sgc_bio_be_t0, observed).
narrative_ontology:measurement(sgc_bio_be_t12, sex_gender_category__biology_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(sgc_bio_be_t12, observed).
narrative_ontology:measurement(sgc_bio_be_t25, sex_gender_category__biology_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement_basis(sgc_bio_be_t25, observed).
narrative_ontology:measurement(sgc_bio_be_t37, sex_gender_category__biology_reading, base_extractiveness, 37, 0.46).
narrative_ontology:measurement_basis(sgc_bio_be_t37, observed).
narrative_ontology:measurement(sgc_bio_be_t50, sex_gender_category__biology_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(sgc_bio_be_t50, observed).
narrative_ontology:measurement(sgc_bio_be_t62, sex_gender_category__biology_reading, base_extractiveness, 62, 0.5).
narrative_ontology:measurement_basis(sgc_bio_be_t62, observed).
narrative_ontology:measurement(sgc_bio_be_t74, sex_gender_category__biology_reading, base_extractiveness, 74, 0.52).
narrative_ontology:measurement_basis(sgc_bio_be_t74, observed).

% Suppression requirement over time
narrative_ontology:measurement(sgc_bio_su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(sgc_bio_su_t0, observed).
narrative_ontology:measurement(sgc_bio_su_t12, sex_gender_category__biology_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(sgc_bio_su_t12, observed).
narrative_ontology:measurement(sgc_bio_su_t25, sex_gender_category__biology_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(sgc_bio_su_t25, observed).
narrative_ontology:measurement(sgc_bio_su_t37, sex_gender_category__biology_reading, suppression_requirement, 37, 0.64).
narrative_ontology:measurement_basis(sgc_bio_su_t37, observed).
narrative_ontology:measurement(sgc_bio_su_t50, sex_gender_category__biology_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(sgc_bio_su_t50, observed).
narrative_ontology:measurement(sgc_bio_su_t62, sex_gender_category__biology_reading, suppression_requirement, 62, 0.67).
narrative_ontology:measurement_basis(sgc_bio_su_t62, observed).
narrative_ontology:measurement(sgc_bio_su_t74, sex_gender_category__biology_reading, suppression_requirement, 74, 0.68).
narrative_ontology:measurement_basis(sgc_bio_su_t74, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legal sex classification' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints, one per reading of the sex_gender_category kernel. Each has its own epsilon, beneficiary/victim structure, and enforcement profile: the biology reading (this file) holds cis women as the protected class and transgender and intersex people as payers; the identity reading dissolves the biological boundary and relocates the victim set; the hybrid reading builds a gated middle with medical gatekeeping as its enforcement surface. The biology reading is upstream of the hybrid reading, which inherits its biological anchor, and stands in direct criterion contradiction with the identity reading. Linking all three lets contamination propagate: erosion of the biological anchor in this file destabilizes the hybrid's gatekeeping and removes the identity reading's principal opposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
