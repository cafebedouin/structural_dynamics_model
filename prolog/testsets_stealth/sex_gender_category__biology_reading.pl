% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Natal-Biology Determination of Legal Sex/Gender Category
 *   domain: social ontology/legal classification/identity politics
 *
 * SUMMARY:
 *   The constraint under examination is the rule that membership in the legal
 *   and social categories 'woman' and 'man' is fixed by immutable
 *   reproductive biology — chromosomes and anatomy as recorded at birth — and
 *   cannot subsequently change. It is presented by its defenders as the plain
 *   tracking of a natural binary; it operates as an actively maintained
 *   classification regime spanning birth registration, equality-law
 *   exemptions, facility-access rules, sports eligibility testing, and
 *   document control. The arrangement delivers a genuine coordination good (a
 *   single verifiable classification consumed by medicine, law, sport, and
 *   administration) while imposing asymmetric, enforced burdens on trans and
 *   intersex people, who cannot alter their classification by any available
 *   means. KEY AGENTS (by structural relationship): - cis_women: Primary
 *   beneficiary (organized/constrained) — hold exclusive claim to the 'woman'
 *   category - cis_men: Secondary beneficiary (organized/constrained) —
 *   symmetric claim to 'man', lower contest salience - trans_women: Primary
 *   target (powerless/trapped) — excluded from 'woman' categorically -
 *   trans_men: Primary target (powerless/trapped) — excluded from 'man'
 *   categorically - intersex_individuals: Primary target (powerless/trapped)
 *   — forced into binary assignment - legislatures_and_courts: Agenda setter
 *   (institutional/arbitrage) — define and redefine the criterion -
 *   womens_sports_governing_bodies: Enforcing administrator
 *   (institutional/constrained) - medical_registration_authorities:
 *   Registration administrator (institutional/constrained) -
 *   gender_critical_advocacy_groups: Organized beneficiary (organized/mobile)
 *   - human_rights_monitoring_bodies: Analytical observer
 *   (institutional/analytical). Per the committer frame, this file authors
 *   ONLY the biology reading of the sex_gender_category kernel as a clean,
 *   epsilon-invariant constraint; the identity and hybrid readings are
 *   separate constraint files, and the selection among readings is carried by
 *   the kernel_reading_selection omega rather than folded into this
 *   classification.
 *
 * KEY AGENTS:
 *   - cis_women: Primary beneficiary (organized/constrained) — exclusive category claim, minimal personal enforcement burden
 *   - cis_men: Secondary beneficiary (organized/constrained) — symmetric claim, lower contest salience
 *   - trans_women: Primary target (powerless/trapped) — categorical exclusion from 'woman'
 *   - trans_men: Primary target (powerless/trapped) — categorical exclusion from 'man'
 *   - intersex_individuals: Primary target (powerless/trapped) — forced binary assignment, historical corrective surgery
 *   - legislatures_and_courts: Agenda setter (institutional/arbitrage) — control the criterion's content
 *   - womens_sports_governing_bodies: Enforcing administrator (institutional/constrained) — run eligibility testing
 *   - medical_registration_authorities: Registration administrator (institutional/constrained) — record natal sex, deliver transition care
 *   - gender_critical_advocacy_groups: Organized beneficiary (organized/mobile) — mission scales with boundary salience
 *   - human_rights_monitoring_bodies: Analytical observer (institutional/analytical) — shift legitimacy conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.58).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.6).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Natal-Biology Determination of Legal Sex/Gender Category").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social ontology/legal classification/identity politics").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '7dd9423a-b6ca-47f7-8c01-ea33655f5d84').
narrative_ontology:cs_kernel_codification('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', formalized).
narrative_ontology:cs_authority_grounding('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', lineage).
narrative_ontology:cs_interpretation_layer_present('7dd9423a-b6ca-47f7-8c01-ea33655f5d84').
narrative_ontology:cs_reading_relation('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', foundational, category_membership_tracks_natal_biology).
narrative_ontology:cs_axiom_status(category_membership_tracks_natal_biology, holdable).
narrative_ontology:cs_axiom_grounding('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', category_membership_tracks_natal_biology, empirically_contingent).
narrative_ontology:cs_axiom('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', foundational, sex_based_provisions_require_exclusive_natal_criteria).
narrative_ontology:cs_axiom_status(sex_based_provisions_require_exclusive_natal_criteria, holdable).
narrative_ontology:cs_axiom_grounding('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', sex_based_provisions_require_exclusive_natal_criteria, instrumental).
narrative_ontology:cs_reference_frame('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', birth_certificate_sex_fixity).
narrative_ontology:cs_drift_state('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', contemporary_self_id_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7dd9423a-b6ca-47f7-8c01-ea33655f5d84', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_men).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, gender_critical_advocacy_groups).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, womens_sports_governing_bodies).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_men).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, medical_registration_authorities).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, immutable_sex_binary_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are recorded female at birth and hold an unconditional lifetime claim to the 'woman' category. They access single-sex services, sports categories, and data collection defined around natal sex with no documentation burden to prove membership. Incidental friction reaches some of them: gender-presentation policing at facilities occasionally sweeps up masculine-presenting cis women, and elite athletes undergo sex-verification procedures. They cannot opt out of being classified, but the classification works in their favor.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, generational, constrained, global).

% Hold the 'man' category symmetrically by natal recording. Public contest concentrates far less on this category, so day-to-day stakes are lower, but the same registration rule secures their claim and the same enforcement machinery stands behind it.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_men, beneficiary,
    organized, generational, constrained, global).

% Organize to defend the natal-sex criterion through legislation, litigation, and media campaigns. Membership, funding, and political relevance scale with the boundary's salience; if the criterion were replaced, these organizations would need to re-found their mission around something else. Their exit is mobile in principle — advocacy organizations pivot — but their current resource base is tied to this specific fight.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_critical_advocacy_groups, beneficiary,
    organized, biographical, mobile, national).

% Set and enforce eligibility rules for female competitive categories, operate sex-verification and hormone-threshold testing, and adjudicate appeals. Clear natal criteria reduce their per-case adjudication load, but every contested athlete case generates litigation, scientific dispute, and reputational cost. They are bound by their own international federation structures and cannot unilaterally abandon category testing.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, womens_sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, womens_sports_governing_bodies, beneficiary).

% Enact and interpret the definitions that fix category membership: birth-registration statutes, equality-law exemptions, eligibility rules, document-amendment powers. They can redefine the criterion at will and absorb the resulting political conflict; their exposure to the rule is political rather than personal, and jurisdictions arbitrage between rival criteria by watching each other's reforms.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Record sex at birth on the basis of external anatomy — and, in disputed cases, chromosomes or gonads — issuing the documents that anchor all later legal claims. In some jurisdictions and eras they performed infant 'normalizing' surgery on children with sex-development variations to make anatomy conform to the binary. They simultaneously deliver transition-related care, placing them on both sides of the boundary they administer.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_registration_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, medical_registration_authorities, beneficiary).

% Are recorded male at birth and remain excluded from the 'woman' category regardless of transition, identity, or documents issued elsewhere. They face denial of access to women's facilities and services, elevated physical risk in men's spaces, exclusion from women's sport, and public scrutiny of documents and bodies. No change available to them — medical, legal, or residential — alters their classification under this rule; moving to another jurisdiction changes the local rule at best, not the category logic they live under globally.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, trapped, global).

% Occupy the mirrored position: recorded female at birth, excluded from the 'man' category, with parallel exposures in facilities, services, sport, and documentation. The public contest centers less on them, which reduces political attention to their situation without reducing the structural bind.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_men, payer,
    powerless, biographical, trapped, global).

% Are born with sex characteristics that do not fit standard binary definitions. The rule forces assignment to one of two categories at birth — historically enforced by non-consensual infant surgery intended to make anatomy conform — and their existence contradicts the criterion's premise of two clean classes. They cannot consent to or decline the assignment made on their behalf, and no later self-understanding changes the recorded category.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).

% Treaty bodies, special rapporteurs, and regional courts review the rule against anti-discrimination, privacy, and bodily-autonomy commitments, publish findings, and press states toward recognition-based alternatives. They decide nothing domestically but shift the legitimacy conditions under which national authorities maintain or revise the criterion.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, externally verifiable binary classification established once at birth and consumed by medicine (epidemiology, dosing, screening), law (single-sex provisions), sport (category eligibility), and administration (documents, vital statistics), avoiding per-context re-adjudication of membership.
% TRANSFER_FUNCTION: Moves the exclusive claim to the 'woman' and 'man' categories — and the services, spaces, competitions, and protections attached to them — to people as classified at birth; moves the costs of that exclusivity (denied recognition, facility risk, eligibility bars, scrutiny, historical corrective surgery) onto trans and intersex people.
% ABSENT_VOICES: Trans and intersex people had no seat when birth-registration frameworks and modern eligibility rules were codified; intersex infants could not consent to the normalizing procedures that enforced their assignment. Today they appear mainly as litigants and petitioners at the margins of the rule-making bodies. Their objection — that the criterion misclassifies them at the cost of safety, dignity, and bodily autonomy — enters the record through courts and treaty bodies rather than through the legislatures and federations that set the rule.
% DISAPPEARANCE_RATIONALE: If the natal-determination rule vanished overnight, every sex-classified institution would reorganize: prisons, shelters, sports categories, document systems, and statistical agencies would need replacement criteria immediately; pending litigation would collapse or transform; advocacy industries on both sides would lose their object. Nothing about the surrounding arrangements is indifferent to this rule — it is load-bearing across medicine, law, sport, and administration simultaneously.
% FOUNDING_PROBLEM: Fix an administrable, verifiable criterion for civil registration and for the operation of sex-based provisions — vital statistics, single-sex services, competitive categories — so membership could be established once, at birth, without per-case adjudication.
% FOUNDING_PROBLEM_CORROBORATION: Civil registries and medical professional bodies corroborate that natal-sex recording continues to solve real registration and clinical problems. Outside the beneficiary set, trans-led organizations, intersex advocacy (including United Nations testimony against non-consensual infant surgery), and human rights treaty bodies attest that the exclusive-natal criterion specifically is no longer necessary for those functions, pointing to recognition-based jurisdictions where registration persisted after reform. Sports-science authorities dispute in both directions depending on federation. No single external source attests the whole genealogy; corroboration splits along the same lines as the kernel contest itself.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58: the burden on trans and intersex people is total within the classification domain (no medical, legal, or residential change alters their assigned category), yet the arrangement also performs real registration and clinical work, so the extraction rides a functioning structure rather than replacing one. Suppression is 0.60 and structural, not interpersonal: persistence depends on statutes excluding self-identification alternatives, facility-access rules, eligibility testing, and document controls — barriers external to the targeted agents, which is why exit_options for the payer seats is trapped rather than identity_locked. Theater is 0.42 and rising: a growing share of enforcement activity defends the boundary performatively (athlete screening programs that historically screened almost nobody successfully, document checks at facility doors, litigation staged for signaling) relative to the functional registration core. Accessibility_collapse is 0.40 — alternatives demonstrably exist and operate in recognition-based jurisdictions, so understanding the constraint does not collapse exits. Resistance is 0.70 — sustained movement litigation, treaty-body pressure, and jurisdictional defection; note the coalition effect: trans and intersex constituencies litigate jointly, so effective resistance exceeds what individually powerless seats could mount alone. All three temporal series share one grid (t=0..50) and rise together: as trans visibility and contestation grew, the enforcement machinery hardened rather than relaxed — an enforcement ratchet, not decay. Claim and metrics are independent authored facts: I claim tangled_rope because the structure possesses both a genuine coordination function and enforced asymmetric extraction; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the cis-women seat the arrangement is a shield: an unconditional category claim, no documentation burden, occasional spillover friction from presentation policing. From the trans and intersex seats the same rule is total classification captivity: exclusion from matching facilities and categories, elevated physical risk in mismatched settings, and — for intersex infants historically — non-consensual surgery to enforce the binary. Administrator seats (sports federations, registries) experience manageable adjudication punctuated by costly contested cases. The agenda-setter seat experiences the rule as revisable policy whose costs are political rather than personal. The engine derives these divergent per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (cis_women, cis_men, gender_critical_advocacy_groups, womens_sports_governing_bodies) drive those seats toward the beneficiary end of directionality — the constraint subsidizes them, and their exit positions (constrained but sheltered, or mobile for advocacy groups) damp further. Victim declarations (trans_women, trans_men, intersex_individuals) drive those seats toward the full-target end, amplified by trapped exit: no arbitrage, no jurisdictional escape that fully escapes the criterion, no bodily route out. Administrators sit mid-range — they run the machinery and absorb litigation costs. The observer seat is analytical and feeds no extraction arithmetic. Scope is national-to-global, which scales effective extraction upward for the trapped targets because verification of compliance is diffuse and hard to audit. No directionality overrides are needed: the derivation from beneficiary/victim declarations plus exit options reproduces the structural relationships directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an administrable, verifiable criterion for civil registration and sex-based provision — is partly live: registries and clinical medicine still consume natal-sex data, so the arrangement is not a zombie running on inertia alone. What is contested is the exclusivity of the criterion: recognition-based jurisdictions demonstrate the registration function survives without the exclusive-natal rule. Classifying this as tangled_rope guards against both symmetrical errors: reading it as pure coordination erases the enforced burden on trans and intersex people; reading it as pure extraction erases the real registration and clinical function that would survive any reform. No sunset logic applies — the contest is over the criterion's content, not its retirement — so no mandatrophy resolution is declared; the R5 interview records the founding problem as contested rather than dead, and the disappearance verdict is world_rearranges because every sex-classified institution would reorganize overnight if the rule vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the sex_gender_category kernel — biology_reading (this file), hybrid_reading, or identity_reading — should govern legal category membership?',
    'Jurisdictional adoption and repeal cycles, constitutional litigation outcomes, and longitudinal comparison of recognition-based versus natal-criterion jurisdictions.',
    'Adopting identity_reading dissolves the trans and intersex victim sets and relocates enforcement effort to document-fraud prevention; hybrid_reading installs a medical-gatekeeping bureaucracy with partial exclusion; retaining this reading preserves the current victim set and its enforcement costs. Each outcome recomputes epsilon and the beneficiary/victim structure from scratch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-frame uncertainty: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    binary_naturalness_vs_construction,
    'Is the two-category structure a natural-kind fact that the law merely tracks, or a constructed simplification imposed on a messier biological distribution (intersex variation, chromosomal mosaicism, discordant anatomy)?',
    'Clinical genetics evidence on the prevalence and stability of sex-development variation, combined with philosophical analysis of whether the registered binary carves nature at its joints or flattens it.',
    'If constructed, the constraint loses any natural-law immunity it is claimed to have and faces the same falsity-of-summit pressure as any defended classification; if genuinely kind-tracking, part of the measured burden is irreducible classification cost rather than removable overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_naturalness_vs_construction, conceptual, 'Whether the binary is discovered or imposed — the naturality claim beneath the criterion.').

omega_variable(
    protection_exclusion_separability,
    'Do the safety and fairness goods attributed to the exclusive category (single-sex service safety, competitive fairness) actually require the natal-biology criterion, or are they deliverable under alternative criteria?',
    'Comparative outcome data from self-identification and hybrid jurisdictions — incident rates in single-sex services, elite competition results — plus facility-level studies.',
    'If separable, the exclusion component is overhead riding on a real coordination function and the constraint sits at the extractive end of its hybrid range; if inseparable, part of the burden borne by trans and intersex people is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_exclusion_separability, empirical, 'Whether the protective function and the exclusionary criterion are structurally separable.').

omega_variable(
    boundary_enforcement_spillover,
    'How much of the boundary-enforcement burden spills beyond the trans and intersex populations onto cis women and gender-nonconforming people (presentation policing at facilities, sex-verification testing of athletes, documentation challenges)?',
    'Facility-challenge incident records, the documented history of athlete sex-testing programs, and litigation dockets naming cis claimants.',
    'High spillover raises effective burden beyond the declared victim set and degrades the beneficiary seat''s experience of protection; low spillover supports a cleaner separation between who pays and who is shielded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_enforcement_spillover, empirical, 'Distribution of enforcement costs across the beneficiary class itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__biology_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(sex__tr_t10, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__biology_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__biology_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(sex__tr_t30, observed).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__biology_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(sex__tr_t40, observed).
narrative_ontology:measurement(sex__tr_t50, sex_gender_category__biology_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(sex__tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__biology_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(sex__be_t10, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__biology_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__biology_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement_basis(sex__be_t30, observed).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__biology_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(sex__be_t40, observed).
narrative_ontology:measurement(sex__be_t50, sex_gender_category__biology_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(sex__be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__biology_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement_basis(sex__su_t10, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__biology_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__biology_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(sex__su_t30, observed).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__biology_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(sex__su_t40, observed).
narrative_ontology:measurement(sex__su_t50, sex_gender_category__biology_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement_basis(sex__su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legal sex/gender category' decomposes into three rival readings of one kernel — biology_reading (this file), identity_reading, and hybrid_reading. Each instantiates a different determination criterion with a different victim set and enforcement cost structure; their epsilon values differ because they are different constraints, not one constraint viewed from multiple angles. The biology reading is upstream in institutional terms (it is the incumbent criterion most jurisdictions still encode), and its persistence shapes the operating environment of the sibling readings; edges here link the family members for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
