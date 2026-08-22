% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman-Category Membership: Intersex-Accommodation Reading
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   'Woman' is a contested kernel with three live readings: strict
 *   typical-case biology, gender identity, and this file's
 *   intersex-accommodation reading, which keeps membership biologically
 *   grounded while acknowledging sex as a non-binary spectrum — 'woman'
 *   includes female-typical biology plus intersex variations that do not fit
 *   the male category. This story instantiates only that reading, with one
 *   epsilon, one beneficiary/victim structure, and no averaging across
 *   siblings. The arrangement runs cheaply across most civil domains —
 *   documents, facilities, records — where the accommodation costs little and
 *   extracts almost nothing; its sharp edge is elite sport, where drawing a
 *   competitive line through a biological spectrum produces testing regimes,
 *   mandated hormonal suppression, and exclusion, with the Semenya litigation
 *   as the emblematic case. Claim and metrics are independent: claimed_type
 *   tangled_rope states what I believe structurally true (genuine
 *   coordination function plus asymmetric extraction held by active
 *   enforcement); the metrics describe the operation as I assess it. Family
 *   links to the sibling readings are declared in
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - sports_governing_bodies: Agenda setter (institutional/arbitrage) — writes eligibility rules, runs testing panels, collects category control and legitimacy
 *   - dsd_elite_athletes: Primary target (powerless/identity_locked) — bears testing, mandated suppression, and exclusion; the Semenya case is emblematic
 *   - intersex_individuals: Dual-positioned (moderate/constrained) — recognized in civil domains, exposed at the sport edge
 *   - female_category_athletes: Beneficiary (organized/mobile) — receive the protected competitive field
 *   - trans_women_denied_membership: Secondary target (organized/identity_locked) — denied membership by the biological basis
 *   - clinical_classification_authorities: Co-administrator (institutional/arbitrage) — holds the definitional pen and profits professionally
 *   - intersex_advocacy_organizations: Excluded from formal rule-making (organized/constrained) — contests via litigation and treaty bodies
 *   - human_rights_courts: Analytical observer (institutional/analytical) — CAS, Swiss Federal Court, ECHR review what the rules may lawfully do
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.55).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.45).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman-Category Membership: Intersex-Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '8320cd21-62d8-4ebe-ac1e-3b838e6cf7de').
narrative_ontology:cs_kernel_codification('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', formalized).
narrative_ontology:cs_authority_grounding('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', expertise).
narrative_ontology:cs_interpretation_layer_present('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de').
narrative_ontology:cs_reading_relation('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', woman_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', foundational, biological_sex_is_real_but_non_binary).
narrative_ontology:cs_axiom_status(biological_sex_is_real_but_non_binary, holdable).
narrative_ontology:cs_axiom_grounding('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', biological_sex_is_real_but_non_binary, empirically_contingent).
narrative_ontology:cs_axiom('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', foundational, membership_tracks_body_not_identity).
narrative_ontology:cs_axiom_status(membership_tracks_body_not_identity, holdable).
narrative_ontology:cs_axiom_grounding('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', membership_tracks_body_not_identity, conventional).
narrative_ontology:cs_reference_frame('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', biological_spectrum_membership).
narrative_ontology:cs_drift_state('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', contemporary_post_semenya_litigation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8320cd21-62d8-4ebe-ac1e-3b838e6cf7de', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, female_category_athletes).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, dsd_elite_athletes).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, trans_women_denied_membership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, clinical_classification_authorities).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_individuals).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, sex_as_biological_spectrum).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, protected_female_category_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and revise eligibility rules for the female category in international competition. After chromosome screening was abandoned, they moved to case-by-case assessment of athletes with differences of sex development, culminating in regulations requiring affected athletes to medically lower natural testosterone below a set threshold to remain eligible. They commission much of the scientific literature they rely on, run the testing panels, and answer to member federations and sponsors. Rewriting the rules or creating open categories is available to them in principle, but each rewrite carries legal, diplomatic, and sponsorship cost.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Compete internationally, several with differences of sex development producing endogenous testosterone in the male typical range. To keep racing they must undergo genetic and hormonal testing and either take suppressive medication with side effects they did not choose or leave the category, including the distances they have trained for since childhood. Arbitration appeals have gone against them; at least one has been unable to race her signature event for years. Walking away means giving up the career and public life that constitutes who they are.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, dsd_elite_athletes, payer,
    powerless, biographical, identity_locked, global).

% People born with variations of sex characteristics that fit neither standard category. In civil life this reading's approach gives them recognition as women where their biology is not male-typical, protecting them from forced assignment and from the surgical normalization pressure that stricter binaries generate. Nearly all the athletes caught in sports eligibility testing come from this same population, so recognition gained in documents and daily life can be reversed at the stadium gate.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, intersex_individuals, payer).

% Compete in the female category without DSD-related scrutiny. They receive a competitive field bounded by the eligibility rules and are the constituency to whom fairness is promised; federations consult them and their representative bodies before changing rules. Their stake cuts both ways in practice: they benefit from the protected field but carry no testing burden, and some publicly defend the rules while others question them.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, female_category_athletes, beneficiary,
    organized, biographical, mobile, global).

% Women whose category membership this biological reading does not recognize, since their bodies are not female-typical and most have no intersex variation to accommodate. Where this reading governs eligibility or provision, they are directed to male or mixed categories and spaces regardless of identity or lived life. The only available compliance is accepting classification they experience as misgendering; no version of the arrangement recognizes them as women.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, trans_women_denied_membership, payer,
    organized, biographical, identity_locked, global).

% Pediatric endocrinologists, sports physicians, and testing panels who assign sex at birth, certify DSD diagnoses, and operate the medical side of eligibility assessment. Whose biology fits which category is decided in their clinics and laboratories. The arrangement sustains their professional authority over sexed classification and generates clinical and consulting work; they bear little personal cost from its operation.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, clinical_classification_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, clinical_classification_authorities, beneficiary).

% Organizations led by and for people with variations of sex characteristics. They campaign against non-consensual infant normalization surgery, filed briefs in the Semenya litigation, and press human-rights bodies on sports eligibility rules, but hold no seat in federation congresses or eligibility panels where the rules are actually written. Their objection is registered in amicus filings and treaty-body reviews rather than in the drafting room.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% The Court of Arbitration for Sport, the Swiss Federal Court, and the European Court of Human Rights review the eligibility rules and the treatment of affected athletes. CAS upheld the regulations on fairness grounds; the ECHR found in 2023 that Semenya's treatment engaged her rights. They shape what the rules may lawfully do without administering them day to day.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, human_rights_courts, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, sports_governing_bodies).
narrative_ontology:fixing_cost_class(woman_category__intersex_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, administrable criterion for who counts as 'woman' across law, records, facilities, and sporting classes, so that sex-segregated provisions and competitive categories operate without case-by-case renegotiation; the intersex accommodation keeps the criterion biological while preventing the most visible misclassifications of people whose bodies fit neither standard category.
% TRANSFER_FUNCTION: Moves classification authority and category control to the institutions that administer the criterion — sports federations, medical panels, registries — and moves the costs of boundary maintenance (testing, medical supervision, medication, exclusion from category and record) onto the small population whose biology sits at the boundary, while denying membership to trans women whose bodies the accommodation does not reach.
% ABSENT_VOICES: Intersex-led organizations and affected athletes have no formal seat in federation congresses or eligibility-panel drafting; trans women are classified by rules they had no part in writing; intersex infants are assigned a category before any voice exists. The appearance of stakeholder consensus around eligibility rules is produced partly by keeping these seats out of the room.
% DISAPPEARANCE_RATIONALE: Remove the criterion overnight and every sex-segregated provision, record system, and women's competitive class must immediately re-decide membership: records freeze, eligibility panels dissolve, facility allocation falls to ad hoc dispute. The rearrangement is contested because the three readings disagree on what replaces it — but nothing continues as-is.
% FOUNDING_PROBLEM: Binary biological classification misclassified people with intersex variations — assigning them against their bodies or subjecting them to verification to prove membership. This reading was built to solve that misclassification while keeping a biological anchor for the category, after chromosome-screening regimes collapsed under exactly these cases.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: intersex-led organizations (including those filing briefs in the Semenya litigation and UN treaty-body submissions) attest both the misclassification problem and the claim that this reading only partly solves it; independent sports-science reviews attest the boundary problem the accommodation answers. The benefiting federations' attestation that the problem is 'managed' is the minority voice.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55 is a blended value over the standing arrangement: near-zero in civil domains where the accommodation simply recognizes, and substantially higher at the sport edge where eligibility machinery imposes testing, medication, and career costs on a handful of athletes; the blend weights the sport edge heavily because that is where the arrangement actively enforces. Suppression 0.45 is authored raw and unscaled: athletes cannot decline testing and keep racing, and there is no compliant path that recognizes trans women — yet civil-domain participants face no coercion at all. Theater 0.36: the fairness-of-the-female-category rationale is partly real (competitive classes do need lines) and partly performative, with protection rhetoric intensifying as the underlying biology proved less binary than the rules pretend. Accessibility_collapse 0.30: the sibling readings remain fully live alternatives — this is a contested kernel, not a settled natural order. Resistance 0.62: sustained litigation (CAS, Swiss courts, ECHR), UN-level criticism, and intersex-led campaigning meet the arrangement continuously. Coordination type is identity_coordination (boundary maintenance and membership claims against evolving criteria) at the default floor; note the FNL gaming caution — the identity framing is genuine at the civil layer, but the sport edge couples institutional power with global scope to concentrate burden on powerless agents, which the complexity offset does not excuse. All three measurement series share one time grid (t=0..35, approximately 1990-2025) so no metric borrows another's endpoints; the slight end-of-interval declines reflect legal pushback (ECHR 2023) and open-category experimentation, not stabilization.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the federation seat the arrangement is legitimate administration of a necessary line — experienced as stewardship. From the DSD athlete seat the same machinery is intimate coercion: bodily testing, unwanted medication, public anatomical debate, with no exit that preserves career or self. Intersex individuals outside sport experience the reading as recognition; trans women experience its boundary as erasure. Courts sit analytically and split: CAS upholds the regulations on fairness grounds while the ECHR finds rights violations. Identity-lock does real work on both payer seats — the athlete's career-self and the trans woman's gendered self make exit equivalent to self-renunciation — which is why their computed extraction sits nearer the full-target end than their formal options suggest. The engine computes these per-seat classifications from the structural data; the divergence is the finding, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: sports_governing_bodies collect category control and legitimacy (d near the beneficiary end); female_category_athletes receive the protected field, and their mobile exit pushes them further toward subsidy; intersex_individuals gain civil recognition, with their sport-edge exposure carried by the secondary payer role rather than by distorting their derived d. Targets: dsd_elite_athletes bear the transfer in full and are locked by career identity (d near 1.0); trans_women_denied_membership bear exclusion under identity_lock. Clinical classification authorities co-administer and profit professionally, giving them a mild beneficiary tilt. Suppression is a raw structural property and is not scaled; the engine scales only extractiveness, by directionality and scope — and the global scope of the sport regime amplifies effective extraction on the athlete seat by making verification and appeal harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binary classification misclassifying intersex people — is live: this reading genuinely addresses it in civil domains, so declaring the mandate dead would misread the arrangement as pure vestige. But the sport edge shows mandate extension: machinery built to classify fairly now maintains category purity against the very spectrum evidence the reading officially acknowledges, and enforcement hardened (chromosome screening, then case-by-case assessment, then numeric testosterone ceilings) as the science blurred. Calling the whole arrangement pure extraction would erase the real civil coordination; calling it pure coordination would erase the athletes paying through it. The tangled_rope claim holds both truths. Mandatrophy is not resolved: the mandate persists, but its center of gravity has migrated from solving misclassification to defending the boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the woman_category kernel. What would each sibling reading change structurally if it governed instead?',
    'Cross-reading comparison across the three family files: hold the policy domain fixed, swap the governing reading, and track which populations enter or leave the victim set.',
    'Under the strict biology reading the boundary-case victim set grows (more bodies fail the typical-case test); under the gender-identity reading the victim set relocates entirely (trans women included, women objecting to identity-based access become the contesting seat). This file''s epsilon is valid only for the accommodation reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings; victim set and epsilon are reading-indexed.').

omega_variable(
    sport_edge_constraint_individuation,
    'Is the sport-edge eligibility machinery part of this constraint, or a separate constraint that merely rides on the category rule?',
    'Decomposition test: if the DSD regulations could be repealed while the civil category rule stands unchanged, and the two arrangements have independent enforcement budgets and victim sets, author a separate sport-eligibility story and link it via network.affects_constraints.',
    'As authored, epsilon blends a near-zero civil regime with a high-extraction sport regime (roughly 0.15 versus 0.75 if split); a split would give each component a cleaner epsilon and sharpen per-seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sport_edge_constraint_individuation, conceptual, 'Whether the domain-heterogeneous epsilon indicates one constraint or a constraint family.').

omega_variable(
    category_boundary_naturalness,
    'Is the woman-category boundary a discovered natural-kind line that this reading approximates, or a constructed allocation device that any line through a spectrum partly invents?',
    'Compare classification stability across independent cultures, historical periods, and the biological literature itself: convergent boundaries indicate discovery; divergent, interest-tracking boundaries indicate construction.',
    'If constructed, the sport-edge line-drawing is policy choice with identifiable payers and the extraction reading strengthens; if discovered, part of the enforcement burden is the irreducible cost of any classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_boundary_naturalness, conceptual, 'Natural-kind versus constructed status of the category boundary.').

omega_variable(
    testosterone_performance_causality,
    'Does endogenous testosterone in the male typical range cause a performance advantage large enough to justify eligibility lines drawn through DSD athletes'' bodies?',
    'Independent replication of the sports-science literature away from federation-commissioned studies, plus natural experiments from competitions where affected athletes raced unrestricted.',
    'A weak causal link strips the fairness rationale from the sport edge, pushing its extraction toward pure exclusion and raising the blended epsilon; a strong link supports the coordination reading of the eligibility machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testosterone_performance_causality, empirical, 'Empirical warrant for the sport-edge eligibility line.').

omega_variable(
    trans_exclusion_cost_attribution,
    'Are the costs borne by trans women under this reading attributable to this reading specifically, or to the biological-basis family of readings generally?',
    'Counterfactual comparison against the strict biology reading: if the exclusion costs are identical under both, they are family-level structure rather than reading-specific delta.',
    'Reading-specific epsilon would fall slightly (the delta over the strict reading is the accommodation, which shrinks the victim set); family-level accounting would instead book the costs to the shared biological-basis structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_exclusion_cost_attribution, conceptual, 'Attribution of the trans-exclusion cost between this reading and its parent family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t7, woman_category__intersex_accommodation_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement_basis(woma_tr_t7, observed).
narrative_ontology:measurement(woma_tr_t14, woman_category__intersex_accommodation_reading, theater_ratio, 14, 0.24).
narrative_ontology:measurement_basis(woma_tr_t14, observed).
narrative_ontology:measurement(woma_tr_t21, woman_category__intersex_accommodation_reading, theater_ratio, 21, 0.33).
narrative_ontology:measurement_basis(woma_tr_t21, observed).
narrative_ontology:measurement(woma_tr_t28, woman_category__intersex_accommodation_reading, theater_ratio, 28, 0.38).
narrative_ontology:measurement_basis(woma_tr_t28, observed).
narrative_ontology:measurement(woma_tr_t35, woman_category__intersex_accommodation_reading, theater_ratio, 35, 0.36).
narrative_ontology:measurement_basis(woma_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t7, woman_category__intersex_accommodation_reading, base_extractiveness, 7, 0.22).
narrative_ontology:measurement_basis(woma_be_t7, observed).
narrative_ontology:measurement(woma_be_t14, woman_category__intersex_accommodation_reading, base_extractiveness, 14, 0.31).
narrative_ontology:measurement_basis(woma_be_t14, observed).
narrative_ontology:measurement(woma_be_t21, woman_category__intersex_accommodation_reading, base_extractiveness, 21, 0.47).
narrative_ontology:measurement_basis(woma_be_t21, observed).
narrative_ontology:measurement(woma_be_t28, woman_category__intersex_accommodation_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement_basis(woma_be_t28, observed).
narrative_ontology:measurement(woma_be_t35, woman_category__intersex_accommodation_reading, base_extractiveness, 35, 0.55).
narrative_ontology:measurement_basis(woma_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t7, woman_category__intersex_accommodation_reading, suppression_requirement, 7, 0.22).
narrative_ontology:measurement_basis(woma_su_t7, observed).
narrative_ontology:measurement(woma_su_t14, woman_category__intersex_accommodation_reading, suppression_requirement, 14, 0.3).
narrative_ontology:measurement_basis(woma_su_t14, observed).
narrative_ontology:measurement(woma_su_t21, woman_category__intersex_accommodation_reading, suppression_requirement, 21, 0.42).
narrative_ontology:measurement_basis(woma_su_t21, observed).
narrative_ontology:measurement(woma_su_t28, woman_category__intersex_accommodation_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement_basis(woma_su_t28, observed).
narrative_ontology:measurement(woma_su_t35, woman_category__intersex_accommodation_reading, suppression_requirement, 35, 0.45).
narrative_ontology:measurement_basis(woma_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% Family member of the woman_category kernel decomposition. The colloquial label 'what makes someone a woman' conflates three structurally distinct membership rules with different victim sets and epsilon values: strict typical-case biology (boundary cases fail verification), this accommodation reading (boundary cases included in civil domains, extracted from at the sport edge), and gender identity (victim set relocates to identity contestation). Each is authored separately with its own epsilon per the epsilon-invariance principle. Edges here express that this reading's spectrum acknowledgment structurally pressures the strict reading's enforcement (which historically mutated from chromosome screening to DSD frameworks under exactly this pressure) while coexisting with the identity reading as rival live positions held by different factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
