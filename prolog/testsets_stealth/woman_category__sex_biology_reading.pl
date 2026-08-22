% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Biological-Sex Membership Criterion for the Category 'Woman' (Sex-Biology Reading)
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'woman_category': the sex-biology reading, under which membership in the
 *   category 'woman' is fixed by chromosomal, anatomical, and reproductive
 *   biology (typical case: XX chromosomes and female reproductive anatomy),
 *   and that membership gates sex-segregated sport, facilities, data
 *   collection, and protective provisions. The standing arrangement under
 *   contest — and therefore the ε referent — is this biological-membership
 *   rule as it actually operates in law, sport, and policy, assessed by the
 *   reading's own lights: the reading credits the rule's protective and
 *   administrable function while conceding real collateral burdens (intersex
 *   verification, transgender men's forced inclusion, enforcement
 *   humiliations). Sibling readings (gender_identity_reading,
 *   intersex_accommodation_reading) are separate constraints with their own ε
 *   and victim sets; they are not averaged into this file. The claim/metric
 *   gap is deliberate: claimed_type is my independent structural judgment
 *   (tangled_rope — genuine coordination plus asymmetric extraction plus
 *   active enforcement), and the metrics are my independent descriptive
 *   estimates; the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computation is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - biological_women: primary beneficiary (organized/identity_locked) — receive the protected-category integrity the rule maintains
 *   - - transgender_women: primary target (moderate/trapped) — bear exclusion from sex-segregated protections with no biological route into the category
 *   - - intersec_people_placeholder_removed
 *   - - intersex_people: target with ambiguous membership (powerless/trapped) — bear verification burdens and case-by-case adjudication
 *   - - transgender_men: collaterally targeted (moderate/trapped) — swept into the women's category against their identity
 *   - - sports_governing_bodies: agenda setter (institutional/arbitrage) — define and enforce eligibility criteria, gain administrable classes
 *   - - legislators_and_courts: agenda setter (institutional/mobile) — codify and adjudicate the definition nationally
 *   - - human_rights_monitors: analytical observer (institutional/analytical) — audit the rule's operation against rights frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.6).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.66).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Biological-Sex Membership Criterion for the Category 'Woman' (Sex-Biology Reading)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, '6ad4541e-b3d9-489b-a84d-687f2a3a7e08').
narrative_ontology:cs_kernel_codification('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', formalized).
narrative_ontology:cs_authority_grounding('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', expertise).
narrative_ontology:cs_interpretation_layer_present('6ad4541e-b3d9-489b-a84d-687f2a3a7e08').
narrative_ontology:cs_reading_relation('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', foundational, membership_determined_by_biological_sex).
narrative_ontology:cs_axiom_status(membership_determined_by_biological_sex, holdable).
narrative_ontology:cs_axiom_grounding('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', membership_determined_by_biological_sex, empirically_contingent).
narrative_ontology:cs_axiom('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', secondary, sex_segregated_provision_requires_stable_biological_boundary).
narrative_ontology:cs_axiom_status(sex_segregated_provision_requires_stable_biological_boundary, holdable).
narrative_ontology:cs_axiom_grounding('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', sex_segregated_provision_requires_stable_biological_boundary, instrumental).
narrative_ontology:cs_reference_frame('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', binary_sexual_dimorphism_typology).
narrative_ontology:cs_drift_state('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', contemporary_multijurisdictional_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6ad4541e-b3d9-489b-a84d-687f2a3a7e08', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, biological_women).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sports_governing_bodies).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with female chromosomal and anatomical development who rely on the category for sex-segregated refuges, prisons, changing rooms, sports classes, medical research cohorts, and discrimination protections keyed to female biology. They cannot opt out of being categorized, but the categorization currently delivers provisions they campaigned to establish. Leaving the category is neither possible nor sought; what varies by jurisdiction is whether the boundary stays where they understand it.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, biological_women, beneficiary,
    organized, generational, identity_locked, global).

% Women under the rival identity reading who live and are recognized as women in much of public life but are excluded from the sex-segregated provisions this reading reserves for female biology. They carry documentation mismatches, face eligibility bars in licensed sport, and in some jurisdictions lose access to single-sex facilities aligned with their lives. There is no biological route into the category; relocating to a jurisdiction applying the rival reading is the only exit, and it is costly and incomplete.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    moderate, biographical, trapped, global).

% People whose chromosomal, gonadal, or anatomical development does not fit the typical binary pattern. The rule assigns them case by case: some variations pass unremarked, others trigger testing panels, medical-history disclosure, or retrospective disqualification. Historically they bore nonconsensual infant assignments and athletic sex verification; today they face committee adjudication of which side of the line they fall on. Small population size limits their political leverage, and classification decisions have historically been made about them rather than with them.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, intersex_people, excluded).

% Men under the identity reading who retain female biology and are therefore pulled into the women's category by this rule — into women's prisons, shelters, and sports classes — contrary to their lived identity. Their presence then feeds enforcement scrutiny of the category's borders. Exit mirrors that of transgender women: jurisdictional relocation or accepting misclassification.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_men, payer,
    moderate, biographical, trapped, global).

% International federations and eligibility panels that operationalize the criterion: they set testing thresholds, adjudicate differences of sexual development cases, and disqualify athletes. They gain administrable competition classes and defensible rulebooks, and they absorb litigation, athlete-welfare controversy, and federation-splitting disputes when the criterion is challenged. They can and do adjust thresholds and shift standards between events and jurisdictions.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, sports_governing_bodies, beneficiary).

% Statutory drafters and judges who fix the definition's legal force: defining 'woman' in equalities law, ruling on eligibility challenges, and setting documentation rules. They gain clear administrable lines to administer and electoral positioning, and they bear the docket load and precedent instability of a definition contested across jurisdictions. Any single legislature can amend its definition, at the price of domestic contestation.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, legislators_and_courts, agenda_setter,
    institutional, generational, mobile, national).

% Treaty bodies, special rapporteurs, and bioethics commissions that audit the rule's operation against rights frameworks. They take testimony from every seat, publish findings on the treatment of intersex and transgender people, and can trigger review procedures, but they enforce nothing directly.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, human_rights_monitors, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, biological_women).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an externally verifiable, identity-independent criterion for sorting people into sex-segregated categories — competitive classes, single-sex facilities, medical research cohorts, protective statistics — so that allocation does not depend on adjudicating anyone's internal self-understanding.
% TRANSFER_FUNCTION: Moves access to sex-segregated protections, competitive eligibility, and categorical recognition toward people with female biology, and moves the costs of exclusion, verification, and misclassification onto transgender and intersex people.
% ABSENT_VOICES: Intersex people were classified about rather than consulted for most of the interval — sex-verification protocols and infant assignment practices were designed by medical and sporting authorities without intersex representation, and intersex advocacy gained standing only late. In several jurisdictions, definitional legislation has passed without transgender testimony. Both groups sit outside the rooms where the criterion is fixed.
% DISAPPEARANCE_RATIONALE: If the biological-membership rule vanished overnight, every sex-segregated provision would need re-founding on some other criterion within weeks: federations would need new eligibility bases, statistical agencies new cohort definitions, refuges and prisons new admission rules. Nothing about the underlying needs disappears; the entire allocation superstructure would have to be rebuilt — rearrangement, not stasis.
% FOUNDING_PROBLEM: Law and policy needed a determinate boundary for the category 'woman': who may compete in women's sport, who is counted in sex-based health data, who may access women's refuges, prisons, and changing rooms, who is covered by sex-discrimination protections. Chromosomal and anatomical biology offered an administrable, externally verifiable answer.
% FOUNDING_PROBLEM_CORROBORATION: Sports-science literature on sex-linked performance differences and clinical endocrinology materials corroborate that the boundary problem is live; court records from eligibility litigation on both sides attest it; UN treaty-body and special-rapporteur reporting corroborates the harm-side accounting from outside the benefiting parties. No party disputes that some boundary question is live; the parties dispute which criterion answers it.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.60: the rule imposes exclusion, verification, and misclassification costs on transgender and intersex people that are decoupled from anything rendered to them, while delivering genuine protective and administrable value to female-biology women and federations — a hybrid profile, not a pure one. Suppression is 0.66 and is authored as a raw structural property (the engine scales only extractiveness): eligibility testing, statutory definitions, documentation requirements, and facility rules are actively enforced, but alternatives are not maximally suppressed because rival readings govern in other jurisdictions. Theater ratio is 0.35: the bulk of activity is functional category assignment, but symbolic definitional politics consumes a growing share of enforcement attention. Accessibility collapse is low (0.30) because the rival readings remain live and operable — this is a contested construct, not a natural law, hence emerges_naturally is false: sexual dimorphism is the biological regularity; the membership RULE built on it is an administrative choice. Resistance is high (0.72): litigation, protest, federation splits, and cross-jurisdictional divergence are constant. The three measurement series share one time grid (t=0..60, decade steps) so no metric row is sampled against another's scalar substitute. The suppression series is deliberately non-monotonic: it peaks with the mass chromosome-testing era (t≈10), declines as mass testing was suspended in favor of individualized case management (t≈20-30), then ratchets back up with DSD regulations and statutory definitional acts (t≈40-60). Coalition check: intersex people's small population caps coalition power despite powerlessness; transgender advocacy coalitions have grown but remain jurisdictionally fragmented, which is why trapped exit persists for all three payer seats.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the beneficiary seat, the rule is the load-bearing wall of hard-won sex-based provisions; from the trapped payer seats, the same wall is the thing keeping them out. Same-level divergence is pronounced: biological_women and transgender_women hold comparable nominal social standing (organized/moderate) yet sit at opposite ends of the directionality range, differentiated by role and exit rather than global power. Inter-institutional divergence: sports_governing_bodies and legislators_and_courts are both institutional agenda setters, but the federations hold arbitrage-grade exit (thresholds adjustable per event, standards shiftable across jurisdictions) while legislatures are bound to national electorates (mobile at best) — so the same definition bites differently in each hall. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   biological_women are declared beneficiaries: the rule subsidizes them with protected-category integrity, so their derived d sits near the beneficiary end and effective extraction inverts toward subsidy. sports_governing_bodies are agenda setters with a declared secondary beneficiary position: administrable competition classes flow to them, damped d, partially offset by litigation costs they bear. transgender_women, intersex_people, and transgender_men are declared victims with trapped exit: no arbitrage, no jurisdictional escape that fully releases them, so their derived d sits near the full-target end — intersex_people worst positioned, bearing both exclusion-risk and verification burden under ambiguous membership. legislators_and_courts take diffuse positional value (electoral, administrable) with no concentrated receipt. The victim declarations are the structural input; the amplification arithmetic belongs to the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a determinate boundary for law and policy — is live, so no mandatrophy is declared; the contest is over the criterion, not over obsolescence, and the (founding_problem_status=live x disappearance_verdict=world_rearranges) cell raises no zombie flag. The tangled_rope classification is what prevents both symmetric mislabelings: calling this a mountain (a natural fact) would erase the victims and the enforcement machinery and launder a constructed administrative choice as inevitability; calling it a snare would erase the genuine coordination function — sports categories, epidemiological cohorts, and protective provisions solved real allocation problems before and independent of the current contest. The hybrid type keeps both halves visible and forces the analysis onto the actual fault line: whether the protective warrant justifies the extraction profile seat by seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of the woman_category kernel (sex_biology_reading). Would the sibling readings — gender_identity_reading and intersex_accommodation_reading — instantiate different constraints with different victim sets and different epsilon over the same referent?',
    'Author and classify each sibling as its own story, then compare victim sets, epsilon, and computed types across the family; the disagreement is located in the determinant of membership (biology vs. identity vs. spectrum-accommodated biology).',
    'Under the gender_identity_reading the victim set relocates to people denied identity-consistent recognition and the beneficiary set expands; under the intersex_accommodation_reading the edge-case extraction borne by intersex people shrinks sharply. Cross-reading comparison is the corpus''s indexical yield; averaging the readings into one story would destroy it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame record: this story is the sex-biology reading of the woman_category kernel; siblings are separate constraints.').

omega_variable(
    domain_application_decomposition,
    'Does the category rule carry one stable epsilon across its application domains, or do sports eligibility and violence-against-women data collection constitute separable constraints with materially different epsilon?',
    'Measure epsilon per application domain; if the sports-eligibility observable yields a decisively higher value than the data-collection observable, author separate stories per domain and link them via network edges rather than averaging.',
    'If separable, this unified story smooths a hot spot (sport, where performance-advantage stakes concentrate extraction) against a cooler one (cohort statistics), which can date type transitions incorrectly and hide the sharpest victim seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_application_decomposition, conceptual, 'Whether the membership rule is one constraint or a family of domain-specific constraints sharing a criterion.').

omega_variable(
    intersex_membership_instability,
    'Is intersex people''s ambiguous membership a stable edge-case-adjudication design, or an unstable boundary that shifts case by case and panel by panel?',
    'Longitudinal review of eligibility rulings, testing protocols, and retrospective disqualifications across federations and decades.',
    'Rising instability increases the extraction borne by the intersex seat and pushes that seat''s computed type toward pure extraction; stable principled adjudication supports the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_membership_instability, empirical, 'Stability of the rule''s handling of the population it fits least well.').

omega_variable(
    natural_kind_vs_administrative_construct,
    'Is the binary category a natural kind that the rule discovers, or an administrative construct riding on partial biological regularities?',
    'Comparative analysis of how the rule handles intersex and edge cases: discovery predicts principled, criterion-driven handling; construction predicts convenience-driven, context-varying handling.',
    'If construct, the rule''s alternatives were always available and the measured suppression carries more classification weight; if natural kind, the measured costs read closer to an irreducible coordination price and the mountain framing gains credibility despite the victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_kind_vs_administrative_construct, conceptual, 'Naturality of the binary category versus constructedness of the membership rule.').

omega_variable(
    protective_warrant_evidence,
    'Does excluding transgender women from sex-segregated protections actually deliver the protective benefit for female-biology women that the rule claims as its warrant?',
    'Incident and safety outcome data from jurisdictions applying each reading, matched for demographics and provision type.',
    'If the warrant fails, the coordination half of the arrangement weakens and the structure slides toward pure extraction; if it holds, the hybrid classification stabilizes and the debate reduces to cost allocation at the margins.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_warrant_evidence, empirical, 'Empirical status of the protective warrant that licenses the exclusion costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sexbio_reading_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sexbio_reading_tr_t10, woman_category__sex_biology_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(sexbio_reading_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(sexbio_reading_tr_t30, woman_category__sex_biology_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(sexbio_reading_tr_t40, woman_category__sex_biology_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(sexbio_reading_tr_t50, woman_category__sex_biology_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement(sexbio_reading_tr_t60, woman_category__sex_biology_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(sexbio_reading_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sexbio_reading_be_t10, woman_category__sex_biology_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(sexbio_reading_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(sexbio_reading_be_t30, woman_category__sex_biology_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(sexbio_reading_be_t40, woman_category__sex_biology_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(sexbio_reading_be_t50, woman_category__sex_biology_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(sexbio_reading_be_t60, woman_category__sex_biology_reading, base_extractiveness, 60, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(sexbio_reading_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(sexbio_reading_su_t10, woman_category__sex_biology_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(sexbio_reading_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(sexbio_reading_su_t30, woman_category__sex_biology_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(sexbio_reading_su_t40, woman_category__sex_biology_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(sexbio_reading_su_t50, woman_category__sex_biology_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(sexbio_reading_su_t60, woman_category__sex_biology_reading, suppression_requirement, 60, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what is a woman' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the woman_category kernel. They share a referent domain but differ in determinant, victim set, and epsilon: the biology reading (this file) fixes membership by chromosomal/anatomical/reproductive biology; the identity reading fixes it by internal gender identity; the accommodation reading keeps biology as determinant but handles it as a spectrum with inclusive edge treatment. This story is upstream historically (older codifications) and its statutory force shapes the accommodation reading's operating environment (influences), while standing in direct logical contradiction to the identity reading (forecloses). Each member links to the others; no orphan stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
