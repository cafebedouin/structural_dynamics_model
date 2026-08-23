% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Biology Determination of Female Category Membership (sex_biology_reading)
 *   domain: political_philosophy/bioethics/law
 *
 * SUMMARY:
 *   A widespread legal and administrative arrangement resolves membership in
 *   the female/woman category by developmental biology — chromosomal
 *   constitution, reproductive anatomy, gamete-production lineage — and on
 *   that footing reserves female-designated provision (prisons, refuges,
 *   wards, changing rooms, sporting categories) for natal females, excluding
 *   trans women from those provisions. Eligibility enforcement concentrates
 *   in elite sport and custodial intake, the two sites with the least
 *   participant mobility. KEY AGENTS (by structural relationship): -
 *   natal_females_seeking_protections: Primary beneficiary
 *   (organized/constrained) - female_prisoners: Trapped beneficiary
 *   (powerless/trapped) - elite_female_athletes: Dual-positioned
 *   payer/beneficiary (moderate/constrained) -
 *   transgender_women_denied_access: Primary target (organized/constrained) -
 *   incarcerated_trans_women: Trapped target (powerless/trapped) -
 *   intersex_dsd_individuals: Misclassified target (powerless/trapped) -
 *   gender_nonconforming_cis_women: Spillover payer (moderate/constrained) -
 *   sports_governing_bodies: Agenda setter (institutional/arbitrage) -
 *   correctional_and_shelter_authorities: Agenda setter
 *   (institutional/constrained) - clinical_classification_bodies and
 *   human_rights_treaty_bodies: Analytical observers. This file is ONE
 *   reading — sex_biology_reading — of the contested kernel
 *   woman_female_category; the gender_identity_reading and
 *   hybrid_contextual_reading instantiate DIFFERENT constraints in their own
 *   files, linked only through network edges and omega variables. The epsilon
 *   referent is this reading's own standing arrangement (biological
 *   determination as operative), assessed by this reading's lights: the
 *   protective core is treated as functional, and the authored cost-bearing
 *   reflects its documented asymmetric burdens. The manifest's expected delta
 *   is reconciled thus: gender_nonconforming_cis_women — biological females —
 *   enter the victim set through enforcement spillover and test failure;
 *   female prisoners appear as the protected seat, whose safety exposure
 *   under the rival self-ID arrangement belongs to the sibling file's ledger,
 *   not this one. ASSUMPTION STATED: the manifest clause pairing a
 *   biological-female victim set with high safety-domain cost-bearing is
 *   ambiguous between this reading's operation and its rival's; the ambiguity
 *   is routed to the omega carceral_safety_extractiveness_referent rather
 *   than folded into the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.6).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.55).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Biology Determination of Female Category Membership (sex_biology_reading)").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, 'cb5d2182-e558-4a40-83a2-f30239844d27').
narrative_ontology:cs_kernel_codification('cb5d2182-e558-4a40-83a2-f30239844d27', distributed).
narrative_ontology:cs_authority_grounding('cb5d2182-e558-4a40-83a2-f30239844d27', distributed).
narrative_ontology:cs_reading_relation('cb5d2182-e558-4a40-83a2-f30239844d27', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('cb5d2182-e558-4a40-83a2-f30239844d27', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('cb5d2182-e558-4a40-83a2-f30239844d27', foundational, category_membership_tracks_developmental_biology).
narrative_ontology:cs_axiom_status(category_membership_tracks_developmental_biology, holdable).
narrative_ontology:cs_axiom_grounding('cb5d2182-e558-4a40-83a2-f30239844d27', category_membership_tracks_developmental_biology, empirically_contingent).
narrative_ontology:cs_axiom('cb5d2182-e558-4a40-83a2-f30239844d27', secondary, protective_boundaries_require_bodily_criterion).
narrative_ontology:cs_axiom_status(protective_boundaries_require_bodily_criterion, holdable).
narrative_ontology:cs_axiom_grounding('cb5d2182-e558-4a40-83a2-f30239844d27', protective_boundaries_require_bodily_criterion, instrumental).
narrative_ontology:cs_reference_frame('cb5d2182-e558-4a40-83a2-f30239844d27', uniform_biological_sex_classification).
narrative_ontology:cs_drift_state('cb5d2182-e558-4a40-83a2-f30239844d27', contemporary_self_identification_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cb5d2182-e558-4a40-83a2-f30239844d27', '2026-08-05T09:30:00Z').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_protections).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, female_prisoners).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, elite_female_athletes).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, transgender_women_denied_access).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, incarcerated_trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_dsd_individuals).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, elite_female_athletes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women and girls who rely on refuges, hospital wards, changing rooms, and sporting categories being reserved by biological sex. What flows to them is a predictable assurance that female-designated provision is occupied by people whose bodies went through female development. When a provider broadens admission rules, their recourse is lobbying, funding alternative services, or relocating; most cannot opt out of the surrounding legal framework.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_protections, beneficiary,
    organized, generational, constrained, global).

% Women serving custodial sentences are searched, housed, and showered under intake rules keyed to biological sex. They cannot shop for a different prison system and hold no individual leverage over placement policy; their protection depends entirely on which criterion their national service applies. Advocacy reaches them only indirectly, through parliamentarians and inspectors.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, female_prisoners, beneficiary,
    powerless, biographical, trapped, national).

% Compete inside a female category whose limits are policed by eligibility testing. Some must periodically submit to blood draws, genital examinations, or chromosome and hormone verification to keep competing; those who fail a test lose ranking, prize money, and career continuity regardless of how they have lived. The same category that tests them is the one that makes their podium attainable.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, elite_female_athletes, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, elite_female_athletes, beneficiary).

% Trans women living as women meet doors marked for females that close under biological criteria: refuge admission refused, changing rooms barred or humiliating, documents retained at birth sex in many jurisdictions. Individual responses run from private avoidance to litigation; organized responses run through equality bodies and party politics. Moving between jurisdictions changes what they face but not the criterion itself.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, transgender_women_denied_access, payer,
    organized, biographical, constrained, global).

% Trans women in custody are placed by the criterion their prison service applies; where that is birth sex, they serve sentences in men's facilities, with documented elevated risk of assault and interrupted gender-affirming care. Segregation units are the usual partial remedy and carry isolation costs of their own. They cannot exit the institution; impact litigation and inspectorate reports are their main channels.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, incarcerated_trans_women, payer,
    powerless, biographical, trapped, national).

% People whose chromosomes, gonads, hormones, or anatomy do not align with the standard XX/XY script — for instance women with complete androgen insensitivity who have XY chromosomes and have lived entirely as female. Fixed biological tests misclassify them at exactly the moments the category matters most: birth registration, sporting eligibility, custodial intake. Some were operated on as infants to fit the script. Their exit options are effectively nil; the classification travels in their records.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, intersex_dsd_individuals, payer,
    powerless, biographical, trapped, global).

% Masculine-presenting women who are not transgender absorb the spillover of boundary enforcement: challenged in restrooms, flagged by appearance-based checks, asked to prove what their documents already show. They also hold the protection the boundary provides. Their exposure rises wherever enforcement leans on appearance rather than paperwork.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, gender_nonconforming_cis_women, beneficiary).

% Federations such as World Athletics and World Boxing write and revise eligibility rules for the female category, commission the science behind thresholds, and defend them before the Court of Arbitration for Sport. Administering the boundary yields rulemaking authority and a commercially valuable guarantee that female titles track comparable physiology; thresholds are revised after athlete controversies, and litigation costs are absorbed centrally.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Prison services and refuge commissioners set intake classification and admission policy under shifting ministerial guidance. Each reversal after a high-profile incident arrives as operational disruption: staff retraining, unit conversion, legal review. They prefer administrable bright-line rules and inherit whichever criterion the legislature last chose.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, correctional_and_shelter_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Endocrinologists, pediatricians, and forensic physicians operationalize sex determination in medicine: newborn assignment, DSD multidisciplinary teams, doping-control oversight. They publish consensus statements, treat the patients the criteria misclassify, and increasingly argue for parameter-specific language — gonadal tissue, hormone profiles — over categorical labels in clinical settings.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, clinical_classification_bodies, observer,
    institutional, generational, analytical, global).

% Council of Europe bodies, UN special procedures, and national equality commissions audit the arrangement against anti-discrimination and dignity commitments. Their recommendations cut both ways: defending single-sex provision as lawful under specified circumstances while condemning blanket exclusions and non-consensual infant interventions.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, human_rights_treaty_bodies, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates access to single-sex provision — refuges, custodial housing, wards, changing rooms, sporting categories, sex-specific medical protocols — around stable biological boundaries, so safety, privacy, fair competition, and dosing do not require case-by-case renegotiation.
% TRANSFER_FUNCTION: Moves access and recognition: confers female-category access and protection on biological females; withholds them from those not biologically female, notably trans women; and concentrates verification costs on athletes and atypically developed individuals.
% ABSENT_VOICES: Intersex and DSD individuals were largely absent when chromosomal and gamete-tier criteria were operationalized; their voice entered mainly through litigation after disqualification. Incarcerated trans women sit on no placement-policy board. Women disqualified by historic sex tests spoke publicly mostly after their careers had ended.
% DISAPPEARANCE_RATIONALE: Overnight removal forces immediate re-derivation of sporting eligibility, custodial placement, refuge admission, and document sex; medicine would accelerate its shift to parameter-specific language; the category system would reorganize around either self-identification or contextual rules — which one is precisely what the sibling files contest.
% FOUNDING_PROBLEM: Male-pattern violence and male physiological advantage made unprotected mixing costly for females: refuges, prisons, sanitation, and competitive sport required a boundary that did not depend on negotiation or self-report, and biological sex supplied an administrable criterion that is hard to forge.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: custodial-population offense statistics published by justice ministries, sports-science literature on performance differentials, and WHO violence data — sources that collect no rent from the category rule. The political salience of the problem is disputed; its empirical basis is independently attested.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.60 at interval end) is substantial but bounded: the protective function is real and widely used, while the costs — exclusion from female-designated provision, misclassification of atypical development, compulsory verification — fall on identifiable minorities, several of them trapped. Suppression (0.55) reflects a matured enforcement apparatus: statutory eligibility bars, mandatory testing regimes, custodial placement rules, and criminalized facility misuse in some jurisdictions. Theater (0.28) captures the growth of symbolic enforcement — certificate rituals, signage politics — atop a still-functional core; the ratio rises over the interval as litigation substitutes for administration. Accessibility_collapse (0.35) is low because alternatives demonstrably survive: inclusive parallel providers, third spaces, jurisdictional variation, and the contextual rule embodied by the sibling hybrid reading. Resistance (0.72) is among the highest of contemporary constructs: sustained litigation (CAS, constitutional courts), mass political mobilization, and academic contestation on every front. Suppression is authored as a raw structural property and is NOT scaled by power or scope; extractiveness is scaled by the engine from directionality and spatial scope — hence the same base epsilon yields much higher effective extraction in trapped carceral seats than in mobile civic ones. All three series share one time grid (2000–2024, eight points); the trajectories are monotone net-ratchet representations that deliberately smooth a pendulum (reform wave, incident, retrenchment) whose phase-dependence is documented in the omega policy_pendulum_phase. Receipt surface: every named seat was checked for capture — federations gain authority but under CAS oversight and recurring adverse rulings; correctional authorities gain administrative simplicity, not rents; no seat demonstrably concentrates the arrangement's gains, so 'diffuse' is asserted affirmatively. Fixing cost is prohibitive: wholesale replacement of the criterion is entangled with constitutional documents, treaty obligations, registration infrastructure, and internationally divergent case law, costing any single administrator far more than it bears.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the trapped beneficiary seats (female_prisoners), the arrangement is the thing that stands between them and mixed-sex housing — near-pure coordination. From the trapped payer seats (incarcerated_trans_women), the same intake rule is the mechanism of their exposure — extraction amplified by zero exit. From elite_female_athletes the arrangement pays them a podium with one hand and administers testing with the other. The agenda setters experience administrative necessity and reputational hazard; the observers see a constructed boundary operating on a natural substrate. The engine computes these per-seat classifications from power, exit, and declared position; the authored claimed_type adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (natal_females_seeking_protections, female_prisoners) sit near the subsidized end: the arrangement delivers protection they cannot purchase elsewhere, and their exit is constrained or nil. Declared payers sit near the target end: transgender_women_denied_access and intersex_dsd_individuals bear the transfer and the misclassification cost respectively, and trapped seats (incarcerated_trans_women, intersex_dsd_individuals) amplify effective extraction because no arbitrage exists. elite_female_athletes and gender_nonconforming_cis_women are genuinely dual-positioned — payer and beneficiary in the same body — which is why they carry secondary roles rather than overrides. The agenda setters are administered-by and administrators-of the boundary simultaneously; their directionality derives from their enforcement position, not from rent collection, which is why no directionality_overrides entries are needed: beneficiary/victim declarations plus exit options reproduce the true structure without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the violence asymmetry and performance gaps the boundary was built to manage are independently attested and unresolved, so no mandatrophy is declared and the arrangement is not a zombie mandate. The classification work cuts both ways. Calling this a snare would erase the genuine protective coordination that millions of women rely on and that the founding problem still justifies; calling it a rope would erase the asymmetric, actively enforced costs borne by trans women, intersex individuals, and appearance-policed cis women through the very same structure. Tangled_rope holds both truths: coordination and extraction run through one boundary, and the enforcement requirement is structural, not incidental. The absence of any sunset mechanism is itself diagnostic — nobody proposes transitional status for a category definition, which distinguishes this from scaffold-type arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This file instantiates one reading — sex_biology_reading — of the kernel woman_female_category. How would instantiating a sibling reading instead change the structural data?',
    'Compile the sibling files separately and compare computed per-seat types; never merge readings into one story (epsilon-invariance discipline).',
    'Under gender_identity_reading the victim and beneficiary sets invert — natal females lose designated protection and trans women gain recognition. Under hybrid_contextual_reading the victim set fragments by context. Aggregating across readings would fabricate a single epsilon over two different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings; disagreement located at the membership criterion (developmental biology vs self-identification vs context-switching).').

omega_variable(
    operational_criterion_selection,
    'Which operational tier governs when the biological criteria conflict — chromosomes, gonads, gamete-production capacity, or anatomy? An XY woman with complete androgen insensitivity passes anatomy and fails chromosomes; every post-menopausal woman fails gamete capacity.',
    'Survey enacted eligibility rules across sport federations, prison services, and civil registration law; identify which tier each binding instrument actually uses.',
    'Strictest-tier selection raises the cost borne by intersex/DSD individuals and by older and prepubertal females; a gamete-capacity tier would disqualify most of the category''s own adult members and force immediate operational revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_criterion_selection, empirical, 'Edge-case divergence among chromosomal, anatomical, and gametic operationalizations of the biological criterion.').

omega_variable(
    carceral_safety_extractiveness_referent,
    'Does the high cost-bearing around physical safety in prisons and shelters attach to this reading''s own operation (trans women housed by birth sex facing assault risk) or to the rival self-ID arrangement this reading contests (natal female prisoners'' exposure where placement follows identity)?',
    'Per-jurisdiction placement-outcome and incident data under each criterion, controlling for population composition and facility type.',
    'If the costs attach to this reading''s operation, effective extraction on trapped trans prisoners dominates this file''s computation. If they attach to the rival arrangement, this reading functions as mitigation and belongs in the sibling file''s ledger. This story authors the former for its own referent and explicitly leaves the latter to the sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carceral_safety_extractiveness_referent, empirical, 'Referent ambiguity for the safety-domain cost concentration flagged in the manifest''s expected structural delta.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression measured on excluded payers structural (legal bars, facility rules, testing mandates) or internalized (anticipatory self-exclusion from female-designated spaces before any rule is invoked)?',
    'Post-reform trajectory studies: where jurisdictions broaden criteria, does usage of formerly exclusive provision by previously excluded women converge, or does self-exclusion persist after the barrier is removed?',
    'Internalized components raise effective suppression above the structural measure and persist after formal repeal; purely structural suppression falls with statute changes. The proportion split informs the omega''s resolution and any recomputed suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in exclusion from female-designated provision.').

omega_variable(
    policy_pendulum_phase,
    'Enforcement intensity oscillates (reform wave, incident, retrenchment, quiet accumulation) while the net trajectory ratchets upward — which phase were the story-level scalars sampled in, and does the cycle itself function as part of the mechanism (intermittent relief raising the perceived value of each concession)?',
    'Longitudinal coding of legislative sessions and federation rule revisions against incident calendars across at least two full pendulum cycles.',
    'If the oscillation is itself the mechanism, smoothing it understates effective suppression; dating any computed transition to the wrong cycle phase mis-times type-shift detection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_pendulum_phase, conceptual, 'Phase-dependence of the authored scalars under pendulum dynamics; the monotone series is a deliberate net-ratchet smoothing of a cyclical process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_female_category__sex_biology_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(woma_tr_t2004, woman_female_category__sex_biology_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(woma_tr_t2008, woman_female_category__sex_biology_reading, theater_ratio, 2008, 0.17).
narrative_ontology:measurement(woma_tr_t2012, woman_female_category__sex_biology_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__sex_biology_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(woma_tr_t2018, woman_female_category__sex_biology_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement(woma_tr_t2021, woman_female_category__sex_biology_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__sex_biology_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_female_category__sex_biology_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(woma_be_t2004, woman_female_category__sex_biology_reading, base_extractiveness, 2004, 0.46).
narrative_ontology:measurement(woma_be_t2008, woman_female_category__sex_biology_reading, base_extractiveness, 2008, 0.49).
narrative_ontology:measurement(woma_be_t2012, woman_female_category__sex_biology_reading, base_extractiveness, 2012, 0.52).
narrative_ontology:measurement(woma_be_t2015, woman_female_category__sex_biology_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(woma_be_t2018, woman_female_category__sex_biology_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(woma_be_t2021, woman_female_category__sex_biology_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__sex_biology_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_female_category__sex_biology_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement(woma_su_t2004, woman_female_category__sex_biology_reading, suppression_requirement, 2004, 0.29).
narrative_ontology:measurement(woma_su_t2008, woman_female_category__sex_biology_reading, suppression_requirement, 2008, 0.34).
narrative_ontology:measurement(woma_su_t2012, woman_female_category__sex_biology_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(woma_su_t2015, woman_female_category__sex_biology_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(woma_su_t2018, woman_female_category__sex_biology_reading, suppression_requirement, 2018, 0.49).
narrative_ontology:measurement(woma_su_t2021, woman_female_category__sex_biology_reading, suppression_requirement, 2021, 0.52).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__sex_biology_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of the kernel woman_female_category, decomposed per the epsilon-invariance principle because the colloquial label 'what makes someone a woman' conflates three structurally distinct claims with distinct victim sets and distinct epsilons. Upstream/downstream: sex_biology_reading is the older codification and supplies the biological criterion that hybrid_contextual_reading borrows for its safety-context limb — changes in this reading's legitimacy propagate directly into the hybrid's foundations (declared as an influences edge). gender_identity_reading is the downstream challenger; as a uniform criterion it is mutually exclusive with this reading, declared as forecloses. Cross-family comparison is valid only at the per-seat level after separate compilation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
