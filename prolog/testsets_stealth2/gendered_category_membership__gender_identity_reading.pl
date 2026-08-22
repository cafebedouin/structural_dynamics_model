% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gendered Category Membership by Self-Declared Identity (Gender-Identity Reading)
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates the gender_identity_reading of the
 *   gendered_category_membership kernel: the arrangement under description is
 *   the one this reading institutes wherever it governs — membership in
 *   gendered categories ('woman', 'man') is constituted by self-declared
 *   identity, and the institutional machinery (legal recognition, facilities,
 *   sports eligibility, service admission) keys to declaration rather than to
 *   birth anatomy or third-party recognition. The epsilon referent is this
 *   identity-governed arrangement itself, assessed by the reading's own
 *   lights: hence moderate rather than negligible extraction — even on
 *   sympathetic assessment the arrangement concentrates real costs on
 *   identifiable seats (dissenting cis women positioned as perpetrators when
 *   they resist, elite athletes, service providers carrying liability in both
 *   directions) while delivering recognition and administrative
 *   simplification. The sibling readings (biological_sex_reading,
 *   social_role_reading) are separate constraint files with their own epsilon
 *   and victim sets; the contest between readings is recorded in omega
 *   variables, not inside this constraint. Claim and metrics are authored
 *   independently: the reading's own claim is that the arrangement is
 *   substantially just coordination; the authored metrics describe its
 *   observed operation including its extractive margins. Receipt surface: the
 *   arrangement's transfers land on the trans_women seat by design; fixing is
 *   prohibitive because reversal would unwind legal recognition retroactively
 *   for everyone holding it. Interval anchors: t=0 corresponds to 2015 (first
 *   comprehensive self-ID statutes), t=10 to 2025.
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary (organized/identity_locked) — gain category membership, corrected documentation, and access by declaration; the category is constitutive of their standing
 *   - dissenting_cis_women: Primary target (moderate/identity_locked) — bear sanction and perpetrator-framing; cannot exit the category whose meaning is the disputed object
 *   - elite_female_athletes: Secondary target (organized/constrained) — bear competitive displacement and safety costs in identity-admitting female categories
 *   - womens_service_providers: Secondary target (moderate/constrained) — bear compliance obligations and bidirectional liability on identity-based admission
 *   - policy_administrating_institutions: Agenda setter (institutional/constrained) — enact, interpret, and enforce the criterion; collect verification-cost savings
 *   - supportive_cis_women: Beneficiary (organized/mobile) — endorse and normalize the criterion at no personal cost
 *   - trans_men: Beneficiary (moderate/identity_locked) — analogous inclusion in the 'man' category
 *   - survivors_seeking_single_sex_provision: Excluded voice (powerless/trapped) — unseated preference for single-sex crisis provision
 *   - analytical_bioethicists: Analytical observer (analytical/analytical) — maps the full structure; nothing material turns on their conclusions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.52).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.62).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gendered Category Membership by Self-Declared Identity (Gender-Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '1c439ffb-12b1-41b6-a48d-9546c4968cd5').
narrative_ontology:cs_kernel_codification('1c439ffb-12b1-41b6-a48d-9546c4968cd5', formalized).
narrative_ontology:cs_authority_grounding('1c439ffb-12b1-41b6-a48d-9546c4968cd5', expertise).
narrative_ontology:cs_interpretation_layer_present('1c439ffb-12b1-41b6-a48d-9546c4968cd5').
narrative_ontology:cs_reading_relation('1c439ffb-12b1-41b6-a48d-9546c4968cd5', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('1c439ffb-12b1-41b6-a48d-9546c4968cd5', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('1c439ffb-12b1-41b6-a48d-9546c4968cd5', foundational, self_declared_identity_constitutes_membership).
narrative_ontology:cs_axiom_status(self_declared_identity_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('1c439ffb-12b1-41b6-a48d-9546c4968cd5', self_declared_identity_constitutes_membership, deontological).
narrative_ontology:cs_axiom('1c439ffb-12b1-41b6-a48d-9546c4968cd5', secondary, identity_gatekeeping_is_impermissible).
narrative_ontology:cs_axiom_status(identity_gatekeeping_is_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('1c439ffb-12b1-41b6-a48d-9546c4968cd5', identity_gatekeeping_is_impermissible, deontological).
narrative_ontology:cs_reference_frame('1c439ffb-12b1-41b6-a48d-9546c4968cd5', self_declared_identity_membership).
narrative_ontology:cs_drift_state('1c439ffb-12b1-41b6-a48d-9546c4968cd5', contemporary_backlash_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1c439ffb-12b1-41b6-a48d-9546c4968cd5', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, supportive_cis_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, policy_administrating_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, dissenting_cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, elite_female_athletes).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, womens_service_providers).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, gender_self_identification_doctrine).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, dignity_based_recognition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under category systems that historically assigned them 'male' at birth and required psychiatric diagnosis — and in some jurisdictions sterilization or divorce — to change legal documents. Under this arrangement they declare their identity and obtain recognition, access to gendered facilities and services, and documentation consistent with how they live. Their stake in the category is constitutive: the recognition is of themselves, not of a role they occupy, so leaving the category system is not an available remedy for dissatisfaction with it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    organized, biographical, identity_locked, global).

% Are included in the 'man' category by the same declaration criterion. Their recognition needs mirror the trans women's case — corrected documents, facility access, freedom from disclosure risk — and their identity stake is equally constitutive, though the public contest centers less on their inclusion.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_men, beneficiary,
    moderate, biographical, identity_locked, global).

% Endorse identity-based membership and adjust their language and associations accordingly. Nothing is asked of them that they do not already affirm: no verification is demanded of anyone, and they experience the arrangement mainly as a simplification of social norms. Their support is expressed through workplaces, unions, and advocacy organizations.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, supportive_cis_women, beneficiary,
    organized, biographical, mobile, national).

% Hold that their own category retains meanings tied to sex-based experience and object to redefinition by declaration. Under the arrangement their objection is reframed as exclusionary conduct: employers treat refusals to use preferred language as policy violations, public bodies invoke dignity codes, and advocacy networks campaign against them by name. Compliance is available at the cost of abandoning the objection; exit from the category whose meaning is contested is not, because their own membership is what the dispute is about.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, dissenting_cis_women, payer,
    moderate, biographical, identity_locked, national).

% Compete in the female category, which under this arrangement admits members by declared identity. Where physiological differences developed under male puberty affect performance or contact safety, they bear displaced placings, lost records, and injury risk. Governing bodies set eligibility rules above their heads, and speaking publicly has ended sponsorships and team positions for some who tried.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, elite_female_athletes, payer,
    organized, biographical, constrained, global).

% Operate refuges, shelters, and prison-adjacent services whose admission criteria were built around sex-based vulnerability. Under the arrangement they must assess applicants by declared identity; funders and regulators condition licenses on compliance, while negligence liability attaches if they admit a resident who harms others. They carry the decision risk in both directions and absorb the legal costs of testing it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, womens_service_providers, payer,
    moderate, generational, constrained, national).

% Legislatures, courts, regulatory bodies, and organizational compliance functions adopt, interpret, and enforce identity-based membership rules. They gain a uniform administrable criterion that removes verification burdens and discrimination exposure, and they bear the cost of defending the rules through tribunals, guidance revisions, and periodic political challenge.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, policy_administrating_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, policy_administrating_institutions, beneficiary).

% Women escaping male violence who would choose a single-sex refuge if one existed locally. Consultations on admission criteria rarely seat them; their preferences surface only in provider surveys, if at all, and their need for the service is immediate enough that they cannot wait out the policy cycle.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, survivors_seeking_single_sex_provision, excluded,
    powerless, immediate, trapped, local).

% Map the category dispute across medicine, law, and philosophy, publishing analyses of competing membership criteria and taking no side in allocation. Their seat sees the full structure because nothing material turns on their conclusions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, analytical_bioethicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns people to gendered categories and synchronizes everything that keys to category membership — identity documents, facility access, forms of address, sports eligibility, service admission — using self-declaration as the sole criterion, which removes verification infrastructure and resolves mismatch cases (people whose documents, anatomy, and lived category diverge) that fixed-marker schemes handle by force or exception.
% TRANSFER_FUNCTION: Moves recognition, access, and standing: declaring members gain category membership, corrected documents, and access to gendered provisions; dissenting cis women transfer social standing (from rights-holders to suspected exclusionary actors), athletes transfer placings and records where physiology differs, service providers transfer admission authority to the declarer; administering institutions shed verification labor and discrimination exposure.
% ABSENT_VOICES: Survivors of male violence who would choose single-sex crisis provision are not seated in admission-policy consultations; women who object but lack institutional platforms speak through surveys and anonymous channels; detransitioned people and gender-nonconforming people who prefer sex-based provisions are marginal in both advocacy coalitions; custodial populations affected by placement policy have no seat at all.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, everyone recognized under it would lose legal category membership retroactively: gender-recognition certificates would void, documents would revert to birth markers, facility and service access would re-key to anatomy or diagnosis, sports categories would reorganize around eligibility testing, and providers would rewrite admission criteria — the legal standing of hundreds of thousands of people, and the administrative systems built on it, would reorganize within months.
% FOUNDING_PROBLEM: Trans people could not obtain legal and social recognition consistent with their identity: recognition routes ran through psychiatric diagnosis, and in several jurisdictions through sterilization or divorce requirements; documents mismatched to lived identity exposed holders to violence, denial of services, and daily disclosure risk.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: World Health Organization and PAHO publications document violence and access barriers against trans populations; ILGA-Europe and Amnesty International track statutes still requiring diagnosis, sterilization, or divorce; legal historians attest the sterilization-era provisions of earlier recognition statutes. None of these sources collects anything from the arrangement.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the arrangement transfers standing, access, and institutional autonomy along identifiable channels — recognition and access flow to declaring members, while sanction risk, competitive displacement, and bidirectional liability land on named paying seats; the transfers are real but partially reciprocated by the coordination value of a verification-free criterion. Suppression 0.62: persistence depends on active enforcement — workplace dignity codes, funding conditions on providers, platform and professional sanctions — rather than on spontaneous assent; the perpetrator-framing of resisters is the arrangement's distinctive suppressive instrument. Theater ratio 0.28: the recognition function is real and heavily used, but a growing share of activity is performative compliance (declarations, training modules, policy restatements) that signals alignment without changing outcomes. Accessibility collapse 0.35: alternatives remain live — biological-criterion regimes still govern many jurisdictions and hybrid systems persist, so understanding the arrangement does not close exits at system scale. Resistance 0.65: sustained legal challenge, legislative reversal attempts, sports-governance disputes, and jurisdictional divergence (adoption in some polities, statutory rollback in others) mark this as a defended construct, not settled background. All three metric series run on one shared time grid (t=0..10) so every tracked metric is authored at every examined point; the trend is monotonic rather than cyclical, with episodic backlash flare-ups averaging into the rising enforcement line.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme because the seats' identities are fused with opposite sides of the same category boundary. From the trans_women seat the arrangement is recognition after decades of medicalized gatekeeping — effective extraction sits near the subsidy end, and the identity lock cuts the other way (exit means misrecognition). From the dissenting_cis_women seat the same arrangement expropriates the meaning of a category they belong to involuntarily and recodes their objection as misconduct — effective extraction sits near the full-target end, amplified because identity lock denies them the exit of indifferent compliance. Elite athletes and service providers compute intermediate: bounded, domain-specific costs without identity fusion. The agenda-setting institutions sit near symmetric-low: they administer the criterion and collect verification savings, but bear defense costs in tribunals and revision cycles. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (trans_women, trans_men, supportive_cis_women, plus the administering institutions' verification savings) drive those seats toward the beneficiary end of d; victim declarations (dissenting_cis_women, elite_female_athletes, womens_service_providers) drive them toward the target end. Exit modulation orders the targets: dissenting_cis_women are identity_locked — their own category membership is the disputed object, so no neutral exit exists — placing them nearest full-target; athletes and providers are constrained (career-bound, license-bound) but not identity-fused, sitting slightly back. Supportive cis women are mobile endorsers with nothing extracted from them, nearest the subsidy end. Scope is national-to-global depending on seat; larger scope amplifies effective extraction modestly for the paying seats because verification of contested claims is harder at scale. Suppression is authored as a raw structural property and is deliberately not scaled — only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — obtaining recognition consistent with identity without medicalized or humiliating gatekeeping — remains live for most of the world's trans population, so no mandatrophy is declared and the mismatch consumer finds status=live paired with verdict=world_rearranges, producing no zombie flag. The classification discipline cuts both ways here: reading the arrangement as pure rope would erase the named paying seats and the enforcement dependence; reading it as snare would erase the genuine coordination function (verification-free assignment solves real mismatch problems that fixed-marker schemes handle badly) and the live founding problem that justifies it. Tangled rope holds both halves: the same structure that assigns categories without verification also channels sanction, displacement, and liability onto seats that did not agree to the criterion. If the founding problem were ever solved globally and enforcement decayed while the policy apparatus persisted, the arrangement would drift toward inertial maintenance — theatrical compliance around an atrophied function — which the theater_ratio series would register first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This story instantiates the gender_identity_reading of the gendered_category_membership kernel; how would the biological_sex_reading or the social_role_reading restructure the same arrangement?',
    'Not resolvable by data within this file: the sibling readings are separate constraints with their own epsilon, beneficiary/victim sets, and classifications. Resolution proceeds by generating and comparing the sibling stories.',
    'Under the biological reading the victim set inverts (trans people excluded from categories become the paying seats) and extraction redistributes toward documentation and medical gatekeeping; under the social-role reading gatekeeping shifts to recognition-by-others tests. Cross-reading comparison is the corpus-level measurement this omega routes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas recorded here rather than inside the constraint body.').

omega_variable(
    payer_cost_magnitude_distribution,
    'How large are the costs actually borne by the paying seats — competitive displacement in sport, bidirectional liability for service providers, sanction rates against dissenters — relative to the coordination value the arrangement delivers?',
    'Longitudinal outcome data from self-ID jurisdictions: elite-sport performance and injury records, refuge incident and insurance data, employment-tribunal and professional-sanction statistics.',
    'If costs concentrate narrowly on few seats at high intensity, the arrangement computes toward the extraction-heavy end; if costs prove diffuse and small, it computes toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_cost_magnitude_distribution, empirical, 'Magnitude and concentration of costs on the paying seats.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression on dissenting cis women structural (employment policy, funding conditions, platform and professional sanctions) or internalized (self-silencing produced by the perpetrator framing)?',
    'Preference-falsification measurement: compare anonymous versus attributed opinion surveys, and track speech trajectories in jurisdictions where sanction regimes are relaxed.',
    'If internalized, effective suppression exceeds the structural measure and persists after policy reversal; if structural, lifting enforcement restores dissent quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism for the dissenting seat.').

omega_variable(
    coordination_extraction_separability,
    'Is the recognition and administration function of self-declared membership separable from its extension into contested domains (elite sport, custodial settings, data collection)?',
    'Jurisdictional natural experiments where self-ID governs documents and civil recognition but not sports eligibility or custodial placement: compare outcomes across the boundary.',
    'If separable, the extraction component rides on domains the reading did not need to capture; if inseparable, part of the measured extraction is intrinsic to the reading''s own criterion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the reading''s coordination and extraction components are structurally separable.').

omega_variable(
    perpetrator_framing_durability,
    'Does the positioning of resisting cis women as perpetrators follow from the reading''s operative logic (objection to identity-based membership is treated as exclusionary conduct) or from a contingent rhetorical environment that could soften?',
    'Comparative policy analysis across jurisdictions with and without expressive-conduct penalties, tracking whether dissent is treated as protected opinion or actionable misconduct.',
    'If logic-immanent, the paying seat''s extraction is stable under the reading and seat divergence is permanent; if contingent, suppression falls with rhetorical normalization without any change to the criterion itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(perpetrator_framing_durability, conceptual, 'Durability of the perpetrator-framing mechanism applied to the dissenting seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gend_tr_t2, gendered_category_membership__gender_identity_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__gender_identity_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(gend_tr_t6, gendered_category_membership__gender_identity_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__gender_identity_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.28).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gend_be_t2, gendered_category_membership__gender_identity_reading, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__gender_identity_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(gend_be_t6, gendered_category_membership__gender_identity_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__gender_identity_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gend_su_t2, gendered_category_membership__gender_identity_reading, suppression_requirement, 2, 0.49).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__gender_identity_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(gend_su_t6, gendered_category_membership__gender_identity_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__gender_identity_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what determines gendered category membership' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the biological reading (fixed markers; extraction concentrated in documentation and medical gatekeeping; trans people as paying seats), this identity reading (declaration; moderate extraction; dissenting cis women, athletes, and providers as paying seats), and the social-role reading (recognized performance; extraction concentrated in recognition-labor demands). Each file authors its own epsilon, victim set, and classification; this file links both siblings because its legal victories reshape their operating environments and its premise contests theirs. Upstream/downstream: the biological reading was the inherited legal order this reading displaces; the social-role reading competes laterally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
