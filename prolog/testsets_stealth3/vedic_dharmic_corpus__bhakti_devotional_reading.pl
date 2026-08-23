% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional-Access Reading of the Dharmic Corpus
 *   domain: religious authority / social stratification / interpretive legitimacy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the vedic_dharmic_corpus kernel:
 *   the claim that direct devotional access to the divine bypasses caste
 *   requirements, and that sincere bhakti rather than birth determines
 *   spiritual authority. The arrangement under contest is the standing
 *   devotional settlement itself — congregational and vernacular practice
 *   open across caste lines, administered by sectarian lineages that assess
 *   sincerity, initiate teachers, and collect offerings and deference —
 *   assessed by this reading's own lights. Its epsilon referent is that
 *   standing arrangement, never the hereditary arrangement it displaced and
 *   never the reformist arrangement its critics propose. Structurally the
 *   settlement does two things at once: it coordinates (a portable,
 *   birth-independent standard of legitimacy that solved a real access
 *   problem for people excluded by birth) and it transfers (offerings, labor,
 *   and interpretive allegiance flow from devotees to lineage seats, and the
 *   promise of spiritual equality absorbs pressure that might otherwise
 *   demand material redistribution). Material caste hierarchy persists
 *   beneath the spiritual bypass; the victim set shrinks dramatically
 *   relative to the hereditary arrangement but does not empty. Per the
 *   epsilon-invariance principle, the sibling readings are separate stories
 *   with their own epsilon values; this file links them via
 *   network.affects_constraints and documents the decomposition in the
 *   dual-formulation note.
 *
 * KEY AGENTS:
 *   - - sectarian_guru_lineages: agenda-setting seat (organized/arbitrage) — administers the sincerity criterion, initiates teachers, collects offerings and deference
 *   - - outcaste_and_low_caste_devotees: primary intended beneficiary, secondary contributor (powerless/constrained) — gains recognized access, pays in offerings, labor, and assent
 *   - - hereditary_brahmin_priesthood: displaced cost-bearer (institutional/identity_locked) — loses interpretive monopoly, adapts by absorbing devotional currents into temple office
 *   - - devotional_women: partial beneficiary under intensified scrutiny (powerless/constrained) — gains devotional voice, remains under household and propriety constraint
 *   - - ruling_elite_patrons: indirect beneficiary (powerful/mobile) — endows the settlement, receives legitimation and channeled egalitarian sentiment
 *   - - caste_oppressed_residual_groups: residual cost-bearers (powerless/trapped) — material hierarchy persists beneath the spiritual bypass
 *   - - excluded_material_equality_advocates: excluded seat (moderate/constrained) — insists spiritual equality without redistribution is incomplete; outside the ratifying councils
 *   - - comparative_religion_scholars: analytical observer (analytical/analytical) — maps the displacement and absorption of the sincerity criterion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.38).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional-Access Reading of the Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious authority / social stratification / interpretive legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__bhakti_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '08c9a4f9-a166-4026-a611-746c03d4935d').
narrative_ontology:cs_kernel_codification('08c9a4f9-a166-4026-a611-746c03d4935d', fixed_text).
narrative_ontology:cs_authority_grounding('08c9a4f9-a166-4026-a611-746c03d4935d', practice).
narrative_ontology:cs_interpretation_layer_present('08c9a4f9-a166-4026-a611-746c03d4935d').
narrative_ontology:cs_reading_relation('08c9a4f9-a166-4026-a611-746c03d4935d', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('08c9a4f9-a166-4026-a611-746c03d4935d', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('08c9a4f9-a166-4026-a611-746c03d4935d', foundational, devotion_determines_spiritual_authority).
narrative_ontology:cs_axiom_status(devotion_determines_spiritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('08c9a4f9-a166-4026-a611-746c03d4935d', devotion_determines_spiritual_authority, theological).
narrative_ontology:cs_axiom('08c9a4f9-a166-4026-a611-746c03d4935d', secondary, divine_access_unmediated_by_birth).
narrative_ontology:cs_axiom_status(divine_access_unmediated_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('08c9a4f9-a166-4026-a611-746c03d4935d', divine_access_unmediated_by_birth, theological).
narrative_ontology:cs_axiom('08c9a4f9-a166-4026-a611-746c03d4935d', secondary, universal_initiation_regardless_of_caste).
narrative_ontology:cs_axiom_status(universal_initiation_regardless_of_caste, overridden).
narrative_ontology:cs_axiom_grounding('08c9a4f9-a166-4026-a611-746c03d4935d', universal_initiation_regardless_of_caste, theological).
narrative_ontology:cs_reference_frame('08c9a4f9-a166-4026-a611-746c03d4935d', realized_devotion_supremacy).
narrative_ontology:cs_drift_state('08c9a4f9-a166-4026-a611-746c03d4935d', contemporary_institutionalized_bhakti_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08c9a4f9-a166-4026-a611-746c03d4935d', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, outcaste_and_low_caste_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_women).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, sectarian_guru_lineages).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, ruling_elite_patrons).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_priesthood).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, caste_oppressed_residual_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_priesthood).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, outcaste_and_low_caste_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain lines of initiation and transmission, assess aspirants' devotion, authorize new teachers, and collect offerings, service, and deference from disciples. They administer the criterion that decides whose devotion counts as sincere and whose spiritual standing is recognized. When challenged, they can branch into new lineages, reframe doctrine, or relocate patronage.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, sectarian_guru_lineages, agenda_setter,
    organized, generational, arbitrage, regional).

% Enter devotional communities that birth-based rules kept closed to them, gaining recognized spiritual standing, vernacular worship, and community dignity. They contribute offerings, labor, and obedience to lineage teachers, and their advancement depends on assent from the same teachers. Moving between sects is possible, but social stigma and economic dependence travel with them.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, outcaste_and_low_caste_devotees, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, outcaste_and_low_caste_devotees, payer).

% Lose exclusive claim on ritual and interpretive authority as devotional legitimacy spreads beyond their ordination. They retain temple offices and adapt by incorporating devotional hymns into liturgy and staffing endowed temples. Their standing is constituted by birth, so abandoning that ground to compete on devotion would dissolve the identity on which their position rests.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_priesthood, payer,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_priesthood, beneficiary).

% Gain devotional voice and, rarely, teaching authority through renunciation or recognized ecstatic devotion. They remain under household and propriety constraints, and their sincerity is scrutinized more closely than men's. Respected teaching roles exist mainly for those who abandon conventional family standing entirely.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_women, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_women, payer).

% Kings, nobles, and merchant houses endow devotional temples and poet-saint lineages. They gain legitimation, integrate diverse subjects into shared worship, and see egalitarian sentiment channeled into devotion rather than into demands for redistribution. They can shift patronage between institutions at will.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, ruling_elite_patrons, beneficiary,
    powerful, generational, mobile, regional).

% Those for whom the devotional settlement changes little in material life: labor obligations, residential segregation, and denial of ordinary dignity persist beneath the promise of spiritual equality. Some enter devotional communities, others remain outside them; exit from the caste order itself is not on offer within this arrangement.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, caste_oppressed_residual_groups, payer,
    powerless, generational, trapped, continental).

% Rationalist and proto-reformist voices insisting that spiritual equality without material redistribution is incomplete. They stand outside the sectarian councils and patronage circuits where the devotional settlement was ratified; their objections surface later as reform movements rather than inside the settlement itself.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, excluded_material_equality_advocates, excluded,
    moderate, generational, constrained, continental).

% Study the devotional turn across centuries using epigraphy, hagiography, and vernacular corpora, mapping how the sincerity criterion displaced and was in turn absorbed by birth-based authority. They hold no stakes in the arrangement's continuation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, sectarian_guru_lineages).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a portable, birth-independent standard for spiritual legitimacy: shared practices (congregational song, pilgrimage, vernacular composition) and a common criterion (sincere devotion) that let people across caste lines form religious communities, transmit teaching, and recognize authority without priestly mediation.
% TRANSFER_FUNCTION: Moves offerings, labor, deference, and interpretive allegiance from devotees to sectarian institutions and lineage teachers; moves spiritual legitimacy from birth-lineages to demonstrated devotion as assessed by recognized teachers; and channels egalitarian aspiration into devotional forms, stabilizing the surrounding social order.
% ABSENT_VOICES: Those demanding material redistribution alongside spiritual equality — rationalist critics and later constitutional reformers — were absent from the sectarian councils and patronage networks that ratified the reading; women's full standing was debated but decided by male lineage heads. They are outside the room in which the criterion of sincerity was standardized.
% DISAPPEARANCE_RATIONALE: If the devotional-access criterion vanished overnight, millions of practitioners would lose the basis of their recognized spiritual standing, sectarian economies and temple networks would lose their legitimacy anchor, and the vacuum would be filled either by reasserted birth-gated authority or by rationalist reform — either way a major rearrangement of religious and social life.
% FOUNDING_PROBLEM: Birth-based ritual monopoly made legitimate spiritual access contingent on hereditary mediation unavailable to most people; the devotional movement formed to solve the problem of access — how those excluded by birth can reach recognized spiritual practice and authority.
% FOUNDING_PROBLEM_CORROBORATION: Epigraphic endowment records and court chronicles attest both the original exclusion and the scale of devotional incorporation; modern academic historiography of the Alvars, the poet-saints, and the panth traditions corroborates that access widened substantially while material caste hierarchy persisted. Much surviving testimony is hagiographic and insider-authored, which weakens corroboration; no source outside the benefiting parties attests that the founding problem is fully solved, and temple-entry disputes into the modern period corroborate that it was not.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.40: the settlement's own lights register real but bounded costs — resource flows to lineage seats, discretionary assessment of sincerity, and the pacifying effect of spiritual-only equality — against a genuinely enlarged access space. Suppression is 0.38 and is authored as a raw structural property (unscaled by power or scope): sect discipline, withholding of recognition, and communal sanction are real but softer than the enforcement the hereditary arrangement required. Theater ratio is 0.25: most devotional activity is functional (worship, teaching, community formation), with a performative share in public piety displays and lineage-legitimation ceremony. Accessibility collapse is 0.30: rival readings, competing sects, and secular paths remain visible and usable, so understanding this arrangement does not close the option space. Resistance is 0.55: the settlement met sustained organized opposition from incumbent orthodoxy, including persecution episodes recorded in hagiography and chronicle. The temporal series run on one shared grid (t=0..30, seven points) for all three tracked metrics; enforcement capacity (suppression_requirement) is tracked because the story specifically traces the build-up of sect discipline as institutions consolidated, then its partial relaxation under modern pluralism. Extractiveness and theater rise together through the institutional-consolidation phase and ease slightly at the end as print culture, lay literacy, and legal religious freedom loosen lineage control. The claimed type (tangled_rope) is stated from structure — genuine coordination function plus asymmetric transfer through the same structure plus active enforcement — independently of the metric values; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the measurement, not an error.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the lineage seat, the settlement is stewardship: they built and maintain the access structure and collect its upkeep. From the devotee seat, it is liberation with dues: access they could not otherwise obtain, purchased with offerings and assent. From the priesthood seat, it is dispossession: their monopoly rent and interpretive authority are taken by a criterion they cannot adopt without dissolving their own identity ground. From the patron seat, it is stability: egalitarian energy routed into devotion rather than redistribution. Same-nominal-level actors differ by constraint-specific factors: two powerless seats (devotees versus residual groups) diverge because one holds a recognized place inside the devotional community and the other does not; two institutional seats (lineages versus priesthood) diverge because one administers the criterion and the other is measured by it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for outcaste_and_low_caste_devotees, devotional_women, sectarian_guru_lineages, and ruling_elite_patrons; victim declarations drive high directionality for hereditary_brahmin_priesthood and caste_oppressed_residual_groups. The dual-positioned seats (devotees and women as beneficiary-with-secondary-payer; priesthood as payer-with-secondary-beneficiary) sit mid-range, with the devotee seats nearer the beneficiary end because the access gain dominates their payment burden, and the priesthood nearer the target end because what they lose is concentrated and irreplaceable. No directionality overrides are authored: the derivation chain already distinguishes the seats through role declarations and exit options, and a power-atom-keyed override could not separate the two powerless seats (net-beneficiary devotees versus trapped residual groups) without mispricing one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — birth-blocked access to legitimate spiritual practice — is contested rather than dead: access widened decisively inside devotional spheres while material caste hierarchy persisted and new assessment gates formed. The arrangement has therefore outlived a pure transitional role without becoming vestigial: it still performs its coordination function daily, but a growing share of its activity reproduces the institutions rather than the access (tracked by the rising theater series through the consolidation phase). The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-mandate zombie flag fires, but the contested status keeps the mandatrophy question open rather than resolved. Classification discipline cuts both ways here: labeling the settlement a pure rope would hide the lineage-seat capture of devotion's surplus; labeling it a pure snare would erase the largest widening of spiritual access in the tradition's history and misidentify its millions of net beneficiaries as victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (bhakti_devotional_reading) of the vedic_dharmic_corpus kernel; which specific structural element separates it from the sibling readings, and what would each sibling change if operative?',
    'Comparative structural audit of the three readings'' authority criteria, victim sets, and enforcement bases: birth-lineage qualification (hereditary_monopoly_reading), demonstrated devotion assessed by recognized lineages (this reading), constitutional-equality conformity of textual meaning (reformist_egalitarian_reading).',
    'If the hereditary reading were operative, extraction rises sharply (full varna exclusion, maximal victim set); if the reformist reading were operative, enforcement migrates to courts and curricula and the victim set contracts further. The authored epsilon of 0.40 is valid only for the devotional reading''s arrangement and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are separate constraints with their own epsilon and victim sets.').

omega_variable(
    sincerity_gate_discretion,
    'Does the sincerity criterion function as genuinely open access, or as a discretionary gate controlled by the lineage teachers who assess devotion?',
    'Initiation and admission records across the major sampradayas; documented cases of aspirants rejected, deferred, or admitted on patronage rather than devotion; comparison of lay-devotee versus initiated-teacher ratios over time.',
    'A tightly held gate raises effective extraction above the authored 0.40 and pushes the arrangement toward purely extractive operation; demonstrably open access lowers it toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_gate_discretion, empirical, 'Whether replacing birth with assessed devotion removed the gate or relocated it.').

omega_variable(
    safety_valve_or_lever,
    'Does the devotional bypass absorb and neutralize anti-caste energy (stabilizing the material hierarchy beneath it), or does it accumulate pressure that eventually erodes that hierarchy?',
    'Longitudinal tracing of devotional-community membership into later egalitarian and reform movements; analysis of whether panth networks supplied leaders, vocabulary, and mobilization structures to anti-caste politics.',
    'If the safety-valve reading holds, incumbent patrons belong among the beneficiaries and deflected emancipation counts as part of the arrangement''s costs; if the lever reading holds, the authored extractiveness overstates the arrangement''s conservatizing effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_valve_or_lever, conceptual, 'Whether spiritual egalitarianism stabilized or undermined the surrounding caste order.').

omega_variable(
    residual_caste_acknowledgment,
    'Does the devotional reading''s own framework treat continuing material caste disability as within its remedial remit, or does it declare the spiritual bypass sufficient on its own?',
    'Doctrinal analysis of how devotional theologians address persistent caste disability: as pending spiritual maturation, as irrelevant to salvation, or as a justice obligation the reading itself must carry.',
    'If the remit is declared spiritual-only, the residual victim set stands outside the arrangement''s accountability and the shrunk-but-present victim structure is stable; if justice enters the remit, unmet material obligations enlarge the effective victim count and raise measured costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_caste_acknowledgment, conceptual, 'Scope of the reading''s own accountability for the hierarchy it does not remove.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_bhakti_reading_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedic_bhakti_reading_tr_t5, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(vedic_bhakti_reading_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(vedic_bhakti_reading_tr_t15, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(vedic_bhakti_reading_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(vedic_bhakti_reading_tr_t25, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(vedic_bhakti_reading_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(vedic_bhakti_reading_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vedic_bhakti_reading_be_t5, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(vedic_bhakti_reading_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(vedic_bhakti_reading_be_t15, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(vedic_bhakti_reading_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(vedic_bhakti_reading_be_t25, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 25, 0.43).
narrative_ontology:measurement(vedic_bhakti_reading_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vedic_bhakti_reading_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(vedic_bhakti_reading_su_t5, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(vedic_bhakti_reading_su_t10, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(vedic_bhakti_reading_su_t15, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement(vedic_bhakti_reading_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(vedic_bhakti_reading_su_t25, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(vedic_bhakti_reading_su_t30, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the dharmic tradition's stance on caste and authority.' The label conflates three structurally distinct arrangements differing in the criterion of spiritual authority: birth into Brahmin lineage (hereditary_monopoly_reading), demonstrated devotion assessed by recognized lineages (this story), and conformity of textual meaning to constitutional equality principles (reformist_egalitarian_reading). Each has its own epsilon, victim set, and enforcement base; this story authors epsilon only for the devotional arrangement. The hereditary reading is upstream (the traditional baseline against which the devotional reading defines itself); the devotional reading is upstream of the reformist reading, whose advocates cite devotional precedent as indigenous evidence for egalitarian correction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
