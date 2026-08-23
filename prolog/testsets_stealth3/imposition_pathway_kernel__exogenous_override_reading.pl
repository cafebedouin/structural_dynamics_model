% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: Exogenous Override Reading of the Imposition Pathway Kernel
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the imposition_pathway_kernel: the
 *   exogenous_override_reading, which holds that sufficiently capable states
 *   displace commitments (calendars, dress, ritual) directly by decree and
 *   enforcement, with no meaningful fringe-adoption stage — Meiji Japan's
 *   1873 calendar and dress changes being the anchor case — and that top-down
 *   imposition therefore requires its own cell in the M-set mechanism
 *   taxonomy. The kernel label 'how commitment displacement happens'
 *   decomposes into three structurally distinct claims (this reading plus the
 *   endogenous_climb and hybrid_cascade siblings), each with its own epsilon,
 *   beneficiary structure, and classification; per the epsilon-invariance
 *   principle they are separate stories linked by network edges, not one
 *   story with a measurement dial. The epsilon referent here is the override
 *   rule itself as a standing analytical arrangement — the category as it
 *   operates in strong-state literatures and in the amendment campaign —
 *   assessed by this reading's own lights, never the climb framework this
 *   reading opposes. The claim/metric gap is deliberate: the reading is
 *   CLAIMED as tangled_rope (genuine coordination function in a shared
 *   taxonomy plus real asymmetric extraction) while the metrics independently
 *   describe moderately extractive, actively enforced, heavily resisted
 *   operation — the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - override_school_leaders: agenda-setting beneficiary (institutional/identity_locked) — administers the override category, sets the no-meaningful-fringe evidentiary standard, collects citation capital
 *   - state_formation_scholars: primary beneficiary (organized/mobile) — gain case ownership and publication economy as the category spreads
 *   - meiji_case_specialists: dual-positioned beneficiary/payer (moderate/constrained) — their archival finding anchors the category; they bear pressure to defend the strong binary
 *   - endogenous_climb_modelers: primary target (organized/identity_locked) — lose model coverage and research time defending the unified framework
 *   - microhistory_evidence_producers: secondary target (moderate/constrained) — junior archivists whose faint-positive findings are classified as noise
 *   - non_western_historiographers: excluded voice (moderate/trapped) — hold relevant evidence but no seat in the allocation conversation
 *   - philosophy_of_social_science_observers: analytical observer (analytical/analytical) — sees the full structure without case ownership
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.5).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.55).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "Exogenous Override Reading of the Imposition Pathway Kernel").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'cac9a014-6ff5-4362-9a47-fe31bd1c84a0').
narrative_ontology:cs_kernel_codification('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', formalized).
narrative_ontology:cs_authority_grounding('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', expertise).
narrative_ontology:cs_interpretation_layer_present('cac9a014-6ff5-4362-9a47-fe31bd1c84a0').
narrative_ontology:cs_reading_relation('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', imposition_pathway_kernel__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', foundational, imposition_creates_commitment_directly).
narrative_ontology:cs_axiom_status(imposition_creates_commitment_directly, holdable).
narrative_ontology:cs_axiom_grounding('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', imposition_creates_commitment_directly, empirically_contingent).
narrative_ontology:cs_axiom('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', foundational, coerced_compliance_constitutes_commitment_change).
narrative_ontology:cs_axiom_status(coerced_compliance_constitutes_commitment_change, holdable).
narrative_ontology:cs_axiom_grounding('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', coerced_compliance_constitutes_commitment_change, conventional).
narrative_ontology:cs_reference_frame('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', mechanism_differentiated_taxonomy).
narrative_ontology:cs_drift_state('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', current_classification_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cac9a014-6ff5-4362-9a47-fe31bd1c84a0', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, state_formation_scholars).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, meiji_case_specialists).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, endogenous_climb_modelers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, microhistory_evidence_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, override_school_leaders).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, meiji_case_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead the state-centered research program within comparative historical sociology. Edit the special issues and handbook chapters where the override category is applied, referee which episodes qualify as decree-driven rather than adoption-driven, and set the evidentiary standard for what counts as a meaningful pre-decree fringe. Their program's authority and citation concentration grow with the category's spread; leaving the program would dissolve the institutional identity they have built around it.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, override_school_leaders, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, override_school_leaders, beneficiary).

% Rank-and-file of the state-centered school. Gain dedicated case ownership, publication slots, and framework validation as the override category spreads through strong-state literatures (Meiji Japan, Prussian reform, Soviet social engineering). They can move between university departments, policy schools, and area-studies venues if the category falls out of favor.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, state_formation_scholars, beneficiary,
    organized, biographical, mobile, global).

% Japan-studies archivists and historians whose documentary finding — no meaningful calendar or dress adoption before the 1873 decrees — anchors the override category empirically. They are vindicated by the category's success but also pressed into service: expected to defend the strong binary reading of their evidence and to treat faint pre-decree adoption signals as noise rather than pursue them.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, meiji_case_specialists, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, meiji_case_specialists, payer).

% Holders of the unified fringe-adoption framework. Every episode carved out as a genuine override shrinks their model's coverage and undermines the parsimony ideal their careers are built on. Fighting the amendment proposal consumes research time and editorial goodwill; abandoning the unified framework would mean disowning the theoretical identity they formed during training.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, endogenous_climb_modelers, payer,
    organized, generational, identity_locked, global).

% Junior historians and graduate students producing granular adoption evidence from local registers, merchant ledgers, and parish records. Under the override rule, their faint-positive findings are classified as noise, which taxes their interpretive labor and narrows their publication prospects; their career stage leaves them little leverage to contest the classification.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, microhistory_evidence_producers, payer,
    moderate, immediate, constrained, local).

% Area-studies traditions with independent accounts of Meiji-era and comparable commitment changes, publishing in language-specific venues that do not feed the Anglophone classification debate. They hold relevant evidence about how decree compliance was experienced locally but have no seat in the conversations that allocate the override category.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, non_western_historiographers, excluded,
    moderate, generational, trapped, continental).

% Philosophers and science-studies analysts tracking the mechanism-pluralism debate. They own no cases and collect no citation capital from any cell winning; they assess whether the taxonomy debate is resolving empirically or degenerating into boundary politics.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, philosophy_of_social_science_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, state_formation_scholars).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the comparative field a shared classification vocabulary for commitment-displacement episodes: the M-set lets scholars sort heterogeneous mechanisms into comparable cells so that findings accumulate across cases instead of relitigating taxonomy paper by paper.
% TRANSFER_FUNCTION: Moves classification authority over imposition episodes from the climb-modeling community to the state-centered school; moves citation capital, case prestige, and graduate-training pipelines toward state-capacity explanations; absorbs junior scholars' archival labor as supporting evidence for the override cell.
% ABSENT_VOICES: Non-Western historiographical traditions and local-archive researchers would object that the category is being allocated without the evidence they hold about how decree compliance was actually lived; they sit outside the Anglophone theory venues where the M-set is negotiated, in area-studies journals the framework literature does not cite.
% DISAPPEARANCE_RATIONALE: If the override category and its enforcement vanished overnight, imposition episodes would flow back into the climb template, the state-centered school would lose its case ownership and special-issue economy, Meiji specialists would lose their anchoring role, and graduate training would revert to the unified framework — the classification economy of the subfield visibly reorganizes around the category's presence.
% FOUNDING_PROBLEM: Comparative scholars kept encountering state-driven commitment changes — calendars, dress codes, naming conventions, ritual obligations — that did not fit diffusion and adoption curves, and the field needed a way to classify them without either denying state agency or dissolving the climb model's generality.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative-methods literature documents recurring forced-conversion and decree cases that resist adoption-curve modeling; handbook editors and journal reviewers across subfields attest the persistent classification failures; and the continued vitality of both sibling readings is itself evidence the underlying classification problem remains unsolved. The override school's own attestation is interested and is not counted here.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.50 at interval end): the override category genuinely improves descriptive accuracy where it applies, but its operation reallocates case ownership and taxes rivals' frameworks, and the evidentiary burden it sets (prove absence of fringe) is costly to discharge. Suppression (0.55) is a raw structural property, unscaled by power or scope: it consists of review gatekeeping, special-issue control, and the classification of inconvenient micro-evidence as noise — real coercion by academic standards, short of outright exclusion. Theater ratio (0.28) is low-moderate: most activity is functional classification work, with a growing rhetorical share devoted to boundary-policing ('impositions are never climbs'). Accessibility collapse is low (0.25): both sibling readings remain fully live and publishable, so alternatives do not collapse under the category. Resistance is high (0.70): the amendment campaign meets sustained organized pushback from the climb-modeling community. The temporal series share one grid ({0,6,12,18,24,30,36}); all three metrics are authored at every point. The rising suppression_requirement series is authored deliberately: this story traces enforcement-capacity build-up — the gatekeeping machinery the school constructs to hold the category against two sibling readings — not merely shifting extraction, which is why the scalar base_properties.suppression alone would under-describe the dynamic.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the override_school_leaders seat the category is a hard-won corrective their program built and defends; from the endogenous_climb_modelers seat the same category is an amputation of their framework's generality, resisted at growing cost. The meiji_case_specialists seat is internally split: vindication and flattening arrive together, which is why they carry both beneficiary and payer roles. Identity-lock dynamics bind the two theorist seats from opposite directions: the climb-modelers' professional identity is constituted by the parsimony ideal (ideological fusion — exit would mean disowning their training), and the override leaders' identity is fused with the state-centered program (institutional fusion — the program has become them). If either identity frame broke, the climb-modelers would most plausibly defect to the hybrid_cascade reading as the cheapest retreat, and the override leaders would lose the enforcement capacity the category currently runs on.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (state_formation_scholars, meiji_case_specialists) derive low directionality for those seats — the category subsidizes them with case ownership and vindication. The victim declarations (endogenous_climb_modelers, microhistory_evidence_producers) derive high directionality — they bear the framework surgery and the noise-classification of their evidence, and the climb-modelers' identity_locked exit pushes them toward the full-target end since they cannot cheaply relocate. The override_school_leaders sit nearest the beneficiary pole: they administer the standard and collect its citation returns. No directionality overrides are used: the derivation from declared beneficiaries, victims, power atoms, and exit options captures the structure, and the schema's power-atom granularity for overrides is too coarse to improve on it here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — classifying state-driven commitment change without collapsing it into adoption curves — is live, so no mandatrophy is declared and none is resolved. The classification matters here in both directions: reading the override category as pure extraction would erase its genuine coordination contribution (a shared, accurate taxonomy is a collective good the whole field consumes, including eventually the climb-modelers' students); reading it as pure coordination would miss the reallocate-and-tax operation visible in the temporal series. The tangled_rope claim holds both halves apart. The mismatch consumer should note that founding_problem_status=live combined with verdict=world_rearranges is the healthy configuration — a dead-problem-plus-rearrangement pairing would flag zombie dynamics that this story does not exhibit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Is the disagreement among the three readings of imposition_pathway_kernel located in the empirical record (did Meiji-era Japan exhibit meaningful pre-decree fringe adoption or not) or in the definition of ''meaningful fringe adoption'' (what quantity of pre-decree adoption disqualifies override status)?',
    'A pre-registered archival census of pre-decree adoption signals (merchant ledgers, early adopters, regional variation) with the meaningfulness threshold fixed before the data are examined; if readings still diverge after threshold agreement, the residue is definitional.',
    'If the disagreement is empirical, evidence can adjudicate and one reading should win; if definitional, the readings coexist as conceptual variants of one taxonomy and the M-set should carry explicit threshold parameters rather than competing cells.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the kernel contest is empirical or definitional in location.').

omega_variable(
    sibling_reading_structural_delta,
    'What would each sibling reading change structurally if it prevailed over this one?',
    'Direct comparison of the sibling stories'' authored structures: under endogenous_climb_reading, Meiji becomes a compressed climb with invisible fringe stages, the victim set inverts (override-school scholars become the payers whose category is dissolved), and the M-set gains no cell; under hybrid_cascade_reading, the artificial-fringe mechanism absorbs the case, meiji_case_specialists'' binary finding is reframed as the initiation half of a cascade, and a cascade cell is added alongside rather than instead of override.',
    'Each sibling victory produces a different beneficiary/victim topology and a different M-set shape; classification of this story''s seats is conditional on which reading wins the kernel contest, so per-seat results should be read as indexed to this reading''s prevalence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame record of what sibling readings would structurally change.').

omega_variable(
    meaningful_fringe_threshold,
    'What quantity of pre-decree adoption separates a genuine exogenous override from a merely compressed endogenous climb?',
    'Comparative calibration across multiple decree episodes (Meiji calendar, Prussian reforms, Soviet campaigns): locate the adoption-signal floor below which enforcement, not emergence, predicts subsequent compliance trajectories.',
    'A low threshold widens the override cell and increases extraction from climb-modelers; a high threshold shrinks it toward the hybrid reading''s territory and shifts this story''s classification toward the sibling''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_fringe_threshold, empirical, 'The quantitative boundary between override and compressed climb.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression in the classification economy structural (editorial gatekeeping, special-issue control, review hostility) or internalized (junior scholars self-censor faint-positive findings because they have learned the field rewards clean categories)?',
    'Post-liberalization trajectory: if submission patterns and topic selection shift after gatekeeping reforms (registered reports, preprint-first norms) without personnel change, the internalized share is substantial; if they do not shift, the structural share dominates.',
    'If internalized, effective suppression exceeds the structural measure and would persist after enforcement reform — the constraint''s coercive force travels inside the targets; if structural, enforcement reform alone would release the suppressed evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in the discipline.').

omega_variable(
    override_category_naturalness,
    'Is the override category a discovered joint in the phenomenon (state-imposed displacement really is mechanistically distinct) or a constructed category serving the state-centered school''s interests?',
    'Out-of-sample test: apply the category''s diagnostic criteria to decree episodes the school did not study (Ottoman, Qing, colonial-administrative mandates) and check whether classification accuracy survives without the school''s curation.',
    'If accuracy survives, the category earns its cell on the merits and extraction estimates should be discounted accordingly; if it fails out of sample, the category is interest-serving construction and this story''s classification should shift toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_category_naturalness, conceptual, 'Discovery versus construction in the override category''s status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipk_exog_tr_t0, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ipk_exog_tr_t6, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement(ipk_exog_tr_t12, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(ipk_exog_tr_t18, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(ipk_exog_tr_t24, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(ipk_exog_tr_t30, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(ipk_exog_tr_t36, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(ipk_exog_be_t0, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ipk_exog_be_t6, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement(ipk_exog_be_t12, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(ipk_exog_be_t18, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 18, 0.43).
narrative_ontology:measurement(ipk_exog_be_t24, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(ipk_exog_be_t30, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(ipk_exog_be_t36, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 36, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ipk_exog_su_t0, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ipk_exog_su_t6, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(ipk_exog_su_t12, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(ipk_exog_su_t18, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 18, 0.45).
narrative_ontology:measurement(ipk_exog_su_t24, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(ipk_exog_su_t30, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(ipk_exog_su_t36, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 36, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, information_standard).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how commitment displacement happens' decomposes into three structurally distinct claims per the epsilon-invariance principle. The endogenous_climb_reading is the upstream, historically established position (highest empirical confidence in the diffusion literature) and influences both downstream readings, which define themselves against it. This story (exogenous_override_reading) and the hybrid_cascade_reading are downstream challengers with different epsilon profiles: this reading's epsilon is indexed to the override rule's own operation in strong-state literatures, the hybrid reading's to the cascade mechanism's operation, and the upstream reading's to the defense of climb universality. All three files carry network links to at least one family member; orphaning any of them would break contamination-propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
