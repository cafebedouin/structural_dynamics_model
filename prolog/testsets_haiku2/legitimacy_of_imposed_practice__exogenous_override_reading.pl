% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Authority Over Prior Practice (Exogenous Override Reading)
 *   domain: political/cultural
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous override reading of a
 *   contested kernel: the legitimacy of state decree authority to displace
 *   prior practice without bottom-up internalization. Under this reading,
 *   legal mandate alone—backed by administrative enforcement—is SUFFICIENT to
 *   establish compliance and reshape practice, regardless of whether subjects
 *   have accepted the legitimacy of the imposed norms. The reading asserts
 *   that state sovereign power creates binding obligation independent of
 *   cultural internalization or consent. We examine this reading as
 *   instantiated in two historical cases (Islamic calendar vs.
 *   Julian/Gregorian in state records; European dress codes imposed on
 *   colonized populations), measuring the constraint's persistence, the
 *   mechanisms sustaining it, and the structural contradiction between the
 *   claim (internalization irrelevant) and the observed metrics (high
 *   suppression requirement, substantial resistance, increasing theater ratio
 *   indicating performative rather than internalized compliance).
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus: Institutional agenda-setter. Enacts and enforces decrees. Powerful institutional exit options. Benefits from unified legible subjects.
 *   - rural_populations: Powerless payers. Trapped in jurisdiction; identity-locked to customary practice. Bear adjustment costs without consultation.
 *   - tradition_bearers: Moderate-power payers + excluded. Institutional identity (as keepers) makes exit identity-locked. Their authority is directly nullified.
 *   - urban_administrative_class: Powerful beneficiaries. Expand authority and employment through decree implementation. Experience legitimacy, not coercion.
 *   - alternative_authority_systems: Powerful but structurally excluded. Displaced by the decree itself. Cannot compete for legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.81).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Authority Over Prior Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political/cultural").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '095d4083-1ed9-4f40-838b-e7fd0992f23b').
narrative_ontology:cs_kernel_codification('095d4083-1ed9-4f40-838b-e7fd0992f23b', formalized).
narrative_ontology:cs_authority_grounding('095d4083-1ed9-4f40-838b-e7fd0992f23b', extraction).
narrative_ontology:cs_interpretation_layer_present('095d4083-1ed9-4f40-838b-e7fd0992f23b').
narrative_ontology:cs_reading_relation('095d4083-1ed9-4f40-838b-e7fd0992f23b', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('095d4083-1ed9-4f40-838b-e7fd0992f23b', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('095d4083-1ed9-4f40-838b-e7fd0992f23b', foundational, decree_sufficiency_over_internalization).
narrative_ontology:cs_axiom_status(decree_sufficiency_over_internalization, holdable).
narrative_ontology:cs_axiom_grounding('095d4083-1ed9-4f40-838b-e7fd0992f23b', decree_sufficiency_over_internalization, deontological).
narrative_ontology:cs_axiom('095d4083-1ed9-4f40-838b-e7fd0992f23b', foundational, state_sovereign_authority_derives_from_power_not_consent).
narrative_ontology:cs_axiom_status(state_sovereign_authority_derives_from_power_not_consent, holdable).
narrative_ontology:cs_axiom_grounding('095d4083-1ed9-4f40-838b-e7fd0992f23b', state_sovereign_authority_derives_from_power_not_consent, deontological).
narrative_ontology:cs_reference_frame('095d4083-1ed9-4f40-838b-e7fd0992f23b', exogenous_state_authority_primacy).
narrative_ontology:cs_drift_state('095d4083-1ed9-4f40-838b-e7fd0992f23b', contemporary_post_suppression_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('095d4083-1ed9-4f40-838b-e7fd0992f23b', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, tradition_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts decrees (abolishing customary calendars, imposing dress codes, centralizing legal authority) to standardize the population and establish state sovereignty over all domains of social practice. Justifies these decrees as modernization, integration, and progress. The apparatus includes the legislative body, executive enforcement, courts, and local administrators who implement the mandate. Benefits accrue through increased state capacity to extract resources, conscript population, and claim legitimate authority over all subjects regardless of traditional practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the costs of displacement: abandoning inherited calendars (disrupting seasonal planting, harvest, ritual cycles), changing dress (economic cost of new materials, loss of identity markers, women's labor increases with new garment demands), submitting to state legal authority (loss of customary dispute resolution, reduced autonomy in family and property matters). Exit from these territories is not practically available. Compliance is coerced through fines, punishment, and administrative pressure rather than internalized belief.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, national).

% Elders, ritual specialists, and customary authorities who hold and transmit inherited practices. The decree directly nullifies their authority and knowledge systems. They face personal loss of status, but also carry responsibility for maintaining practices in the face of state prohibition—a role that becomes criminalized. Their institutional identity (as keepers of tradition) makes exit impossible; resistance is their structural position.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, tradition_bearers, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, tradition_bearers, excluded).

% Clerks, judges, police, and functionaries who implement the decrees. They benefit through employment, career advancement, and increased authority over rural subjects. They experience the decree as legitimate state power, not coercion. The decree expands their institutional power and legitimizes their social standing relative to rural populations.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class, beneficiary,
    powerful, biographical, mobile, national).

% Religious authorities, customary councils, regional nobility, or rival state apparatus that previously held legitimate authority over practice in their territories. The decree explicitly displaces these authorities in favor of centralized state law. They are structurally barred from competing for legitimacy; their exclusion is enforced through the same decree machinery that enforces rural compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, alternative_authority_systems, excluded,
    powerful, generational, trapped, regional).

% Historians, anthropologists, political theorists, and international bodies that assess the decree's operation: whether it actually displaces practice or merely produces performative compliance, whether internalization follows or remains absent, whether the constraint's persistence depends on active enforcement or becomes normalized.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, observing_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, state-legible system of temporal/dress/legal organization across heterogeneous territories: unified calendar enables tax collection and conscription synchronization; standardized dress codifies status within state hierarchy; centralized law makes property and family disputes adjudicable by state authority rather than rival powers. The coordination solves the state's problem of territorial administration and unified legitimacy claims.
% TRANSFER_FUNCTION: Transfers authority from local/customary systems to state apparatus, and transfers the adjustment costs (economic, psychological, identity loss) from the state modernization agenda onto rural populations and tradition-bearers who do not share the modernization commitment. The material movement is backward: resources flow from rural subjects toward the state (through fines, labor reorganization, conscription enabled by standardized identity).
% ABSENT_VOICES: Alternative authority systems that previously held legitimacy are structurally excluded and criminalized. Tradition-bearers' institutional voices are nullified by the decree itself. Had rural populations been consulted, they would object to the identity costs and adjustment burdens; their non-participation in the decision reflects not agreement but their powerlessness at the point the decree was enacted.
% DISAPPEARANCE_RATIONALE: If the decree suddenly lost enforcement, rural populations would immediately re-adopt customary practices (calendars, dress, dispute resolution)—they have been performing compliance under coercion, not internalizing the imposed norms. The state apparatus would lose the uniform temporal/identity infrastructure it built the decree to establish. Urban administrators would lose the authority delegated to them under the decree. The constraint's disappearance would force either renegotiation of state authority or return to pre-decree fragmentation.
% FOUNDING_PROBLEM: The state claims it was built to solve: establishing legitimate unified authority over heterogeneous territories, creating a modern nation-state with standardized subjects legible to central administration, eliminating rival authority structures that compete with state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and urban administrative class attest the founding problem is live and ongoing—rival authorities still exist, rural territories resist centralization, uniform legibility remains incomplete. Rural populations and tradition-bearers attest that the 'problem' was manufactured to serve state extraction; that heterogeneity did not prevent functioning societies; that the founding problem is a pretext for authority consolidation. International scholars and historians not embedded in the state apparatus document that many territories achieved administrative effectiveness without imposing dress and calendar uniformity, suggesting the stated founding problem admits alternative solutions.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The exogenous override reading is claimed because the state apparatus advances decree authority as self-sufficient: 'legal mandate establishes obligation; internalization follows enforcement, not consent.' The authored metrics reveal the structural cost of this claim. Suppression (0.81) is high because the constraint persists through active enforcement machinery—fines, punishment, administrative pressure—not through accepted legitimacy. Theater_ratio (0.42) tracks the gap: rural populations perform compliance in public while maintaining customary practice in hidden spaces (private calendars, underground rituals, family dress at home). This performative compliance is not internalization; it is coerced spectacle. Extractiveness (0.78) reflects the asymmetric cost distribution: the state extracts authority and coordination benefit; rural populations absorb identity loss, economic disruption, and the psychological cost of practicing one norm publicly while holding another privately. Resistance (0.72) remains substantial over the 40-year interval, declining modestly (from 0.68 to 0.55 individually) but stabilizing at organizational and class levels (0.79→0.62 organizational; 0.72→0.58 class), indicating that suppression intensity prevents active rebellion but not passive non-compliance. Accessibility_collapse is moderate (0.48 at start, 0.48 measured at end across all levels) because alternatives—customary practice—remain cognitively available and practically exercised in hidden spaces; the collapse is structural and legal, not perceptual. The reading hinges on the claim that decree suffices; the metrics show that decree requires increasingly intensive enforcement to maintain performative compliance, suggesting that exogenous override works operationally only through sustained suppression, not through self-sustaining legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and urban administrators compute the constraint as legitimate coordination: a unified nation-state, modern administration, integrated subjects. From their seats, the decree is authority, and compliance is obligation. The constraint persists because power concentrates in the state and rural populations cannot exit. Rural populations compute the constraint as coerced displacement: their authority is nullified, their practices criminalized, their adjustment costs are uncompensated. Tradition-bearers compute it as institutional foreclosure: their role is eliminated, their knowledge is delegitimized. The engine's per-seat computation reveals this divergence: an institutional agenda-setter with arbitrage exit will compute the constraint as protective coordination; a powerless, trapped subject will compute it as snare-flavored extraction. The divergence is not a measurement error—it is exactly what exogenous override produces: legitimacy for the top, coercion for the bottom.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: beneficiary, institutional power, arbitrage exit → d ≈ 0.15 (full beneficiary end). Rural populations: victims, powerless, trapped exit → d ≈ 0.92 (full target end). Tradition-bearers: victims, moderate power but identity-locked exit → d ≈ 0.88 (near-target). Urban administrators: beneficiaries, powerful, mobile → d ≈ 0.22. The directionality gradient (0.15 to 0.92) is extreme, reflecting the core structural asymmetry of exogenous override: the imposers benefit and can exit the system entirely; the imposed-upon bear costs and cannot exit. This gradient creates an effective extraction χ that compounds extractiveness with directionality amplification: low d on the beneficiary end damps χ further downward; high d on the victim end amplifies χ toward the maximum. The engine's χ computation on these d values will show that the same ε (0.78) produces radically different effective extraction experienced by different seats—a key feature of exogenous override constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing state legitimacy and unified authority) is classified as 'contested' because the state and rural populations disagree on whether it is a genuine problem or an imposed pretext. The state attests it is live; tradition-bearers and rural populations attest it is manufactured to justify authority consolidation. This is not mandatrophy in the sense of the founding function becoming obsolete (the state still needs to coordinate subjects). Rather, it is the contestation of whether the founding problem justified the decree in the first place. The disappearance_verdict (world_rearranges) indicates that the constraint's operation is not natural law—it is enforced arrangement dependent on active state power. If enforcement vanished, rural populations would re-adopt customary practice immediately, indicating that the decree has not created self-sustaining internalized compliance. The theater_ratio progression (0.28→0.42) reflects increasing performative maintenance: the state must invest more in theatrical displays of compliance (public ceremonies, administrative rituals, propaganda messaging) because the base compliance is not internalized and must be continuously restaged. This is piton-adjacent: the function (unified legible administration) persists because state power is concentrated, but the constraint is increasingly maintained through performance rather than belief or structural necessity. The constraint is not mandatrophy-resolved because the state still extracts benefit and continues enforcement; it is instead mandatrophy-adjacent: the decree claimed to solve a contested foundational problem; the solution works operationally through coercion; the costs of maintaining the coercion are rising (theater_ratio increases); future resolution would require either accepting the contested problem as real (bottom-up belief change) or abandoning the decree (state power loss).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_performative_compliance,
    'Does the high suppression requirement (0.81) and rising theater_ratio (0.28→0.42) indicate that rural populations have internalized the imposed norms, or are they performing compliance under coercion while maintaining private practice?',
    'Post-enforcement ethnography: if suppression were suddenly withdrawn, what practices would rural populations re-adopt immediately (indicating non-internalization) vs. what would persist (indicating internalization)? Family interviews, ritual practices in private spaces, oral history of compliance vs. belief.',
    'If performative, the exogenous override reading is true but operationally unsustainable—it requires permanent enforcement apparatus. If internalized, the reading is true and self-sustaining. The distinction divides the constraint''s future stability: performative compliance can degrade rapidly when suppression decays; internalized compliance persists even without enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalization_vs_performative_compliance, empirical, 'Whether observed compliance reflects genuine belief change or coerced performance.').

omega_variable(
    competing_readings_of_state_authority,
    'Does state decree authority derive from sovereign power (the exogenous override claim), or does legitimate authority require some degree of internalization/consent by subjects (the endogenous climb counter-claim)?',
    'This is a conceptual/normative question without empirical resolution. It depends on the framework: positive law (decree is self-legitimating) vs. social contract (authority requires consent) vs. legitimacy-as-belief (authority requires internalized acceptance). Different institutional seats endorse different framings.',
    'If decree alone is legitimating, exogenous override is the right framing; the state need only maintain enforcement. If internalization is required for legitimacy, the reading mischaracterizes the constraint—it should be reclassified as snare (pure coercion without legitimacy claim) or piton (claims legitimacy but operates through performance). This is the core committer contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_readings_of_state_authority, conceptual, 'Whether state decree authority is self-legitimating or requires internalization for true legitimacy.').

omega_variable(
    structural_necessity_of_uniformity,
    'Is uniform calendar/dress/legal authority structurally necessary for effective state administration, or do alternative institutional designs (federalism, multi-legal systems, cultural pluralism) achieve equivalent administrative capacity?',
    'Comparative institutional analysis: examine states that achieved comparable administrative capacity with heterogeneous systems (Ottoman millet system, modern federal pluralism) vs. states that enforced uniformity. Did the uniform approach provide more capacity or just different extractive capacity?',
    'If uniformity is necessary, the state''s founding problem is genuine; the costs to rural populations are coordination costs unavoidable in large-scale state formation. If alternative designs exist, uniformity is a choice to extract authority and legitimacy, not a structural necessity—the constraint becomes pure extraction rather than coordination-plus-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_necessity_of_uniformity, empirical, 'Whether uniform practice is structurally required for state administration or a choice enabling extraction.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.81) structural (external coercive apparatus, legal penalties, administrative pressure) or internalized (subjects have accepted the state''s authority to dictate practice)?',
    'Ethnographic and interview analysis distinguishing external enforcement (police, fines, public punishment) from internalized self-enforcement (subjects avoid banned practice because they believe it is wrong, not because they fear punishment). Track which forms of suppression are present at different social levels.',
    'If primarily structural, the constraint is dependent on the continuous maintenance of the enforcement apparatus and will degrade if enforcement capacity declines. If substantially internalized, the constraint becomes self-sustaining (subjects enforce it on themselves even without external pressure). The stability of the constraint over generations depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized belief.').

omega_variable(
    hidden_practice_persistence,
    'The theater_ratio progression (0.28→0.42) suggests increasing performative maintenance; does this indicate that rural populations continue banned practices in hidden spaces (family, private ritual), contradicting the claim of actual practice displacement?',
    'Ethnographic documentation of private vs. public practice; oral histories of how families maintained customary practice despite bans; administrative records of enforcement actions targeting hidden practice.',
    'If hidden practice persists substantially, the decree has displaced PUBLIC practice legible to the state, but not actual practice—exogenous override has achieved administrative legibility, not cultural displacement. This would mean the constraint''s actual function is extracting state authority over legible subjects, not displacing practice itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hidden_practice_persistence, empirical, 'Whether banned practices persist in hidden spaces despite public compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(legi_tr_t25, observed).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(legi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(legi_be_t25, observed).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(legi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 5, 0.77).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 15, 0.8).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement_basis(legi_su_t25, observed).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(legi_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(legi_grid_01, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(class), 0, 0.42).
narrative_ontology:measurement(legi_grid_02, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(class), 40, 0.52).
narrative_ontology:measurement(legi_grid_03, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(individual), 0, 0.25).
narrative_ontology:measurement(legi_grid_04, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(individual), 40, 0.38).
narrative_ontology:measurement(legi_grid_05, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(legi_grid_06, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(legi_grid_07, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(legi_grid_08, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(structural), 40, 0.78).
narrative_ontology:measurement(legi_grid_09, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(legi_grid_10, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(class), 40, 0.58).
narrative_ontology:measurement(legi_grid_11, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(legi_grid_12, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(individual), 40, 0.55).
narrative_ontology:measurement(legi_grid_13, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(organizational), 0, 0.79).
narrative_ontology:measurement(legi_grid_14, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(organizational), 40, 0.62).
narrative_ontology:measurement(legi_grid_15, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(structural), 0, 0.64).
narrative_ontology:measurement(legi_grid_16, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(structural), 40, 0.48).
narrative_ontology:measurement(legi_grid_17, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(legi_grid_18, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(class), 40, 0.71).
narrative_ontology:measurement(legi_grid_19, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(legi_grid_20, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(individual), 40, 0.62).
narrative_ontology:measurement(legi_grid_21, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(legi_grid_22, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(organizational), 40, 0.85).
narrative_ontology:measurement(legi_grid_23, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(structural), 0, 0.82).
narrative_ontology:measurement(legi_grid_24, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(structural), 40, 0.88).
narrative_ontology:measurement(legi_grid_25, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(class), 0, 0.71).
narrative_ontology:measurement(legi_grid_26, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(class), 40, 0.8).
narrative_ontology:measurement(legi_grid_27, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(individual), 0, 0.62).
narrative_ontology:measurement(legi_grid_28, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(individual), 40, 0.74).
narrative_ontology:measurement(legi_grid_29, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(organizational), 0, 0.81).
narrative_ontology:measurement(legi_grid_30, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(organizational), 40, 0.87).
narrative_ontology:measurement(legi_grid_31, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(structural), 0, 0.85).
narrative_ontology:measurement(legi_grid_32, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(structural), 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of a single contested kernel: 'legitimacy_of_imposed_practice'. The exogenous_override_reading claims decree authority is sufficient to displace practice; the endogenous_climb_reading claims internalization is necessary; the hybrid_scaffolding_reading claims scaffolded messaging can generate quasi-endogenous adoption. These readings have different ε values, different beneficiary/victim structures, and different classifications because they represent genuinely different claims about the same institutional arrangement. The kernel is contested—the state apparatus and rural populations hold incompatible readings of whether the same decree is legitimate authority or coerced extraction. Decomposition into separate constraint stories enables each reading to carry its own ε, beneficiary structure, and per-seat classification, rather than attempting to average or merge incompatible interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
