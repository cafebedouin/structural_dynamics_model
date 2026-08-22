% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Norms — Liberal Institutional Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   The Rules-Based International Order (RBIO) as read through the liberal
 *   institutional lens presents itself as a universal, consent-based
 *   normative framework revisable through legitimate multilateral processes.
 *   This reading claims that enforcement selectivity — the observed pattern
 *   where powerful states intervene more freely while weaker states face
 *   sanctions — reflects capacity constraints and geopolitical complexity
 *   rather than structural illegitimacy. The constraint coordinates
 *   collective security through UNSC authorization and humanitarian
 *   intervention norms, but simultaneously extracts via economic
 *   conditionality (IMF/World Bank structural adjustment), sanctions regimes
 *   that harm civilian populations, and a security contractor economy that
 *   profits from intervention. Beneficiaries include intervening states
 *   (primarily P5 and their allies), their defense/security contractors,
 *   multilateral institutions that administer conditionality, and
 *   humanitarian NGOs that gain operational access. Victims include targeted
 *   states facing regime-change pressure, civilian populations bearing
 *   sanctions' humanitarian costs, and displaced populations from
 *   intervention zones. The constraint requires active enforcement (UNSC
 *   resolutions, sanctions committees, peacekeeping mandates, conditionality
 *   enforcement). The claimed_type is tangled_rope because the reading
 *   genuinely coordinates collective security (rope function) while
 *   asymmetrically extracting from targeted populations (snare function) —
 *   the engine will compute per-seat divergence.
 *
 * KEY AGENTS:
 *   - intervening_states: Primary beneficiary (institutional/arbitrage) — initiates and profits from interventions
 *   - security_contractors: Beneficiary (powerful/constrained) — captures intervention economy
 *   - multilateral_institutions: Agenda setter (institutional/generational) — administers norms and conditionality
 *   - humanitarian_ngos: Beneficiary (organized/mobile) — gains access and legitimacy
 *   - targeted_states: Primary victim (powerless/trapped) — bears intervention and sanctions
 *   - civilian_populations_under_sanctions: Victim (powerless/trapped) — bears humanitarian costs
 *   - displaced_populations: Victim (powerless/trapped) — bears intervention displacement
 *   - sovereignty_maximalist_states: Excluded (moderate/constrained) — rejects intervention legitimacy
 *   - global_south_coalition: Excluded (organized/constrained) — demands reform of selectivity
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.38).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.22).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Norms — Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '900bd059-28cf-4c3d-b909-4df064a5068a').
narrative_ontology:cs_kernel_codification('900bd059-28cf-4c3d-b909-4df064a5068a', formalized).
narrative_ontology:cs_authority_grounding('900bd059-28cf-4c3d-b909-4df064a5068a', lineage).
narrative_ontology:cs_interpretation_layer_present('900bd059-28cf-4c3d-b909-4df064a5068a').
narrative_ontology:cs_reading_relation('900bd059-28cf-4c3d-b909-4df064a5068a', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('900bd059-28cf-4c3d-b909-4df064a5068a', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('900bd059-28cf-4c3d-b909-4df064a5068a', foundational, multilateral_consent_basis).
narrative_ontology:cs_axiom_status(multilateral_consent_basis, holdable).
narrative_ontology:cs_axiom_grounding('900bd059-28cf-4c3d-b909-4df064a5068a', multilateral_consent_basis, conventional).
narrative_ontology:cs_axiom('900bd059-28cf-4c3d-b909-4df064a5068a', foundational, legitimate_process_revisability).
narrative_ontology:cs_axiom_status(legitimate_process_revisability, holdable).
narrative_ontology:cs_axiom_grounding('900bd059-28cf-4c3d-b909-4df064a5068a', legitimate_process_revisability, conventional).
narrative_ontology:cs_axiom('900bd059-28cf-4c3d-b909-4df064a5068a', secondary, selectivity_as_capacity_not_legitimacy).
narrative_ontology:cs_axiom_status(selectivity_as_capacity_not_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('900bd059-28cf-4c3d-b909-4df064a5068a', selectivity_as_capacity_not_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('900bd059-28cf-4c3d-b909-4df064a5068a', post_war_collective_security_framework).
narrative_ontology:cs_drift_state('900bd059-28cf-4c3d-b909-4df064a5068a', contemporary_selective_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('900bd059-28cf-4c3d-b909-4df064a5068a', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, security_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_ngos).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, displaced_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, security_contractors).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, responsibility_to_protect_norm).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate and authorize interventions through UNSC; gain strategic positioning, resource access, and regime-change leverage. Contractors from these states capture intervention economies. They hold veto power over enforcement decisions, giving them near-arbitrage exit from the constraint's costs while collecting its benefits.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary,
    institutional, generational, arbitrage, global).

% Capture the intervention economy: logistics, training, private security, reconstruction contracts. Their profits depend on intervention volume. They lobby for intervention authorization and influence doctrine development. Exit is constrained by dependency on state contracts and regulatory licenses.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, security_contractors, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, security_contractors, payer).

% Administer the RBIO architecture: UNSC, IMF, World Bank, ICC, peacekeeping operations. They set enforcement priorities, design conditionality packages, and legitimize interventions. Their institutional identity is fused with the RBIO — they cannot exit without ceasing to be what they are. They extract budget and legitimacy from the constraint while coordinating collective security.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, multilateral_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Gain operational access, funding, and legitimacy through RBIO frameworks (R2P, humanitarian corridors, UN partnership). They deliver genuine aid but their access depends on the constraint's enforcement architecture. They can redirect to other crises (mobile exit) but lose the RBIO's institutional platform.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_ngos, beneficiary,
    organized, biographical, mobile, global).

% Face regime-change pressure, sanctions, military intervention, and conditionality. They bear the costs of enforcement selectivity. Exit is trapped: no veto, no alternative security architecture, surrender means regime death. They resist through sovereignty claims, regional alliances, and asymmetric warfare.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    powerless, biographical, trapped, national).

% Bear the humanitarian costs of comprehensive sanctions: mortality, morbidity, malnutrition, healthcare collapse, educational disruption. They have no political voice in the sanctioning decisions, no exit from the territory, and no alternative survival structures. The constraint extracts their wellbeing as leverage on their governments.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, payer,
    powerless, immediate, trapped, local).

% Displaced by interventions and conflicts authorized under RBIO norms. Bear the costs of intervention (bombing, infrastructure destruction, sectarian violence unleashed). No exit from displacement camps, no political representation in authorizing bodies, no compensation mechanism.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, displaced_populations, payer,
    powerless, immediate, trapped, regional).

% Reject the legitimacy of humanitarian intervention and R2P as pretexts for regime change. They would object to the constraint's enforcement selectivity but are excluded from the agenda-setting process (no UNSC veto, limited influence in General Assembly). They build alternative frameworks (Shanghai Cooperation Organization, BRICS) but remain constrained by the dominant system.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_maximalist_states, excluded,
    moderate, generational, constrained, national).

% Collectively demand reform of UNSC, end to unilateral coercive measures, and equitable development governance. They articulate the selectivity-as-legitimacy-problem critique but lack structural power to change the constraint. Their exit is constrained by economic dependency on the RBIO's financial architecture (IMF, World Bank, dollar system).
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, global_south_coalition, excluded,
    organized, generational, constrained, global).

% Sees the full structure: the coordination function (collective security, humanitarian norms) and the extraction function (contractor economy, sanctions harm, conditionality) operating simultaneously. No material stake; the analytical seat computes the per-seat divergence the engine formalizes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal framework for collective security (UNSC authorization), humanitarian protection (R2P), and economic cooperation (multilateral trade/development institutions) — solving the problem of great-power conflict and mass atrocity through legitimate multilateral processes rather than unilateral force.
% TRANSFER_FUNCTION: Moves strategic leverage, contractor revenue, and institutional legitimacy from intervening states and their agents to targeted states and civilian populations via sanctions, conditionality, and military intervention. Moves humanitarian access and operational mandates to NGOs. Moves normative authority to multilateral institutions.
% ABSENT_VOICES: Civilian populations under sanctions and displaced populations have no voice in UNSC decisions. Global South states are structurally excluded from veto power. Future generations (who inherit the institutional architecture) are not represented. The sovereignty-maximalist and hegemonic-extraction readings' constituencies are present in discourse but excluded from the liberal institutional reading's internal legitimacy calculus.
% DISAPPEARANCE_RATIONALE: If the liberal institutional reading's constraint vanished overnight, the UNSC authorization regime would collapse, sanctions architectures would lose legitimacy, conditionality enforcement would cease, and the contractor intervention economy would lose its legal basis. Great-power relations would revert to raw power balancing; humanitarian intervention would lose its legal framework; the Global South would lose its primary institutional platform for development claims. The world would rearrange around either raw sovereignty (sovereignty_maximalist outcome) or explicit hegemonic management (hegemonic_extraction outcome).
% FOUNDING_PROBLEM: Prevent great-power war and provide collective security after WWII; establish universal norms for state conduct; create legitimate multilateral processes for norm revision and enforcement.
% FOUNDING_PROBLEM_CORROBORATION: UN Charter (1945) and Nuremberg Principles attest the founding problem. The great-power war prevention function is corroborated by the absence of direct P5 conflict (SIPRI, Correlates of War data). The contested status is corroborated by Non-Aligned Movement summits (1961-present), G77 declarations, UNGA resolution voting patterns on intervention/conditionality, and critical IR scholarship (Chomsky, Chomsky & Herman, Petras, Third World Approaches to International Law scholars) — all from outside the intervening states/contractor beneficiary set.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).
:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the asymmetric transfer: intervening states and contractors gain materially; targeted states and civilians lose. The coordination function (collective security, humanitarian protection) is real but partial — extraction is not merely incidental. Suppression (0.22) is moderate: the constraint does not rely on total coercion but on institutional legitimacy and the lack of viable alternatives for weaker states. Theater_ratio (0.31) captures the gap between the universalist rhetoric and the selective enforcement reality — the legitimacy performance exceeds functional delivery. Accessibility_collapse (0.45) is moderate: alternatives exist (regional security arrangements, non-aligned movement) but are structurally disadvantaged. Resistance (0.52) is significant: sovereignty-maximalist states, Global South coalitions, and critical scholars actively contest the constraint's legitimacy. The temporal series shows gradual extraction accumulation and theater creep from 1945 to 2024, with suppression stabilizing post-Cold War.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening states seat (institutional power, arbitrage exit), the constraint appears as genuine coordination — they built it, benefit from it, and can exit via veto power. From the targeted states seat (powerless, trapped), the same structure operates as enforced extraction with no meaningful exit. The civilian populations seat experiences pure extraction (sanctions mortality, displacement) with zero coordination benefit. The multilateral institutions seat (agenda_setter) experiences the constraint as both their mandate and their legitimacy source — they coordinate but also extract through conditionality. The engine computes this divergence from the structural data: power atoms, exit options, and beneficiary/victim declarations produce different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and security contractors are structural beneficiaries: they collect material gains (contracts, strategic position, resource access) and face near-arbitrage exit (veto power, alternative alliances). Multilateral institutions are agenda_setters with institutional power and generational horizon — they administer the constraint and extract legitimacy/budget from it. Humanitarian NGOs are beneficiaries with organized power and mobile exit — they gain operational access but could redirect to other crises. Targeted states, civilian populations under sanctions, and displaced populations are victims with powerless/trapped profiles — they bear costs with no exit. Sovereignty-maximalist states and Global South coalitions are excluded: they would object but are structurally locked out of the agenda-setting process. The analytical observer sees the full structure without material stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1945): prevent great-power war and provide collective security after WWII. Status: contested — the great-power war prevention function holds (no direct P5 conflict), but the collective security function has degraded into selective intervention. Corroboration: UN Charter preamble and Chapter VII attest the founding problem; Global South statements (Non-Aligned Movement, G77) and critical IR scholarship attest its contested status. The constraint shows mandatrophy signals: theater_ratio rising (0.12→0.31) while extractiveness rises (0.18→0.38), suggesting the coordination function is being hollowed out while extraction persists. The liberal institutional reading resists this diagnosis (capacity problem frame), while the hegemonic extraction reading treats it as confirmation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint one reading of the contested rbio_practice_norm_complex kernel, and does it instantiate the liberal_institutional_reading as distinct from the hegemonic_extraction_reading and sovereignty_maximalist_reading?',
    'Structural comparison of the three readings'' beneficiary/victim structures, claimed types, and cs_structure axioms. If the liberal institutional reading''s axioms (multilateral_consent_basis, legitimate_process_revisability) are mutually holdable with sibling axioms, they coexist; if they logically contradict, they foreclose.',
    'Confirms this story correctly isolates one reading per the ε-invariance principle. Misidentification would collapse distinct constraints into one, violating DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Commitment of this story to the liberal_institutional_reading of the rbio_practice_norm_complex kernel').

omega_variable(
    selectivity_capacity_vs_legitimacy,
    'Is enforcement selectivity genuinely a capacity problem (resource constraints, geopolitical complexity) rather than a legitimacy problem (systematic bias toward powerful states'' interests)?',
    'Longitudinal analysis of intervention authorization rates by UNSC permanent members vs. non-permanent members; correlation of intervention outcomes with intervening states'' material interests; independent commissions'' assessments of mandate implementation.',
    'If selectivity is a legitimacy problem, the constraint''s extractiveness is structurally higher than authored; the liberal institutional reading''s claimed_type (tangled_rope) would understate extraction and the reading would compute closer to the hegemonic_extraction_reading''s profile. If capacity, the current metrics hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_capacity_vs_legitimacy, empirical, 'Whether enforcement selectivity reflects capacity limits or extractive bias').

omega_variable(
    revisability_in_practice,
    'Are RBIO norms genuinely revisable through legitimate multilateral processes, or does P5 veto power and institutional path-dependency make revision practically impossible?',
    'Track record of successful norm revisions since 1945; failed revision attempts and their blockers; structural analysis of UN Charter amendment procedures vs. customary international law evolution.',
    'If revision is practically blocked, the constraint''s theater_ratio is understated (more performative than functional) and the claimed_type shifts toward piton or snare. If genuinely revisable, the current coordination function (rope-like) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisability_in_practice, empirical, 'Whether multilateral revisability is operational or theoretical').

omega_variable(
    civilian_harm_as_extraction,
    'Do sanctions'' civilian population impacts constitute extraction from targeted states'' populations, or are they acceptable collateral damage of legitimate pressure?',
    'Epidemiological studies of sanctions'' mortality/morbidity effects; comparison of sanction design (targeted vs. comprehensive) with humanitarian outcomes; legal analysis of proportionality under international humanitarian law.',
    'If civilian harm is structural extraction rather than collateral, the victim group ''civilian_populations_under_sanctions'' bears higher effective extraction, raising the constraint''s overall extractiveness and strengthening the snare/tangled_rope classification for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_harm_as_extraction, conceptual, 'Classification of sanctions'' civilian impacts as extraction vs. collateral damage').

omega_variable(
    contractor_beneficiary_capture,
    'Do security contractors and intervening states'' defense industries capture the intervention economy such that their benefit becomes the constraint''s driver rather than a byproduct?',
    'Financial tracking of contract flows in UN-authorized interventions; revolving door analysis between government decision-makers and contractor boards; lobbying expenditure correlation with intervention authorization.',
    'If contractor capture drives intervention decisions, the beneficiary structure shifts from ''incidental benefit'' to ''structural driver'' — the constraint becomes more snare-like for the intervening states seat and more extractive overall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contractor_beneficiary_capture, empirical, 'Whether security contractor interests drive intervention decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_liberal_inst_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(rbio_liberal_inst_tr_t1960, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(rbio_liberal_inst_tr_t1975, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(rbio_liberal_inst_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(rbio_liberal_inst_tr_t2005, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(rbio_liberal_inst_tr_t2015, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(rbio_liberal_inst_tr_t2024, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(rbio_liberal_inst_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(rbio_liberal_inst_be_t1960, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(rbio_liberal_inst_be_t1975, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(rbio_liberal_inst_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(rbio_liberal_inst_be_t2005, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(rbio_liberal_inst_be_t2015, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2015, 0.37).
narrative_ontology:measurement(rbio_liberal_inst_be_t2024, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(rbio_liberal_inst_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(rbio_liberal_inst_su_t1960, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1960, 0.18).
narrative_ontology:measurement(rbio_liberal_inst_su_t1975, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1975, 0.2).
narrative_ontology:measurement(rbio_liberal_inst_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(rbio_liberal_inst_su_t2005, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2005, 0.22).
narrative_ontology:measurement(rbio_liberal_inst_su_t2015, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2015, 0.22).
narrative_ontology:measurement(rbio_liberal_inst_su_t2024, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__liberal_institutional_reading, 0.12).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, imf_conditionality_regime).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, unsc_sanctions_architecture).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_intervention_doctrine).

% DUAL FORMULATION NOTE:
% This is the liberal_institutional_reading of the rbio_practice_norm_complex kernel. The hegemonic_extraction_reading treats the same normative complex as a frozen hegemonic project with extraction as primary function. The sovereignty_maximalist_reading treats it as illegitimate except when protecting sovereignty. All three readings share the same referent (the post-1945 institutional architecture) but author different ε, beneficiary/victim structures, and claimed_types. They are linked via affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, institutional, 0.15).
constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, powerless, 0.92).
constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, organized, 0.25).
constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, powerful, 0.18).
constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
