% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Treaty Minority Protections (Expansive Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Lausanne Treaty (1923) contains provisions protecting non-Muslim
 *   minorities' religious rights. The expansive reading interprets these
 *   protections as guaranteeing institutional continuity: minority religious
 *   institutions retain self-governance, property rights, and clergy
 *   formation via theological education, modeled on pre-1923 Ottoman millet
 *   structures. This reading is contested by a restrictive reading
 *   (protections extend only to individual worship) and a guarantor reading
 *   (protections are international obligations supervised by external
 *   guarantor powers). The expansive reading faces a structural
 *   vulnerability: it depends on interpretive contest remaining open. If the
 *   restrictive reading wins, the constraint's legal foundation erodes. This
 *   is not extraction from identified beneficiaries but institutional
 *   precariousness rooted in contested kernel interpretation.
 *
 * KEY AGENTS:
 *   - Orthodox Patriarchate (Istanbul): Institutional agenda-setter claiming self-governance under the expansive reading; operationally trapped because Turkish state approval is still required despite the reading's assertions of autonomy.
 *   - Turkish State: Institutional holder of sovereignty; applies general secular law that often conflicts with the expansive reading; leans toward restrictive interpretation in practice.
 *   - Minority religious communities (Armenian, Evangelical, Jewish): Beneficiaries of the expansive reading's assertion of institutional autonomy; materially vulnerable if the reading is displaced.
 *   - European human rights bodies (ECHR, Council of Europe): Observers reinforcing the expansive reading through case law, giving it continental-level legitimacy while Turkish domestic courts apply narrower readings.
 *   - Secular Turkish nationalism: Excluded from formal Lausanne negotiations but shapes political pressure against the expansive reading's assertion of separate institutional governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.38).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.52).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Treaty Minority Protections (Expansive Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '77912548-91a2-4129-9a37-12237572fb96').
narrative_ontology:cs_kernel_codification('77912548-91a2-4129-9a37-12237572fb96', fixed_text).
narrative_ontology:cs_authority_grounding('77912548-91a2-4129-9a37-12237572fb96', lineage).
narrative_ontology:cs_interpretation_layer_present('77912548-91a2-4129-9a37-12237572fb96').
narrative_ontology:cs_reading_relation('77912548-91a2-4129-9a37-12237572fb96', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('77912548-91a2-4129-9a37-12237572fb96', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('77912548-91a2-4129-9a37-12237572fb96', foundational, pre_1923_institutional_structures_are_continuous).
narrative_ontology:cs_axiom_status(pre_1923_institutional_structures_are_continuous, holdable).
narrative_ontology:cs_axiom_grounding('77912548-91a2-4129-9a37-12237572fb96', pre_1923_institutional_structures_are_continuous, deontological).
narrative_ontology:cs_axiom('77912548-91a2-4129-9a37-12237572fb96', foundational, minority_religious_autonomy_is_legally_separable_from_citizenship).
narrative_ontology:cs_axiom_status(minority_religious_autonomy_is_legally_separable_from_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('77912548-91a2-4129-9a37-12237572fb96', minority_religious_autonomy_is_legally_separable_from_citizenship, deontological).
narrative_ontology:cs_reference_frame('77912548-91a2-4129-9a37-12237572fb96', ottoman_millet_institutional_continuity).
narrative_ontology:cs_drift_state('77912548-91a2-4129-9a37-12237572fb96', contemporary_secular_nation_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77912548-91a2-4129-9a37-12237572fb96', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, orthodox_patriarchate_istanbul).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, evangelical_protestant_minorities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_apostolic_minorities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, jewish_minorities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint does not collect rents from the beneficiaries — it asserts their institutional autonomy. The asymmetry is not economic but structural: the expansive reading depends on treaty interpretation remaining contested in its favor. If the restrictive reading wins the interpretive contest, the beneficiaries lose institutional foundations. Suppression rises over the interval (0.35 → 0.52) as secular Turkish state pressure against institutional autonomy increases, particularly post-2000s (Kurdish conflict politicization, religious education restrictions, property disputes). Theater ratio rises (0.18 → 0.41) because an increasing share of 'protection' takes the form of diplomatic statements, European court cases, and international pressure, while domestic operational autonomy remains constrained. The measurement series tracks the constraint's gradual conversion from active coordination into performative protection.
 *
 * PERSPECTIVAL GAP:
 *   From the Patriarchate's seat, the expansive reading is functional continuity of pre-1923 self-governance; Turkish state compliance is treaty-bound. From the Turkish state's seat, the same reading is an international intrusion into domestic law-making; the state interprets Lausanne narrowly and applies general law. From minority community seats, the reading is existential — it is the legal foundation for institutional survival; if it loses, they face assimilation into secular law with no protected status. From the European observer's seat, the reading is human rights enforcement; the ECHR applies it to check Turkish state power. The engine computes these divergences from power atoms and exit options: the Patriarchate (moderate power, trapped exit, asserting institutional agenda-setting) diverges from the Turkish state (institutional power, constrained by international law but still sovereign), which diverges from powerless minority communities (trapped, dependent on treaty interpretation). These structural differences should produce divergent computed types.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (Patriarchate, minority religious institutions) have low d values because they benefit from the expansive reading without extracting from others — the reading asserts their institutional autonomy, not their collection of rents. The Turkish state sits near d=0.5 (symmetric): it bears costs from international supervision and legal constraint but also benefits from the reading's legitimacy (proves minority protections are in place). Excluded agents (secular Turkish nationalists) have high d values because the constraint limits their preferred policy (uniform secular law applied to all institutions). The key directionality insight: this is a constraint whose asymmetry is interpretive (beneficiaries vulnerable to reading loss), not economic (no one is extracting rents), so d values should reflect interpretive dominance and institutional fragility, not collection of wealth.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (continuity of minority institutional structures post-Ottoman transition) sits in a critical zone. The expansive reading asserts the problem remains live (institutions still need protection). The restrictive reading asserts it is solved (individual worship rights are protected; institutional status is secondary). If the restrictive reading wins the interpretive contest, the founding problem dissolves in the restrictive framework — but the expansive reading will have lost its mandate. This is not mandatrophy in the classical sense (function atrophied, constraint persists theatrically) but rather mandate death through interpretive loss. The measurements track this risk: rising suppression and theater ratio suggest the constraint is gradually losing functional autonomy and converting to diplomatic performance. A mandatrophy verdict would require the restrictive reading to win while the expansive reading persists in some contexts — a possible future state, not the current one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_outcome,
    'Which reading of Lausanne minority protections will achieve stable interpretive dominance: expansive (institutional autonomy), restrictive (individual worship only), or guarantor (international supervision)?',
    'Sequence of high-court decisions (Turkish Constitutional Court, ECHR, Council of Europe rulings) establishing consistent interpretation; or formal treaty amendment/clarification by signatory powers; or long-term observational record of which reading shapes actual institutional practice.',
    'If expansive reading achieves dominance, minority institutions gain legal security and the constraint''s classification remains rope (coordination with beneficiary protection). If restrictive reading dominates, the expansive reading loses its mandate; minority institutions lose treaty-based autonomy and the constraint dissolves. If guarantor reading dominates, supervisory mechanisms strengthen, converting the constraint toward snare-like extraction (minority institutions become subjects of international oversight). The outcome directly determines whether the expansive reading survives as a constraint at all.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_outcome, conceptual, 'Which interpretive reading of the Lausanne kernel will achieve stable dominance.').

omega_variable(
    ottoman_millet_continuity_assumption,
    'Is institutional religious governance continuity from the Ottoman millet system actually achievable under a modern secular nation-state, or is the expansive reading rooted in a nostalgia for institutional structures that are fundamentally incompatible with secular law?',
    'Comparative analysis of other post-imperial secular states with minority religious institutions (India, Indonesia, Malaysia, Lebanon) to assess whether institutionalized religious governance is compatible with secular legal frameworks. Or long-term observational record of whether minority institutions actually achieve self-governance under the expansive reading''s framework.',
    'If continuity is achievable, the expansive reading is structurally sound and the beneficiaries can maintain institutional autonomy. If incompatible, the reading is aspirational but not functionally realizable; minority institutions are bound to gradually lose autonomy regardless of the reading''s legal claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ottoman_millet_continuity_assumption, conceptual, 'Whether pre-1923 Ottoman millet institutional continuity is achievable under secular nation-state law.').

omega_variable(
    european_court_leverage,
    'How much structural leverage does European human rights jurisprudence actually exercise over Turkish state behavior regarding minority religious institutions? Is ECHR case law a meaningful constraint on the state, or a symbolic backing for a reading that the state disregards domestically?',
    'Comparative analysis of ECHR rulings on Turkish minority religious cases and actual Turkish government compliance; assessment of whether ECHR decisions actually change institutional access, property rights, or clergy training practices; or diplomatic correspondence revealing the state''s actual regard for European rulings.',
    'If European leverage is substantial, the expansive reading gains material enforcement power beyond the treaty text itself. If purely symbolic, the reading''s beneficiaries lack real protection and face gradual attrition despite the favorable European legal frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_court_leverage, empirical, 'Whether European human rights mechanisms provide substantive constraint on Turkish state behavior toward minority religious institutions.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.52) primarily structural (state legal barriers, property restrictions, education licensing) or internalized (minority institutions'' self-censorship, identity fusion with Turkish citizenship, internalized expectations of limited autonomy)?',
    'Post-institutional-shift observation: if minority institutions gain autonomy (through reading victory, state policy change, or EU accession pressure) and suppression persists, reclassify as substantially internalized. Or ethnographic study of minority leaders'' self-perception of autonomy and constraints.',
    'If structural, the constraint''s suppression is removable through policy change or European pressure. If internalized, minority institutions carry the suppression with them even if formal barriers are removed; institutional recovery would require identity re-fusion, not just legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural or internalized within minority institutional consciousness.').

omega_variable(
    kernel_reading_as_distinct_constraint,
    'Is each reading (expansive, restrictive, guarantor) a distinct constraint with its own ε and structure, or are they all one constraint viewed through different interpretive lenses?',
    'ε-invariance test: if the same observable (e.g., Turkish state enforcement of Halki Seminary closure) is measured one way under the expansive reading (treaty violation) and another way under the restrictive reading (valid secular law enforcement), and these produce clearly different ε values, then two distinct constraints exist. If the ε remains the same regardless of reading frame, then one constraint has multiple readings, not multiple constraints.',
    'If distinct constraints, this story (expansive reading) and its sibling stories form a constraint family linked by network.affects_constraints — interpretive victory for one reading affects the others'' viability. If one constraint, the framework has collapsed into rhetorical interpretation without structural consequence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_as_distinct_constraint, conceptual, 'Whether contested kernel readings instantiate distinct constraints or are views of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__expansive_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__expansive_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__expansive_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__expansive_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.41).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__expansive_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__expansive_reading, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__expansive_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__expansive_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__expansive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__expansive_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(laus_su_t40, lausanne_minority_protections__expansive_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(laus_su_t60, lausanne_minority_protections__expansive_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(laus_su_t80, lausanne_minority_protections__expansive_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement(laus_su_t100, lausanne_minority_protections__expansive_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel is interpreted via three distinct constraint readings: expansive (institutional autonomy), restrictive (individual worship only), and guarantor (international supervision). Each reading instantiates a different constraint with different ε, beneficiary structure, and classification. The readings are not views of one constraint but three structurally distinct constraints rooted in contested kernel interpretation. Interpretive victory for any one reading affects the viability of the others: restrictive victory undermines the expansive reading's mandate; guarantor dominance converts both into supervisory mechanisms; expansive dominance legitimizes institutional autonomy. The three constraints form a family linked by interpretive dependency and mutual foreclosure risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
