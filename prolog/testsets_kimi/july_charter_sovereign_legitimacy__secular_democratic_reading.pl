% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter Secular Democratic Sovereignty (Secular Reading)
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   Post-revolutionary charter establishes secular democratic institutions
 *   and military subordination to civilian authority. This constraint
 *   instantiates the secular_democratic_reading of the contested kernel
 *   july_charter_sovereign_legitimacy. The reading treats the charter as
 *   mandating a secular popular-sovereignty framework that structurally
 *   excludes political Islam actors (notably Jamaat-e-Islami) from
 *   constitutional legitimacy and subordinates the military establishment to
 *   elected civilian control. The constraint solves a genuine
 *   post-revolutionary coordination problemâpreventing succession crises
 *   and military dictatorshipâwhile asymmetrically extracting political
 *   authority from identifiable victims.
 *
 * KEY AGENTS:
 *   - secular_civilian_government: Primary agenda setter and beneficiary (institutional power, constrained exit) â enforces secular democratic order through state institutions and derives governing authority from the charter.
 *   - jamaat_e_islami: Primary target (organized power, identity-locked exit) â Islamist political actor excluded from sovereignty discourse; cannot fully participate without abandoning core political theology.
 *   - military_establishment: Secondary target (powerful, constrained exit) â constitutionally subordinated institution that loses autonomous political prerogatives and institutional autonomy.
 *   - democratic_constituency: Beneficiary (moderate, mobile exit) â secular democratic supporters who gain representative institutions and electoral channels.
 *   - political_islam_excluded: Excluded voice (moderate, trapped exit) â grassroots Islamist actors barred from constitutional design processes.
 *   - international_democratic_community: Analytical observer (institutional, analytical exit) â external monitors who condition recognition and aid on democratic compliance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.8).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter Secular Democratic Sovereignty (Secular Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'd972ae66-10b9-43be-aab6-41b36d55fe87').
narrative_ontology:cs_kernel_codification('d972ae66-10b9-43be-aab6-41b36d55fe87', fixed_text).
narrative_ontology:cs_authority_grounding('d972ae66-10b9-43be-aab6-41b36d55fe87', lineage).
narrative_ontology:cs_interpretation_layer_present('d972ae66-10b9-43be-aab6-41b36d55fe87').
narrative_ontology:cs_reading_relation('d972ae66-10b9-43be-aab6-41b36d55fe87', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('d972ae66-10b9-43be-aab6-41b36d55fe87', july_charter_sovereign_legitimacy__military_custodian_reading, influences).
narrative_ontology:cs_axiom('d972ae66-10b9-43be-aab6-41b36d55fe87', foundational, popular_sovereignty_secular_ground).
narrative_ontology:cs_axiom_status(popular_sovereignty_secular_ground, holdable).
narrative_ontology:cs_axiom_grounding('d972ae66-10b9-43be-aab6-41b36d55fe87', popular_sovereignty_secular_ground, conventional).
narrative_ontology:cs_axiom('d972ae66-10b9-43be-aab6-41b36d55fe87', foundational, military_subordination_civilian_authority).
narrative_ontology:cs_axiom_status(military_subordination_civilian_authority, holdable).
narrative_ontology:cs_axiom_grounding('d972ae66-10b9-43be-aab6-41b36d55fe87', military_subordination_civilian_authority, conventional).
narrative_ontology:cs_reference_frame('d972ae66-10b9-43be-aab6-41b36d55fe87', popular_sovereignty_secular_constitutionalism).
narrative_ontology:cs_drift_state('d972ae66-10b9-43be-aab6-41b36d55fe87', post_transition_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d972ae66-10b9-43be-aab6-41b36d55fe87', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civilian_government).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, democratic_constituency).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order, controls the state apparatus, and enforces secular principles through courts, electoral commissions, and security policy. Derives governing authority from the charter's supremacy clause and is bound by its procedural constraints.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civilian_government, agenda_setter,
    institutional, generational, constrained, national).

% Islamist political party excluded from constitutional sovereignty discourse. Cannot participate in electoral politics or constitutional revision without abandoning its core political-theological commitments. Bears the cost of political marginalization, legal restrictions on religious mobilization, and exclusion from state resources.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, biographical, identity_locked, national).

% Constitutionally subordinated to civilian authority and legally barred from political intervention. Loses autonomous budgetary, appointment, and strategic prerogatives. Retains coercive capacity but bears the cost of reduced institutional autonomy and criminal liability for political action.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_establishment, payer,
    powerful, generational, constrained, national).

% Secular democratic supporters who gain representative institutions, electoral channels, and constitutional protections against military or theocratic rule. Benefits from orderly succession and civilian supremacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, democratic_constituency, beneficiary,
    moderate, biographical, mobile, national).

% Grassroots Islamist activists, civil society actors, and religious scholars completely excluded from constitutional design, amendment processes, and secular democratic deliberation. Would demand recognition of religious legitimacy grounds if present.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_excluded, excluded,
    moderate, biographical, trapped, national).

% Foreign governments, multilateral organizations, and international NGOs that monitor constitutional compliance, provide conditional assistance, and attest to the charter's democratic legitimacy from an external seat.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civilian_government).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves post-revolutionary authority vacuum by establishing a unified constitutional framework with clear succession rules, civilian supremacy, and secular democratic institutions that prevent immediate relapse into military dictatorship or theocratic civil conflict.
% TRANSFER_FUNCTION: Moves political authority, institutional autonomy, and constitutional legitimacy from Islamist political actors and the military establishment to secular civilian democratic institutions and their constituencies.
% ABSENT_VOICES: Political Islam actors beyond Jamaat-e-Islami (Salafist movements, Islamist intellectuals) and mid-rank military officers favoring autonomous institutional status are structurally excluded from constitutional deliberation; they would contest the secular and subordination clauses.
% DISAPPEARANCE_RATIONALE: If the secular democratic charter and its enforcement vanished overnight, the military would likely assert sovereign guardianship, political Islam would mobilize for constitutional recognition of religious legitimacy grounds, and the civilian democratic architecture would collapse into open contestation between these camps.
% FOUNDING_PROBLEM: Post-revolutionary absence of legitimate authority with high risk of military coup, civil war, or authoritarian restoration; need for a constitutional basis to transition from revolutionary rupture to representative governance.
% FOUNDING_PROBLEM_CORROBORATION: International constitutional advisors and human rights organizations corroborate the need for civilian democratic authority. Islamist political theorists and military historians dispute that the problem required secular exclusion, arguing the revolution itself established legitimacy and the charter now serves as partisan consolidation. No consensus corroboration exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the systematic transfer of political authority and legitimacy claims from Islamist actors and the military to secular civilian institutions. Suppression (0.80) is high because the charter's persistence requires active constitutional enforcement against military coup temptations and Islamist mobilization challenges. Theater ratio (0.45) acknowledges that military subordination may be partly performative, with informal influence persisting behind formal compliance. Accessibility collapse (0.60) is moderate: alternatives (Islamist governance, military custodianship) remain visible in political discourse but are structurally barred by constitutional architecture. Resistance (0.70) is substantial from both military and Islamist camps. Temporal measurements share a single grid (T=0,2,5,7,10) showing extraction, theater, and enforcement hardening during consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The secular civilian government seat experiences the charter as necessary coordination that prevents dictatorship and civil war. The Jamaat and military seats experience the identical arrangement as extraction of their legitimate political and institutional roles. The engine computes this divergence from structural data: beneficiaries and agenda setters hold institutional power with constrained but authoritative exit; victims hold organized or powerful status but face identity-locked or constrained exit that amplifies effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular civilian government is the structural beneficiary and agenda setter (d near 0.0). Democratic constituency is an incidental beneficiary with relatively low extraction (d near 0.2). Jamaat-e-Islami is a full target due to identity-locked exclusion from the political sphere (d near 0.9). Military establishment is a strong target due to constrained loss of institutional autonomy despite retaining coercive capacity (d near 0.8). No directionality overrides are required because beneficiary and victim declarations, combined with exit modulations, accurately capture these structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification as tangled rope prevents both errors: mislabeling the constraint as pure extraction ignores the genuine coordination function (establishing post-revolutionary authority, preventing military dictatorship, creating succession rules). Mislabeling it as pure coordination ignores the asymmetric victimhood of Islamist actors and the subordinated military. The founding problemâpost-revolutionary vacuumâis contested, and the constraint persists beyond its initial transitional justification, but it has not yet atrophied into pure theater. Temporal measurements show rising extraction consistent with consolidation entangling coordination with power consolidation, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_kernel_reading_contestation,
    'Is the secular democratic reading a faithful interpretation of the charter text, or a factional imposition that constrains alternative sovereignty claims?',
    'Textual analysis of drafting records and comparative constitutional history of post-revolutionary charters.',
    'If the text is genuinely ambiguous, this constraint is one structurally distinct reading among several; if the text explicitly mandates secularism, sibling readings are misreadings rather than distinct constraints, and this constraint''s extraction is the price of textual fidelity rather than partisan consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_kernel_reading_contestation, conceptual, 'Whether the secular reading is textually compelled or politically imposed.').

omega_variable(
    military_subordination_genuine_or_theatrical,
    'Does military constitutional subordination reflect genuine institutional transformation, or theatrical compliance masking continued autonomous influence?',
    'Analysis of budget autonomy, appointment procedures, and crisis intervention patterns.',
    'If theatrical, theater_ratio should rise and the constraint drifts toward piton; if genuine, extraction from military autonomy is real and the constraint remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_subordination_genuine_or_theatrical, empirical, 'Whether military subordination is functional or performative.').

omega_variable(
    political_islam_exclusion_necessity,
    'Is the exclusion of political Islam actors necessary for democratic consolidation, or asymmetric extraction consolidating secular power?',
    'Comparative analysis of democratic transitions with and without Islamist inclusion.',
    'If necessary, the constraint''s extraction is coordination cost; if consolidation, the extraction is rent-seeking by secular elites and victimhood is intensified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_islam_exclusion_necessity, preference, 'Whether Islamist exclusion is structural necessity or power consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t2, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(july_tr_t7, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 7, 0.4).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(july_be_t2, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(july_be_t7, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 7, 0.62).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(july_su_t2, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 2, 0.6).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(july_su_t7, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 7, 0.76).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, military_custodian_reading).

% DUAL FORMULATION NOTE:
% The kernel july_charter_sovereign_legitimacy decomposes into three structurally distinct constraints because the charter text supports incompatible legitimacy claims with different beneficiary/victim structures. Each reading carries its own epsilon and its own structural classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
