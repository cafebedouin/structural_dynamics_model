% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope Enforcement Vacuum
 *   domain: legal/intellectual-property/software-licensing
 *
 * SUMMARY:
 *   This constraint instantiates the enforcement_vacuum_reading of the
 *   gpl_copyleft_scope kernel. The GPL's copyleft scopeâspecifically
 *   whether and when dynamically linked, plugin-architecture, or aggregated
 *   code triggers the source-code disclosure obligationâhas never received
 *   definitive resolution in Anglo-American or EU high courts. The resulting
 *   vacuum means the effective constraint in any specific context depends on
 *   which interpretive community possesses enforcement capacity: FSF-aligned
 *   enforcers can threaten broad readings in friendly jurisdictions, while
 *   industry ecosystems can defend narrow readings through market power and
 *   legal defense funds. The uncertainty itself becomes a structural feature
 *   of the software-licensing landscape.
 *
 * KEY AGENTS:
 *   - Pragmatic adopters (powerful/mobile): Primary beneficiaryâexploit ambiguity to minimize compliance obligations.
 *   - Clarity-seeking adopters (moderate/constrained): Primary payerâbear transaction costs of risk assessment and defensive engineering.
 *   - FSF-aligned enforcers (organized/constrained): Agenda setterâmaintain strong copyleft reading through selective enforcement.
 *   - Industry-dominant ecosystems (institutional/constrained): Agenda setterâmaintain narrow reading through market practice and legal defense.
 *   - End-users (powerless/trapped): Excluded voiceâstructurally absent from licensing negotiations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.28).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope Enforcement Vacuum").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "legal/intellectual-property/software-licensing").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '43669f33-8483-4d13-b372-fdf8b0e76cf4').
narrative_ontology:cs_kernel_codification('43669f33-8483-4d13-b372-fdf8b0e76cf4', formalized).
narrative_ontology:cs_authority_grounding('43669f33-8483-4d13-b372-fdf8b0e76cf4', distributed).
narrative_ontology:cs_reading_relation('43669f33-8483-4d13-b372-fdf8b0e76cf4', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('43669f33-8483-4d13-b372-fdf8b0e76cf4', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('43669f33-8483-4d13-b372-fdf8b0e76cf4', foundational, scope_determined_by_enforcement_capacity).
narrative_ontology:cs_axiom_status(scope_determined_by_enforcement_capacity, holdable).
narrative_ontology:cs_axiom_grounding('43669f33-8483-4d13-b372-fdf8b0e76cf4', scope_determined_by_enforcement_capacity, empirically_contingent).
narrative_ontology:cs_axiom('43669f33-8483-4d13-b372-fdf8b0e76cf4', foundational, licensed_plurality_as_stable_equilibrium).
narrative_ontology:cs_axiom_status(licensed_plurality_as_stable_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('43669f33-8483-4d13-b372-fdf8b0e76cf4', licensed_plurality_as_stable_equilibrium, conventional).
narrative_ontology:cs_reference_frame('43669f33-8483-4d13-b372-fdf8b0e76cf4', enforcement_contingent_scope_equilibrium).
narrative_ontology:cs_drift_state('43669f33-8483-4d13-b372-fdf8b0e76cf4', contemporary_oss_licensing, gap(stable, minor, false)).
narrative_ontology:cs_created_at('43669f33-8483-4d13-b372-fdf8b0e76cf4', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commercial adopters and integrators who combine GPL components with proprietary systems, exploiting the scope ambiguity to minimize source disclosure obligations while maintaining multiple plausible compliance positions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    powerful, biographical, mobile, global).

% Risk-averse organizations and developers requiring unambiguous licensing boundaries. They bear elevated transaction costs through legal review, defensive engineering, dual-licensing negotiations, and compliance overhead to navigate unsettled copyleft scope.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Software Freedom Conservancy, FSF, and aligned projects that assert broad copyleft scope through enforcement actions and legal threats. Their enforcement capacity sustains the strong reading as a live constraint in specific jurisdictional and community contexts.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_enforcers, agenda_setter,
    organized, generational, constrained, global).

% Major technology companies and industry consortia that maintain narrow copyleft scope through market practice, proprietary integration patterns, and legal defense. Their economic capacity sustains the narrow reading as operational reality despite textual ambiguity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominant_ecosystems, agenda_setter,
    institutional, generational, constrained, global).

% End-users of software incorporating GPL components who lack standing to demand source code or clarity on their rights. They are structurally absent from licensing negotiations and would object to ambiguity if included.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, end_users, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits multiple interpretive communities to coexist and self-organize around divergent copyleft scope readings without a single judicial monopoly extinguishing pluralism; enables adaptive enforcement calibrated to community norms rather than rigid textual boundaries.
% TRANSFER_FUNCTION: Moves interpretive flexibility and reduced compliance obligation to pragmatic adopters; moves transaction costs, legal overhead, and risk-assessment burden to clarity-seeking adopters; moves enforcement leverage to whichever community has local capacity.
% ABSENT_VOICES: End-users who would want certainty about source-code availability rights; judicial authorities who have systematically declined to issue definitive copyleft-scope rulings; proprietary software advocates who would prefer judicial minimization of copyleft reach.
% DISAPPEARANCE_RATIONALE: If definitive judicial precedent resolved copyleft scope, interpretive pluralism would collapse. Pragmatic adopters would lose flexibility and face unambiguous compliance obligations; clarity-seekers would gain predictable rules but lose the premium value of legal certainty services; enforcement-dependent communities would lose threat advantage or gain definitive backing, fundamentally reorganizing open-source licensing strategy and compliance economics.
% FOUNDING_PROBLEM: Prevent proprietary appropriation of free software by ensuring derivative works remain under copyleft, while adapting to unanticipated technical coupling mechanisms (dynamic linking, plugins, cloud services) that the drafters could not fully specify.
% FOUNDING_PROBLEM_CORROBORATION: FSF and original drafters attest the ambiguity was intentional strategic under-specification to maximize software freedom reach. Industry commentators and some legal scholars attest the ambiguity reflects drafting limitations or overreach. Independent academic legal analysis documents the under-specification but remains agnostic on intent; no neutral judicial authority has corroborated either account.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the vacuum imposes diffuse transaction costs rather than concentrated rent extraction. Suppression is moderate (0.42): the ambiguity is maintained by active enforcement posturing on both sides and by judicial avoidance, not by direct coercion. Theater ratio is moderate (0.35): a substantial portion of enforcement activity consists of performative threats and compliance theater that never reaches adjudication. Accessibility collapse is moderate (0.45): adopters can exit to permissive licenses (BSD/MIT) or proprietary clean-room implementations, but within the GPL ecosystem alternatives are limited. Resistance is moderate-high (0.55): both industry and FSF-aligned communities actively resist each other's readings, creating a contested equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The pragmatic adopter seat experiences the vacuum as valuable flexibility; the clarity-seeking adopter seat experiences the identical legal landscape as costly uncertainty. The FSF-aligned enforcer seat sees the vacuum as necessary leverage to preserve software freedom; the industry ecosystem seat sees it as an obstacle to clear rules. The engine computes these divergent classifications from the same structural data: low d for beneficiaries (flexibility is a subsidy), high d for payers (uncertainty extracts as compliance cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatic adopters are beneficiaries because the ambiguity subsidizes their preferred compliance posture; their mobile exit options (can choose permissive alternatives, can litigate, can restructure code) place them near the beneficiary end. Clarity-seeking adopters are victims because the vacuum forces them to pay for legal certainty that the constraint itself withholds; their constrained exit (locked into GPL by upstream dependencies or organizational policy) places them near the target end. The agenda-setting enforcers on both sides derive influence from the vacuum but are not its primary beneficiaries or victims; they occupy symmetric positions in the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the enforcement vacuum as either pure coordination (a rope) or pure extraction (a snare). It has a genuine coordination functionâlicensed pluralism lets heterogeneous communities self-organize without a single judicial monopoly. But it also has asymmetric extractionâclarity-seekers pay costs that pragmatists avoid. The tangled_rope classification captures this hybridity: the same structural feature (absence of precedent) simultaneously coordinates pluralism and extracts from risk-averse parties. If the founding problem (preventing proprietary enclosure) were dead and the vacuum persisted solely as inertia, it would drift toward piton; if judicial clarity arrived and the structure remained, it would reclassify as rope or snare depending on the ruling's distributional consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates the enforcement_vacuum_reading of kernel gpl_copyleft_scope; does the low-extraction tangled_rope classification hold if definitive judicial precedent were to emerge?',
    'Track actual judicial rulings on GPL copyleft scope; a definitive high-court ruling would collapse the vacuum and force re-evaluation against the resulting settled rule.',
    'If precedent resolves broad scope, this reading likely collapses into strong_copyleft or narrow_scope depending on outcome, reclassifying from tangled_rope to rope (if widely accepted) or snare (if imposed against dominant practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Whether the vacuum reading survives judicial resolution').

omega_variable(
    sibling_reading_exclusion,
    'Does the enforcement_vacuum reading foreclose the strong_copyleft or narrow_scope readings, or do all three coexist as licensed plurality?',
    'Examine whether any single legal framework can simultaneously hold that copyleft scope is irreducibly unsettled and that it is definitively broad or narrow.',
    'If the vacuum is the true structural fact, both strong and narrow readings are performative enforcement positions rather than independent constraints; if one reading is legally correct, the vacuum reading is a temporary piton awaiting resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_exclusion, conceptual, 'Structural relationship between vacuum and sibling readings').

omega_variable(
    extraction_beneficiary_ambiguity,
    'Who captures the gains from the enforcement vacuumâpragmatic adopters gaining flexibility, legal practitioners gaining advisory revenue, or enforcement organizations gaining leverage?',
    'Economic analysis of compliance cost flows and enforcement-organization funding sources.',
    'If gains are captured by legal practitioners as advisory fees, the constraint is more extractive than the low-epsilon framing suggests; if gains are purely diffuse flexibility benefits, the constraint trends toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_beneficiary_ambiguity, empirical, 'Who receives the extraction from legal uncertainty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_envac_tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl_envac_tr_t4, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(gpl_envac_tr_t8, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(gpl_envac_tr_t12, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(gpl_envac_tr_t16, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(gpl_envac_tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(gpl_envac_tr_t24, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(gpl_envac_be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpl_envac_be_t4, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(gpl_envac_be_t8, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(gpl_envac_be_t12, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(gpl_envac_be_t16, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(gpl_envac_be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(gpl_envac_be_t24, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gpl_envac_su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gpl_envac_su_t4, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(gpl_envac_su_t8, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(gpl_envac_su_t12, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(gpl_envac_su_t16, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(gpl_envac_su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(gpl_envac_su_t24, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, narrow_scope_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'GPL copyleft scope' decomposes into three structurally distinct constraints: strong_copyleft_reading (high-extraction snare from the payer seat if broadly enforced), narrow_scope_reading (low-extraction rope if narrowly adhered to), and enforcement_vacuum_reading (low-extraction tangled_rope where uncertainty itself is structural). Their epsilon values differ because they describe different referents: the first two are competing positive legal theories, while the third is a second-order constraint describing the indeterminacy condition under which the first two operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
