% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Strong Copyleft Derivative Work Boundary
 *   domain: software licensing / intellectual property / open source governance
 *
 * SUMMARY:
 *   This constraint instantiates the strong_copyleft_reading of the contested
 *   kernel gpl_copyleft_scope. It treats GPL Section 2(b) as mandating that
 *   any combined or dynamically linked work must be licensed under the GPL,
 *   extending the derivative-work boundary to all forms of code coupling. The
 *   natural-language label 'GPL copyleft scope' conflates multiple
 *   structurally distinct claims; this story isolates the strong reading,
 *   which operates as a high-extraction constraint that structurally excludes
 *   proprietary vendors from integrating GPL components without full source
 *   release. The free software community gains a structural guarantee of code
 *   availability, while enforcement threats against dynamic linking patterns
 *   create credible coercion. Sibling readings (narrow_scope_reading and
 *   enforcement_vacuum_reading) are separate constraints linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - FSF: agenda_setter (institutional/constrained) â stewards the license text and promotes the broad interpretation
 *   - Free software community: primary beneficiary (organized/identity_locked) â gains source reciprocity guarantee
 *   - GPL contributors: secondary beneficiary (moderate/identity_locked) â contributes expecting downstream openness
 *   - Proprietary vendors: primary payer (powerful/constrained) â excluded from proprietary integration of GPL code
 *   - Commercial integrators: secondary payer (moderate/constrained) â faces compliance uncertainty and litigation risk
 *   - Permissive advocates: excluded (organized/mobile) â offers alternative licensing framework but marginalized in GPL governance
 *   - Judiciary: observer (institutional/analytical) â has not definitively ruled on software derivative works
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.84).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.75).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Derivative Work Boundary").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software licensing / intellectual property / open source governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '7f96f6d2-f763-4a49-a94c-58688f28197f').
narrative_ontology:cs_kernel_codification('7f96f6d2-f763-4a49-a94c-58688f28197f', fixed_text).
narrative_ontology:cs_authority_grounding('7f96f6d2-f763-4a49-a94c-58688f28197f', lineage).
narrative_ontology:cs_interpretation_layer_present('7f96f6d2-f763-4a49-a94c-58688f28197f').
narrative_ontology:cs_reading_relation('7f96f6d2-f763-4a49-a94c-58688f28197f', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('7f96f6d2-f763-4a49-a94c-58688f28197f', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('7f96f6d2-f763-4a49-a94c-58688f28197f', foundational, all_code_coupling_generates_derivative_work).
narrative_ontology:cs_axiom_status(all_code_coupling_generates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('7f96f6d2-f763-4a49-a94c-58688f28197f', all_code_coupling_generates_derivative_work, conventional).
narrative_ontology:cs_axiom('7f96f6d2-f763-4a49-a94c-58688f28197f', foundational, dynamic_linking_triggers_copyleft).
narrative_ontology:cs_axiom_status(dynamic_linking_triggers_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('7f96f6d2-f763-4a49-a94c-58688f28197f', dynamic_linking_triggers_copyleft, conventional).
narrative_ontology:cs_reference_frame('7f96f6d2-f763-4a49-a94c-58688f28197f', complete_source_reciprocity_framework).
narrative_ontology:cs_drift_state('7f96f6d2-f763-4a49-a94c-58688f28197f', contemporary_industry_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f96f6d2-f763-4a49-a94c-58688f28197f', '2026-06-20T00:00:00Z').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_contributors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, strong_copyleft_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, source_reciprocity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stewards the GPL text and promotes the strong copyleft interpretation as the authentic and intended reading of Section 2(b); its institutional identity and historical legitimacy are fused with preserving an unwavering derivative-work boundary that reaches all forms of code coupling.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from a structural guarantee that downstream proprietary integration of GPL code requires reciprocal source release; participants contribute under terms expecting the commons to expand rather than be enclosed, and their self-concept is constituted through opposition to proprietary software models.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_community, beneficiary,
    organized, generational, identity_locked, global).

% Individual and corporate authors who license code under GPL expecting downstream availability; they bear the cost of reduced commercial adoption in exchange for the structural guarantee that derivative works remain open, and are ideologically committed to the reciprocity framework.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_contributors, beneficiary,
    moderate, biographical, identity_locked, global).

% Large software firms seeking to integrate GPL libraries and components into proprietary products; they face credible copyright enforcement threats that force either complete source release, costly clean-room reimplementation, or architectural isolation that degrades product performance.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% SMEs, contractors, and product teams that rely on GPL dependencies; they operate under deliberate legal ambiguity around dynamic linking and plugin boundaries, often over-complying by releasing more source than necessary or abandoning useful GPL components to avoid litigation risk.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, national).

% BSD, MIT, and Apache ecosystem actors who argue that permissive licensing maximizes adoption and innovation; they are structurally excluded from GPL governance forums and their arguments are treated as hostile to the free software movement's goals rather than as legitimate alternative coordination.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, permissive_advocates, excluded,
    organized, biographical, mobile, global).

% Courts across jurisdictions that have not definitively adjudicated whether dynamic linking and plugin architectures constitute derivative works under copyright law; their future rulings could validate the strong reading, collapse it to narrow scope, or leave the boundary indeterminate.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of digital commons production by preventing free-riding: downstream users who modify and redistribute code must share those modifications, preserving the commons against enclosure.
% TRANSFER_FUNCTION: Transfers the obligation to release complete corresponding source code from proprietary integrators and commercial vendors to the free software community, enforced through copyright threat and license termination.
% ABSENT_VOICES: Permissive licensing advocates and proprietary industry associations are structurally excluded from GPL interpretive governance; they would argue that the derivative-work boundary should follow traditional copyright doctrine and exclude dynamic linking, but their positions are treated as external opposition rather than internal dissent.
% DISAPPEARANCE_RATIONALE: If the strong copyleft boundary vanished overnight, proprietary vendors would immediately integrate GPL libraries without source release; the free software commons would face rapid enclosure; enforcement organizations would lose their primary function; and new projects would likely shift toward permissive licenses as the guarantee of reciprocity dissolved.
% FOUNDING_PROBLEM: The enclosure of communal software by proprietary vendors who incorporated freely shared code into closed products without contributing back, leading to free-rider exploitation of volunteer labor and the erosion of the digital commons.
% FOUNDING_PROBLEM_CORROBORATION: Academic scholars of digital commons (e.g., Yochai Benkler, Steven Weber) attest from an analytical seat that free-rider risk is structurally present in information goods; proprietary industry associations attest from a payer seat that the problem is overstated and that strong copyleft itself creates adoption barriers that reduce overall welfare.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84) because the constraint forces proprietary actors to surrender source code or abandon integration, constituting a direct extraction of intellectual property. Suppression (0.75) reflects the active exclusion of proprietary business models and the credible enforcement threat that closes alternative integration paths. Theater ratio (0.48) captures the performative dimension: much GPL discourse frames the constraint as moral stewardship of the commons, while the functional effect is structural exclusion of competitors. Accessibility collapse (0.62) is moderate-high because once a firm commits to a GPL dependency, exiting to a proprietary alternative is costly; resistance (0.70) is substantial because proprietary vendors actively develop workarounds, fund alternative libraries, and lobby against broad copyleft interpretations. The measurement series run on a single shared time grid (0â35) to prevent temporal misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF and community seats, the constraint is legitimate commons governance preventing enclosure; from the proprietary vendor and integrator seats, it is coercive extraction of proprietary source code under threat of copyright litigation. The engine computes this divergence from the structural beneficiary/victim asymmetry and divergent exit options (identity_locked versus constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   The FSF and free software community sit near the beneficiary end (low d): they are subsidized by the constraint through expanded commons and mission vindication. Proprietary vendors and commercial integrators sit near the target end (high d): they bear the costs of forced source release or architectural constraint. The permissive advocates are excluded rather than coordinated â their exclusion is constitutive of the constraint's boundary. The judiciary sits at analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The strong copyleft reading carries a genuine coordination function â preventing free-riding in digital commons production â which could support a rope or tangled_rope classification. However, the classification as snare is warranted because the coordination story functions as cover for asymmetric extraction: one party (proprietary vendors) is forced to transfer a valuable asset (source code) under threat, while the beneficiary party does not bear symmetric costs. The constraint persists through suppression of proprietary alternatives rather than through mutual benefit, and the enforcement machinery is directed outward at non-adopters rather than inward at community coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the strong copyleft reading represent the authentic legal meaning of GPL Section 2(b), or is it an interpretive overreach by the FSF that exceeds the text''s conventional copyright boundaries?',
    'Definitive judicial precedent from high courts on whether dynamic linking and plugin architectures constitute derivative works under the GPL, or a systematic survey of national court rulings converging on one boundary.',
    'If courts reject the broad reading, this constraint collapses to lower extractiveness or becomes unenforceable, and the narrow_scope_reading gains structural dominance. If courts endorse it, the strong reading''s extraction is ratified as legally valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether strong copyleft is authentic legal meaning or interpretive overreach').

omega_variable(
    enforcement_capacity_empirical,
    'Are GPL enforcement threats actually credible and frequent enough to sustain high extraction, or does an enforcement vacuum render the constraint largely theatrical?',
    'Empirical inventory of GPL enforcement actions over the interval, including settlement rates, litigation outcomes, and resource levels of enforcement organizations.',
    'If enforcement is rare and unsuccessful, effective extraction is substantially lower than the structural reading suggests and the enforcement_vacuum_reading becomes the more accurate model. If enforcement is frequent and successful, the strong reading''s extraction is realized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_empirical, empirical, 'Whether enforcement threats are credible or constitute a vacuum').

omega_variable(
    market_exit_vs_legal_coercion,
    'Is proprietary vendor exclusion driven primarily by active legal coercion, or by rational self-selection away from legally risky integration?',
    'Survey of proprietary engineering decisions regarding GPL dependencies, measuring the relative weight of legal threat versus strategic preference for permissive stacks.',
    'If exclusion is primarily self-selection, suppression is lower than authored and the constraint operates more as a sorting mechanism than a snare. If driven by active enforcement threat, suppression is accurately measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_exit_vs_legal_coercion, empirical, 'Whether exclusion is coerced or self-selected').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_strong_tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gpl_strong_tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(gpl_strong_tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(gpl_strong_tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(gpl_strong_tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(gpl_strong_tr_t25, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(gpl_strong_tr_t30, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(gpl_strong_tr_t35, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(gpl_strong_be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(gpl_strong_be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement(gpl_strong_be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(gpl_strong_be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(gpl_strong_be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(gpl_strong_be_t25, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(gpl_strong_be_t30, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(gpl_strong_be_t35, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 35, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gpl_strong_su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gpl_strong_su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(gpl_strong_su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(gpl_strong_su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(gpl_strong_su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(gpl_strong_su_t25, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(gpl_strong_su_t30, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(gpl_strong_su_t35, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 35, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'GPL copyleft scope' conflates three structurally distinct constraints: narrow_scope_reading (low extraction, traditional copyright boundary), strong_copyleft_reading (high extraction, broad coupling boundary), and enforcement_vacuum_reading (contingent extraction, depends on enforcement capacity). They are decomposed per the epsilon-invariance principle and linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
