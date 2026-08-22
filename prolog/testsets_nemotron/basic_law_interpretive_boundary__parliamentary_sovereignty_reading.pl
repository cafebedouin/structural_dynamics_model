% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty Over Basic Law Interpretation
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary_sovereignty_reading
 *   of the basic_law_interpretive_boundary kernel. Under this reading, the
 *   Knesset as the elected sovereign holds ultimate authority to interpret
 *   and amend Basic Laws via simple majority, including the power to override
 *   judicial review. The judiciary serves an advisory function only; no
 *   external veto exists on legislative will. The constraint's extraction is
 *   near-zero for majoritarian policy except where international treaty
 *   obligations create binding external constraints. This reading positions
 *   the arrangement as a coordination mechanism (rope) — solving the problem
 *   of who ultimately decides constitutional meaning in a democratic polity
 *   by vesting that authority in the elected legislature. The claimed type is
 *   rope; metrics describe low extraction, minimal suppression, and low
 *   theater. Sibling readings (judicial_supremacy_reading,
 *   balanced_contestation_reading) are separate constraints with different
 *   structural profiles.
 *
 * KEY AGENTS:
 *   - knesset_majority_coalition: Primary beneficiary (institutional/arbitrage) — holds interpretive authority and amendment power
 *   - elected_representatives: Beneficiary (organized/mobile) — derive democratic legitimacy from unconstrained legislative sovereignty
 *   - supreme_court_justices: Advisory role (institutional/analytical) — interpretive output carries moral authority but no binding veto
 *   - minority_communities: Excluded (powerless/trapped) — lack institutional protection against majoritarian constitutional amendment
 *   - international_treaty_bodies: External constraint (organized/constrained) — impose obligations the Knesset cannot unilaterally override
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.08).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty Over Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism/judicial_review").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '39c04b58-a20a-4f85-a5f7-ed0115084824').
narrative_ontology:cs_kernel_codification('39c04b58-a20a-4f85-a5f7-ed0115084824', formalized).
narrative_ontology:cs_authority_grounding('39c04b58-a20a-4f85-a5f7-ed0115084824', lineage).
narrative_ontology:cs_interpretation_layer_present('39c04b58-a20a-4f85-a5f7-ed0115084824').
narrative_ontology:cs_reading_relation('39c04b58-a20a-4f85-a5f7-ed0115084824', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('39c04b58-a20a-4f85-a5f7-ed0115084824', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('39c04b58-a20a-4f85-a5f7-ed0115084824', foundational, knesset_sovereign_constitutional_interpreter).
narrative_ontology:cs_axiom_status(knesset_sovereign_constitutional_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('39c04b58-a20a-4f85-a5f7-ed0115084824', knesset_sovereign_constitutional_interpreter, deontological).
narrative_ontology:cs_axiom('39c04b58-a20a-4f85-a5f7-ed0115084824', foundational, judicial_review_advisory_only).
narrative_ontology:cs_axiom_status(judicial_review_advisory_only, holdable).
narrative_ontology:cs_axiom_grounding('39c04b58-a20a-4f85-a5f7-ed0115084824', judicial_review_advisory_only, deontological).
narrative_ontology:cs_reference_frame('39c04b58-a20a-4f85-a5f7-ed0115084824', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('39c04b58-a20a-4f85-a5f7-ed0115084824', contemporary_judicial_activism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('39c04b58-a20a-4f85-a5f7-ed0115084824', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, elected_representatives).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, democratic_legitimacy_of_elected_bodies).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_constitutional_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority to interpret and amend Basic Laws via simple majority. Can override judicial review. Collects the democratic legitimacy dividend of unconstrained constitutional sovereignty. Faces no institutional veto on domestic policy. Exit is arbitrage-grade: can shift coalition composition, call elections, or amend the constitutional framework itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, beneficiary,
    institutional, generational, arbitrage, national).

% Derive democratic legitimacy and effective power from the Knesset's unconstrained sovereign authority. Their legislative acts cannot be invalidated by courts on constitutional grounds. Exit is mobile: can join/leave coalitions, run on different platforms, or seek judicial appointments — but their power derives from the parliamentary sovereignty this constraint establishes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, elected_representatives, beneficiary,
    organized, biographical, mobile, national).

% Issue advisory opinions on Basic Law interpretation. Their interpretations carry moral and professional authority but no binding veto. They neither collect extraction nor bear its costs. Exit is analytical: they observe the constraint's operation from within the institution but cannot change its structural terms.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices, observer,
    institutional, generational, analytical, national).

% Lack institutional protection against majoritarian constitutional amendment. Their rights depend on political coalition-building rather than judicial enforcement. Exit is trapped: identity and territory bind them to the polity; emigration is costly and incomplete. They would object to the absence of judicial veto but are structurally excluded from the constitutional interpretive authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_communities, excluded,
    powerless, generational, trapped, national).

% Impose treaty obligations that the Knesset cannot unilaterally override under this reading. These obligations function as the primary external constraint on Knesset sovereignty. They do not collect extraction from this constraint but their decisions create the near-zero ε floor for domestic policy. Exit is constrained: they operate within international legal frameworks but can apply diplomatic and reputational pressure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, observer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates final constitutional interpretive authority to the elected legislature, resolving the coordination problem of who ultimately decides constitutional meaning in a democratic polity. Prevents judicial veto from blocking democratic majorities.
% TRANSFER_FUNCTION: Moves constitutional interpretive authority from the judiciary to the Knesset. The arrangement transfers the power of final constitutional say from unelected judges to elected representatives. No monetary transfer; the transfer is of authority and veto power.
% ABSENT_VOICES: Minority communities (Arab citizens, Haredi subgroups, migrant populations) would object to the absence of judicial veto protection but are structurally excluded from the constitutional interpretive authority. International human rights bodies would object to the lack of domestic enforcement mechanism for treaty obligations but operate at global scope.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight and judicial supremacy were instantiated instead, the Knesset would lose its override power, minority rights would gain judicial enforcement, and the constitutional order would reorganize around court-centered review. The world rearranges because arrangements depend on this constraint — the Knesset's legislative strategy, coalition agreements, and minority political mobilization all presuppose parliamentary sovereignty.
% FOUNDING_PROBLEM: Resolving the constitutional interpretive authority vacuum after 1992 Basic Laws: whether the Knesset (elected, majoritarian) or the Supreme Court (professional, rights-protective) holds ultimate say over Basic Law meaning. The parliamentary sovereignty reading was built to solve the democratic legitimacy deficit of judicial review over constitutional norms.
% FOUNDING_PROBLEM_CORROBORATION: Knesset majorities and coalition partners attest the problem is live — democratic legitimacy requires parliamentary sovereignty. Supreme Court justices and civil society organizations attest the problem is dead — judicial review has de facto displaced parliamentary sovereignty. Legal scholars are divided. No external corroboration resolves the contest; the founding problem's status is itself the axis of the kernel dispute.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.12) because the constraint primarily coordinates democratic decision-making by allocating final interpretive authority to the elected body; the primary cost is borne by minorities who lack exit from majoritarian constitutional amendment, but this cost is structural to majoritarian democracy rather than extractive rent. Suppression is minimal (0.08) because the constraint does not actively prevent alternatives — minority protections exist through political process, not judicial veto. Theater ratio is low (0.15) and rising slowly as performative judicial review debates increase while the structural reality remains parliamentary sovereignty. The measurement grid shows gradual increases across all three metrics from 1992 (pre-constitutional revolution) to 2024, reflecting the judicialization of politics and growing legitimacy contests, but the structural ε remains near-zero for domestic majoritarian policy.
 *
 * PERSPECTIVAL GAP:
 *   From the Knesset majority seat, this is pure coordination (rope) — the elected sovereign decides. From minority community seats, the same structure operates as a snare-like vulnerability — no institutional barrier prevents majoritarian rights restriction. The engine computes this seat divergence from the structural data; the claimed type (rope) reflects the authoring seat's assessment of the constraint's primary function. The divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majority coalition and elected representatives are beneficiaries (d near 0.0) — the constraint subsidizes their authority. Minority communities are de facto targets (d nearer 1.0) — they bear the cost of having no constitutional veto against majoritarian amendment, but their exit options are 'trapped' (identity/territory-bound) rather than constrained by this specific constraint. The Supreme Court sits near analytical (d ~ 0.5) — it neither collects nor pays but provides interpretive labor. International treaty bodies are external constraint sources, not targets of this constraint's extraction. The directionality derivation from beneficiary/victim declarations + exit options produces this gradient without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving constitutional interpretive authority in favor of democratic legitimacy) remains live per the Knesset and its supporters; critics argue it is dead (judicial review has de facto displaced parliamentary sovereignty) or contested. The mandate has not atrophied — the constraint actively coordinates constitutional decision-making. No mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this reading of the basic_law_interpretive_boundary kernel instantiate a distinct constraint from its siblings, or is it a facet of the same constraint evaluated differently?',
    'Trace whether the structural elements (ε, beneficiaries/victims, enforcement, exit options) diverge sufficiently across readings to satisfy ε-invariance; if changing the reading changes ε or the beneficiary/victim structure, they are distinct constraints.',
    'If distinct, each reading gets its own story and classification; if not, the kernel is one constraint with observer-relative type — which violates DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether parliamentary_sovereignty_reading is a separate constraint from judicial_supremacy_reading and balanced_contestation_reading per ε-invariance').

omega_variable(
    extraction_boundary_treaty_obligations,
    'Do international treaty obligations constitute genuine extraction under this reading, or are they voluntarily accepted coordination costs?',
    'Compare the Knesset''s practical ability to denounce or reinterpret treaties versus domestic Basic Laws; if treaty exit is structurally harder, the obligation functions as extraction.',
    'If treaties are extraction, ε rises above near-zero and the constraint may shift toward tangled_rope (coordination + asymmetric extraction from treaty-bound minorities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_boundary_treaty_obligations, empirical, 'Whether international obligations under this reading operate as coordination or extraction').

omega_variable(
    advisory_judiciary_enforcement,
    'Is the judiciary genuinely advisory under this reading, or does its interpretive output create de facto constraints through legitimacy pressure?',
    'Measure compliance rates when the Court issues non-binding opinions contrary to Knesset majorities; if compliance is near-universal, the advisory label masks structural constraint.',
    'If advisory opinions bind in practice, suppression and extraction are higher than declared; the constraint may be tangled_rope rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_judiciary_enforcement, empirical, 'Whether the Court''s advisory role under this reading generates de facto suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement(basi_tr_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2020, 0.14).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.05).
narrative_ontology:measurement(basi_be_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1995, 0.08).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2010, 0.11).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2020, 0.12).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.03).
narrative_ontology:measurement(basi_su_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1995, 0.05).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2000, 0.06).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2010, 0.07).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2020, 0.08).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_boundary kernel. The ε-invariance principle requires separate stories because ε differs: this reading has near-zero ε for domestic majoritarian policy; judicial_supremacy_reading has substantial ε (judicial veto extracts from legislative majorities); balanced_contestation_reading has intermediate ε. The beneficiary/victim structures also differ: this reading benefits the Knesset majority; judicial_supremacy_reading benefits minority-rights holders via judicial protection; balanced_contestation_reading has a more diffuse beneficiary structure. All three stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
