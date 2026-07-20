% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right (Magistrate Reading)
 *   domain: constitutional/political/legal
 *
 * SUMMARY:
 *   This constraint story instantiates the magistrate reading of the
 *   remonstrance authority kernel: the claim that the remonstrance right was
 *   a fundamental constitutional mechanism preserving ancient liberties
 *   against arbitrary innovation. Structurally, the constraint operates as a
 *   tangled rope. It coordinates genuine constitutional review (preventing
 *   arbitrary edicts) while asymmetrically extracting from fiscal reform and
 *   preserving a tax-exempt magisterial class. The divergence between the
 *   magistrate reading's normative framing (rope/mountain) and the
 *   structurally authored metrics (high extraction, active enforcement,
 *   victim set) is the signal the corpus is designed to capture.
 *
 * KEY AGENTS:
 *   - tax_exempt_magistracy: Primary beneficiary (powerful/constrained) â collects corporate immunities and tax exemptions under cover of constitutional guardianship.
 *   - parlementary_benches: Agenda-setter and victim when overridden (institutional/constrained) â administers remonstrance but pays via lit de justice and exile.
 *   - fiscal_reform_state: Primary payer (institutional/constrained) â bears the cost of blocked fiscal modernization.
 *   - tax_burden_bearers: Excluded victims (powerless/trapped) â carry the tax load that reform would have shifted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.79).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.75).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional/political/legal").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '2be16ea8-c91a-41b3-8178-c0db212c0a58').
narrative_ontology:cs_kernel_codification('2be16ea8-c91a-41b3-8178-c0db212c0a58', fixed_text).
narrative_ontology:cs_authority_grounding('2be16ea8-c91a-41b3-8178-c0db212c0a58', lineage).
narrative_ontology:cs_interpretation_layer_present('2be16ea8-c91a-41b3-8178-c0db212c0a58').
narrative_ontology:cs_reading_relation('2be16ea8-c91a-41b3-8178-c0db212c0a58', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('2be16ea8-c91a-41b3-8178-c0db212c0a58', foundational, ancient_liberties_are_fundamental_law).
narrative_ontology:cs_axiom_status(ancient_liberties_are_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('2be16ea8-c91a-41b3-8178-c0db212c0a58', ancient_liberties_are_fundamental_law, conventional).
narrative_ontology:cs_axiom('2be16ea8-c91a-41b3-8178-c0db212c0a58', foundational, magistracy_guardianship_of_constitution).
narrative_ontology:cs_axiom_status(magistracy_guardianship_of_constitution, holdable).
narrative_ontology:cs_axiom_grounding('2be16ea8-c91a-41b3-8178-c0db212c0a58', magistracy_guardianship_of_constitution, deontological).
narrative_ontology:cs_reference_frame('2be16ea8-c91a-41b3-8178-c0db212c0a58', parlementary_constitutional_order).
narrative_ontology:cs_drift_state('2be16ea8-c91a-41b3-8178-c0db212c0a58', late_ancien_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2be16ea8-c91a-41b3-8178-c0db212c0a58', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, tax_exempt_magistracy).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, fiscal_reform_state).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, parlementary_benches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds venal and hereditary judicial offices, enjoying tax exemptions and corporate privileges tied to the parlementary bench. The remonstrance right preserves their status by validating a constitutional order in which the magistracy is the guardian of fundamental law, making their fiscal and social immunities appear as necessary correlates of judicial independence.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, tax_exempt_magistracy, beneficiary,
    powerful, generational, constrained, national).

% Exercise the right of remonstrance to review royal edicts against fundamental law and ancient liberties, temporarily blocking registration. Their institutional identity is fused with this constitutional role. When the crown overrides them via lit de justice or exile, they bear the ceremonial and political costs of suppressed autonomy, yet they remain bound to the office and its rituals.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlementary_benches, agenda_setter,
    institutional, generational, constrained, national).

% The crown and its finance ministers attempt to pass fiscal reform edicts to rationalize taxation and service public debt. The remonstrance right blocks, delays, or dilutes these reforms, extracting political time and fiscal capacity. The state cannot easily abolish the parlements without triggering a legitimacy crisis, so it is constrained to operate within a constitutional vocabulary that the magistracy interprets.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, fiscal_reform_state, payer,
    institutional, biographical, constrained, national).

% Commoners, peasants, and urban laborers who carry the regressive and inefficient tax load that fiscal reform edicts would have alleviated. They are not represented in the constitutional dialogue between crown and parlements, and their interests appear only indirectly as arguments wielded by one side or the other.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, tax_burden_bearers, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, tax_exempt_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a layer of fundamental law and corporate liberties against arbitrary royal innovation by requiring parlementary review and temporary blocking of edicts that violate ancient constitutional norms.
% TRANSFER_FUNCTION: Moves fiscal and legislative blocking power from the crown to the parlementary benches, and preserves tax exemptions and corporate immunities for the robe nobility at the cost of blocked public finance modernization.
% ABSENT_VOICES: The tax-burdened commoners who would benefit from fiscal rationalization are structurally excluded from the remonstrance dialogue; their interests are mediated by the crown or ignored entirely.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, the constitutional equilibrium of the ancien rÃ©gime would collapse toward unchecked royal absolutism on one side and unmediated popular claim-making on the other. The magistracy would lose its primary institutional justification, and the fiscal structure would reorganize around direct royal extraction or revolutionary rupture.
% FOUNDING_PROBLEM: The theoretical absolutism of the French crown in the sixteenth and seventeenth centuries threatened to erode the corporate liberties, customary checks, and fundamental law that the judicial magistracy claimed to embody.
% FOUNDING_PROBLEM_CORROBORATION: Parlementary registers and magistrate jurisprudence attest the problem from the beneficiary side. Royal ministers and Enlightenment fiscal critics attesting the crown's need for rationalization provide an outside perspective that the problem is overstated; no fully neutral corroborator exists.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because the constraint systematically blocks or delays fiscal reform edicts, preserving fiscal inequities that benefit the magisterial class. Suppression is high (0.75) because the arrangement depends on the crown's repeated resort to lit de justice and coercion to overcome remonstrance; it is actively enforced from both sides (parlements enforce delay, crown enforces override). Theater ratio rises to 0.52 as remonstrance becomes increasingly ritualized in the eighteenth century: the constitutional vocabulary is performed even as both sides know the crown will ultimately prevail by force. Accessibility collapse is moderate-high (0.65) because once the fundamental-law framework is accepted, alternatives (absolutist or popular-democratic) become illegitimate within the discourse. Resistance is substantial (0.70) because the crown and reform ministers actively contest parlementary blockage.
 *
 * PERSPECTIVAL GAP:
 *   The tax_exempt_magistracy and parlementary_benches experience the constraint as constitutional duty and corporate honor; the fiscal_reform_state experiences it as an extractive veto. The engine will compute these seats differently because beneficiary declarations and exit options differ: the magistracy has no exit from its privilege-structure, but it is subsidized; the crown has constrained exit because abolishing parlements risks legitimacy collapse; the parlements are identity-locked to their constitutional role.
 *
 * DIRECTIONALITY LOGIC:
 *   The tax_exempt_magistracy sits at the beneficiary end: the constraint subsidizes their fiscal and corporate privileges by framing them as necessary to judicial independence. The fiscal_reform_state sits at the target end: it bears the direct cost of blocked edicts and lost fiscal capacity. The parlementary_benches are directionally split: as agenda-setters they administer the constraint and gain institutional dignity, but as victims of override they suffer high extraction when the crown deploys lit de justice; their identity-locked exit (office fused with constitutional self-concept) keeps them near the target end during override episodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents the false binary of rope (pure coordination, no victims) versus snare (pure extraction, no coordination). The magistrate reading's genuine coordination function â blocking arbitrary edicts â is real and documented. However, the presence of a concentrated beneficiary class (tax-exempt magistracy) and an identifiable victim set (fiscal reform state, overridden parlements) means the coordination is not purity but hybrid. Mandatrophy would occur if we treated the constitutional rhetoric as dispositive and ignored the fiscal extraction it enabled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ancient_liberty_constructedness,
    'Is the ''ancient liberty'' invoked by the magistrate reading a genuinely continuous customary law, or a retroactive construction that legitimizes magisterial privilege?',
    'Archival continuity analysis comparing parlementary registers of the sixteenth-eighteenth centuries with earlier customary and capitulary sources to establish whether the claimed liberties pre-exist the magistracy''s interest in them.',
    'If the liberties are largely constructed, the coordination function is cover for extraction and the constraint drifts toward snare; if continuous, the coordination function is more robustly grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_liberty_constructedness, empirical, 'Whether ancient liberties are continuous custom or retroactive construction.').

omega_variable(
    remonstrance_fiscal_extraction,
    'Does the remonstrance right extract from the broader public by blocking necessary fiscal reform, or does it protect the public from predatory royal taxation?',
    'Content analysis of blocked edicts: classify each as regressive, progressive, or neutral with respect to the tax burden distribution, and compare against enacted edicts that passed.',
    'If blocked edicts were predominantly progressive or public-goods-enhancing, the constraint extracts from the tax-burdened public; if predatory, the constraint coordinates protection and shifts toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remonstrance_fiscal_extraction, empirical, 'Whether fiscal reform blockage serves or harms the broader public.').

omega_variable(
    parlementary_victim_ambiguity,
    'When parlements are overridden by lit de justice, are they victims of the remonstrance constraint itself or of the constraint''s suppression?',
    'Structural analysis of costs: compare the incidence of override costs (exile, loss of autonomy) against the baseline of a system without remonstrance rights.',
    'If they are victims only of suppression, their inclusion in the victim set may overstate the constraint''s intrinsic extraction; if the override is an inherent structural feature of the constraint, the victim classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parlementary_victim_ambiguity, conceptual, 'Whether parlementary override costs are intrinsic to the constraint or external suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rem_auth_mag_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(rem_auth_mag_tr_t10, remonstrance_authority__magistrate_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(rem_auth_mag_tr_t20, remonstrance_authority__magistrate_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(rem_auth_mag_tr_t30, remonstrance_authority__magistrate_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(rem_auth_mag_tr_t40, remonstrance_authority__magistrate_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(rem_auth_mag_tr_t50, remonstrance_authority__magistrate_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(rem_auth_mag_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(rem_auth_mag_be_t10, remonstrance_authority__magistrate_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(rem_auth_mag_be_t20, remonstrance_authority__magistrate_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(rem_auth_mag_be_t30, remonstrance_authority__magistrate_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(rem_auth_mag_be_t40, remonstrance_authority__magistrate_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(rem_auth_mag_be_t50, remonstrance_authority__magistrate_reading, base_extractiveness, 50, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(rem_auth_mag_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(rem_auth_mag_su_t10, remonstrance_authority__magistrate_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(rem_auth_mag_su_t20, remonstrance_authority__magistrate_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(rem_auth_mag_su_t30, remonstrance_authority__magistrate_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(rem_auth_mag_su_t40, remonstrance_authority__magistrate_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(rem_auth_mag_su_t50, remonstrance_authority__magistrate_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% The remonstrance authority kernel decomposes into two structurally distinct constraints: the magistrate reading (remonstrance as constitutional liberty preserving ancient law) and the crown reading (remonstrance as particularist veto blocking fiscal modernization). They share the institutional kernel (parlementary registration and remonstrance) but differ in Îµ, beneficiary/victim structure, and normative grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
