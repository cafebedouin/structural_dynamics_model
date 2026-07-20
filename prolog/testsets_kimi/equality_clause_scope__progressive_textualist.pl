% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Progressive Textualist Equality Clause Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the progressive_textualist reading of
 *   the equality_clause_scope kernel. The kernel is the constitutional text's
 *   equality principle (primarily the Fourteenth Amendment Equal Protection
 *   Clause). Three competing readings exist: restrictive_originalist
 *   (equality limited to propertied white males as political actors within an
 *   eighteenth-century social contract), expansive_universalist (self-evident
 *   universal truth enforceable by courts regardless of textual history), and
 *   this reading, progressive_textualist (the text contains a general
 *   equality principle whose application scope can expand only through
 *   democratic constitutional amendment, not judicial reinterpretation). This
 *   reading occupies a middle position: it claims textual fidelity and
 *   democratic legitimacy, but requires active judicial enforcement of the
 *   amendment-versus-interpretation boundary. Its structural effect is to
 *   channel all equality expansion through supermajoritarian processes,
 *   benefiting political coalitions that control those processes while
 *   imposing costs on discrete minorities who lack the leverage to secure
 *   amendments.
 *
 * KEY AGENTS:
 *   - textualist_judiciary (agenda_setter/institutional/analytical exit): Enforces the no-reinterpretation boundary through doctrines of restraint.
 *   - amendment_gatekeepers (beneficiary/institutional): Control the Article V process and benefit from procedural stability and democratic legitimation.
 *   - discrete_insular_minorities (payer/powerless/trapped): Bear the costs of judicial exclusion and high amendment barriers.
 *   - living_constitutional_advocates (excluded/organized): Methodologically excluded from authoritative constitutional argumentation.
 *   - comparative_constitutional_scholars (observer/analytical): External analytical seat documenting democratic deficits of rigid textualism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.58).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.65).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Progressive Textualist Equality Clause Reading").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '33fee544-298e-46de-8903-6e92941a7b16').
narrative_ontology:cs_kernel_codification('33fee544-298e-46de-8903-6e92941a7b16', fixed_text).
narrative_ontology:cs_authority_grounding('33fee544-298e-46de-8903-6e92941a7b16', lineage).
narrative_ontology:cs_reading_relation('33fee544-298e-46de-8903-6e92941a7b16', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('33fee544-298e-46de-8903-6e92941a7b16', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('33fee544-298e-46de-8903-6e92941a7b16', foundational, equality_principle_textually_embedded).
narrative_ontology:cs_axiom_status(equality_principle_textually_embedded, holdable).
narrative_ontology:cs_axiom_grounding('33fee544-298e-46de-8903-6e92941a7b16', equality_principle_textually_embedded, conventional).
narrative_ontology:cs_axiom('33fee544-298e-46de-8903-6e92941a7b16', foundational, amendment_sole_legitimate_expansion_mechanism).
narrative_ontology:cs_axiom_status(amendment_sole_legitimate_expansion_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('33fee544-298e-46de-8903-6e92941a7b16', amendment_sole_legitimate_expansion_mechanism, conventional).
narrative_ontology:cs_reference_frame('33fee544-298e-46de-8903-6e92941a7b16', amendment_bounded_equality).
narrative_ontology:cs_drift_state('33fee544-298e-46de-8903-6e92941a7b16', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33fee544-298e-46de-8903-6e92941a7b16', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, amendment_gatekeepers).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, discrete_insular_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constraint by refusing to expand the application scope of the equality principle without a constitutional amendment; enforces the boundary between interpretation and revision through doctrines of judicial restraint, original meaning, and textual fidelity.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, textualist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% State legislatures and congressional supermajorities whose consent is required under Article V to expand the scope of the equality principle; they control the pace of inclusion and benefit from the procedural stability and democratic legitimacy the reading preserves.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, amendment_gatekeepers, beneficiary,
    institutional, generational, constrained, national).

% Groups historically excluded from the constitutional bargain who seek equality protections but face supermajoritarian barriers to amendment and are denied judicial shortcuts to inclusion; they bear the cost of constitutional inertia.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, discrete_insular_minorities, payer,
    powerless, biographical, trapped, national).

% Legal scholars and jurists who argue for evolutionary interpretation of the equality clause as a legitimate method of constitutional growth; their methodological approach is structurally excluded from authoritative constitutional argument under the amendment-only rule.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, living_constitutional_advocates, excluded,
    organized, generational, constrained, national).

% Observe that peer liberal democracies often permit judicial evolution of equality norms; document the high amendment barriers and potential democratic deficits of rigid textualism when minority rights are at stake.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large, diverse polity around a stable constitutional text by channeling all normative expansion of equality into a supermajoritarian amendment process, preventing arbitrary judicial revision and preserving democratic legitimacy.
% TRANSFER_FUNCTION: Transfers the costs of constitutional inertia and majoritarian gatekeeping to discrete minorities seeking equality protections, while transferring agenda control over the pace and scope of equality expansion to supermajoritarian political coalitions.
% ABSENT_VOICES: Expansive universalists who would argue for immediate judicial enforcement of equality regardless of textual history; living constitutionalists who would treat the text as evolving through interpretation alone; and members of permanently excluded groups who lack the political capital to drive amendments.
% DISAPPEARANCE_RATIONALE: If the amendment-only boundary vanished, courts would likely reinterpret the equality clause directly, bypassing supermajoritarian barriers; the locus of constitutional change would shift from legislatures to the judiciary, and the polity would lose the coordinating stability of fixed textual meaning channeled through democratic process.
% FOUNDING_PROBLEM: How to secure a stable constitutional order that can perpetuate itself across generations while allowing for democratic revision, avoiding both arbitrary judicial despotism and unchangeable textual fossilization.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars attest that rigid textualism with high amendment barriers creates democratic deficits; abolitionists and suffragists historically attested that the amendment process was necessary but insufficient without judicial engagement. No neutral party fully corroborates the live status from outside the benefiting parties.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-high because the constraint systematically transfers the costs of constitutional inertia to minorities who cannot overcome amendment barriers. Suppression (0.65) reflects the active judicial and political suppression of interpretive expansion as a legitimate method. Theater_ratio (0.30) is moderate: the democratic-amendment story is partially genuine but also serves to dignify what functionally operates as a veto gate. Accessibility_collapse (0.50) is moderate because while judicial reinterpretation is closed off, the amendment alternative remains formally open (though practically inaccessible). Resistance (0.60) reflects sustained opposition from living constitutionalists, minority advocates, and judges who view amendment-only approaches as morally inadequate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (textualist judiciary) experiences the constraint as a discipline of judicial restraint and democratic deferenceâa genuine coordination mechanism preserving the rule of law. The payer seat (discrete minorities) experiences the same structure as an enforced waiting period that may extend indefinitely, with no recourse against majoritarian obstruction. The engine computes this divergence from identical structural data through directionality: the judiciary's d is pushed toward beneficiary by its institutional control over enforcement, while minorities' d is pushed toward full target by victim-status plus trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (amendment_gatekeepers) have low directionality because the constraint subsidizes their institutional control and democratic legitimacy. Victims (discrete_insular_minorities) have high directionality because the constraint extracts from them by denying judicial relief and binding them to a political process they cannot mobilize. The textualist judiciary sits ambiguously: it enforces the constraint but does not personally collect the extractive surplus; its power and analytical exit place it nearer the beneficiary end, though not as low as the gatekeepers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misidentification as a pure snare: there IS a genuine coordination function (stable constitutional text, democratic legitimation, prevention of arbitrary judicial rule). However, the mandatrophy risk is that the amendment process, once a genuine safety valve, has atrophied into a near-impossible threshold for minorities, converting a coordination mechanism into an inertial lock. The authored metrics (theater_ratio 0.30, rising over time) capture this drift toward performative democratic formalism without real revision capacity. If amendment passage rates for minority rights approach zero, the constraint would migrate toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does this constraint represent a genuine reading of the constitutional text or a post-hoc rationalization for majoritarian control over minority rights?',
    'Historical-linguistic analysis of the original public meaning of equality clauses combined with examination of ratification debates to determine whether the text plausibly contains a general principle or only a limited application.',
    'If the text contains no general principle, this reading collapses toward restrictive originalism; if it does, the reading gains textual credibility but the extraction mechanism remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Uncertainty about whether the progressive textualist reading is textually grounded or constructed.').

omega_variable(
    amendment_barrier_empiricism,
    'Are supermajoritarian amendment barriers structurally passable for discrete minorities seeking equality, or do they function as permanent veto gates?',
    'Empirical study of amendment success rates for minority-protective amendments compared to majoritarian-interest amendments across constitutional history.',
    'If minority-protective amendments are systematically blocked, the coordination function is shown to be asymmetrically extractive and the constraint leans toward snare; if passage rates are comparable, the extraction is moderated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_barrier_empiricism, empirical, 'Whether amendment process is genuinely accessible to minorities.').

omega_variable(
    judicial_restraint_suppression,
    'Does the judicial restraint required by this reading structurally suppress minority rights or merely defer democratic deliberation?',
    'Comparative analysis of rights expansion in jurisdictions with strong-form judicial review versus pure amendment-based textualism.',
    'If judicial review proves empirically necessary for minority rights expansion, the suppression metric understates the harm; if amendment processes suffice, the reading is vindicated as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_restraint_suppression, empirical, 'Comparative necessity of judicial review for equality expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.18).
narrative_ontology:measurement(equa_tr_t12, equality_clause_scope__progressive_textualist, theater_ratio, 12, 0.21).
narrative_ontology:measurement(equa_tr_t24, equality_clause_scope__progressive_textualist, theater_ratio, 24, 0.24).
narrative_ontology:measurement(equa_tr_t36, equality_clause_scope__progressive_textualist, theater_ratio, 36, 0.27).
narrative_ontology:measurement(equa_tr_t48, equality_clause_scope__progressive_textualist, theater_ratio, 48, 0.29).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__progressive_textualist, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(equa_be_t12, equality_clause_scope__progressive_textualist, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(equa_be_t24, equality_clause_scope__progressive_textualist, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(equa_be_t36, equality_clause_scope__progressive_textualist, base_extractiveness, 36, 0.52).
narrative_ontology:measurement(equa_be_t48, equality_clause_scope__progressive_textualist, base_extractiveness, 48, 0.55).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__progressive_textualist, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(equa_su_t12, equality_clause_scope__progressive_textualist, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(equa_su_t24, equality_clause_scope__progressive_textualist, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(equa_su_t36, equality_clause_scope__progressive_textualist, suppression_requirement, 36, 0.61).
narrative_ontology:measurement(equa_su_t48, equality_clause_scope__progressive_textualist, suppression_requirement, 48, 0.63).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__progressive_textualist, suppression_requirement, 60, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equality_clause_scope kernel, which decomposes into three structurally distinct constraints: restrictive_originalist, progressive_textualist, and expansive_universalist. Each has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
