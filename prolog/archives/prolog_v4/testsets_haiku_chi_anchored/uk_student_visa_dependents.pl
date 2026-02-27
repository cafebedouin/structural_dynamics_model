% ============================================================================
% CONSTRAINT STORY: uk_student_visa_dependents
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_student_visa_dependents, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_student_visa_dependents
 *   human_readable: UK Policy Restricting Dependents of International Students
 *   domain: political/economic/immigration
 *
 * SUMMARY:
 *   The UK government's 2023 policy restricting international students on
 *   taught postgraduate courses from bringing family dependents represents a
 *   constraint imposed to reduce net migration statistics. The policy creates
 *   a structural tension between the institutional interest in international
 *   student recruitment (and associated tuition revenue) and the political
 *   priority of demonstrating migration reduction. The constraint exhibits
 *   snare characteristics for students and families (high suppression, no
 *   coordination benefit), while appearing as rope to the Home Office (pure
 *   coordination of exclusion rules). The extraction mechanism is enforced
 *   via visa conditions with substantial coercive overhead — students must
 *   prove dependent income or abandon family reunion. The theater ratio
 *   reflects that the policy achieves its stated metric (reduced net
 *   migration counts) through accounting redefinition as much as behavioral
 *   change. The policy has generated significant organized opposition from
 *   universities and advocacy groups, suggesting it functions as snare rather
 *   than legitimate coordination.
 *
 * KEY AGENTS:
 *   - International postgraduate students: Primary victim (powerless/trapped) — bear full cost of family separation while policy extracts migration-reduction value
 *   - Student families and dependents: Primary victim (powerless/trapped) — cannot join student without private income sponsorship now unaffordable
 *   - UK higher education sector: Secondary actor (organized/constrained) — benefits from tuition revenue but faces enrollment decline and reputational damage
 *   - UK Home Office and border control: Primary beneficiary (institutional/arbitrage) — achieves net migration reduction metric at no cost
 *   - Prospective students from competitor nations: Mobile victim (moderate/mobile) — can exit to Canada, Australia, or US offering dependent visas
 *   - Migration policy bureaucracy: Institutional actor (institutional/arbitrage) — maintains enforcement mechanism; sees constraint as solution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_student_visa_dependents, 0.58).
domain_priors:suppression_score(uk_student_visa_dependents, 0.72).
domain_priors:theater_ratio(uk_student_visa_dependents, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_student_visa_dependents, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_student_visa_dependents, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(uk_student_visa_dependents, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_student_visa_dependents, snare).
narrative_ontology:human_readable(uk_student_visa_dependents, "UK Policy Restricting Dependents of International Students").
narrative_ontology:topic_domain(uk_student_visa_dependents, "political/economic/immigration").

domain_priors:requires_active_enforcement(uk_student_visa_dependents).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, uk_net_migration_statistics).
narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, home_office_border_control).
narrative_ontology:constraint_victim(uk_student_visa_dependents, international_postgraduate_students).
narrative_ontology:constraint_victim(uk_student_visa_dependents, student_families).
narrative_ontology:constraint_victim(uk_student_visa_dependents, uk_higher_education_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERNATIONAL POSTGRADUATE STUDENT (SNARE) — Trapped by visa conditions: cannot bring dependents without losing visa status or incurring massive costs. Exit options are either abandon family separation or abandon UK education. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. High effective extraction: student bears full cost of family separation while policy extracts migration-reduction value.
constraint_indexing:constraint_classification(uk_student_visa_dependents, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEPENDENT FAMILY MEMBER (SNARE) — Trapped by policy: cannot join student without private income and sponsorship thresholds now unaffordable. Cannot exit separation during study period (1-3 years). d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. Maximum extraction: family bears complete separation cost; policy supplies no coordination or benefit.
constraint_indexing:constraint_classification(uk_student_visa_dependents, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UK HIGHER EDUCATION SECTOR (TANGLED ROPE) — Organized but constrained. Sector benefits from international student fees (£25,000-50,000 annually per student) but faces extraction via enrollment decline. Policy is enforced against sector's own interests. Sector has institutional voice but cannot exit policy without losing recruitment volume to competitors (Canada, Australia, US). d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.58. Mixed: coordination function (international recruitment) is preserved, but extraction (enrollment decline, reputational damage) grows.
constraint_indexing:constraint_classification(uk_student_visa_dependents, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: UK HOME OFFICE / NET MIGRATION ACCOUNTING (ROPE) — Institutional beneficiary with arbitrage exit. Policy directly reduces net migration figures, achieving stated policy objective at no cost to Home Office. Experiences constraint as pure coordination: setting exclusion rules solves the migration-counting problem. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.006. Negative effective extraction = net beneficiary with minimal coercion overhead.
constraint_indexing:constraint_classification(uk_student_visa_dependents, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MIGRATION POLICY THEATER (PITON) — The policy is substantially performative. Net migration figures are manufactured through accounting definitions (adding/subtracting dependent counts), not actual migration flow reduction. The policy's function (reducing net migration count) is decoupled from any mechanism that reduces actual migration. Theater_ratio=0.65 reflects that policy achieves its stated metric while producing minimal real behavioral change for most students (many were already separating, or choosing competing destinations). The constraint persists through institutional inertia (policy announcements, enforcement bureaucracy) despite degraded actual effectiveness.
constraint_indexing:constraint_classification(uk_student_visa_dependents, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PROSPECTIVE STUDENT WITH ALTERNATIVES (SNARE) — Moderate power due to ability to choose competitors (Canada, Australia, US offer dependent access). Trapped for those committed to UK education, mobile for those choosing. d≈0.58, f(d)≈0.75, σ=1.2 → χ≈0.52. Moderate-high extraction: policy extracts via reduced choice set, but students can exit to competitor destinations.
constraint_indexing:constraint_classification(uk_student_visa_dependents, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risk of naturalizing policy as inevitable (family separation is inherent to migration, visa control is inherent to national sovereignty). Structural data contradicts this: extractiveness=0.58, suppression=0.72, theater=0.65, requires_active_enforcement=true. These metrics indicate a contingent institutional arrangement, not a natural law. The 'universal' framing is aspirational (all nations must control borders), but the specific dependent exclusion is a policy choice, not a law of nature.
constraint_indexing:constraint_classification(uk_student_visa_dependents, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_student_visa_dependents_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_student_visa_dependents, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_student_visa_dependents, TR),
    TR >= 0.70.

:- end_tests(uk_student_visa_dependents_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The policy extracts via exclusion: students cannot bring dependents without meeting newly elevated income thresholds (currently £29,600 for one dependent, £20,500 additional per dependent). This is extraction in the classical sense — a cost imposed on target agents that benefits the policy's stated objective (net migration reduction). The level is not maximal (0.75+) because prospective students with resources can pay for separation costs or choose competitors; exit is difficult but not impossible. Suppression (0.72): High. Multiple barriers exist: visa enforcement, income verification, lack of alternative pathways, and the time-bound nature of postgraduate study (1-3 years). Suppressions is not total (1.0) because some students separate by choice and some have private resources. Theater ratio (0.65): Moderate-high. The policy achieves its stated metric (net migration reduction) largely through accounting redefinition — counting dependents differently, not through mechanisms that reduce actual population flows. The performative element is substantial: policy announcements, enforcement bureaucracy, and rhetoric dominate, while actual behavioral change is modest. Theater rises over the interval as the accounting manipulation becomes clearer and alternatives (students choosing competitors) become visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximal perspectival disagreement. International students and families see pure extraction (Snare) — the policy imposes costs with no coordination benefit. The UK higher education sector sees tangled rope — the policy both enables (international recruitment remains legal) and extracts (enrollment declines, reputational damage). The Home Office sees pure coordination (Rope) — setting exclusion rules achieves the stated objective with minimal coercion overhead. The analytical observer risks seeing natural law (Mountain) — family separation is inherent to migration control — but the structural data reveals this as a false summit: the policy choice to exclude dependents (rather than setting income thresholds, reducing fees, or accepting separation as a cost of study) is contingent, not inevitable. The migration policy theater (Piton) sees the constraint as degraded ritual — the policy persists through institutional momentum despite limited effectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   International students: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction for trapped agents. Families: Victim + trapped → d≈0.95, f(d)≈1.42. Absolute extraction (victims with zero exit). UK higher education sector: Mixed (victim + constrained + beneficiary of fees) → d≈0.65, f(d)≈1.00. Sector benefits from tuition but suffers from enrollment decline; constrained because they cannot exit policy without losing recruitment volume. Home Office: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary with minimal extraction cost. Prospective students from competitors: Victim + mobile → d≈0.58, f(d)≈0.75. Can exit but at cost (choosing inferior programme, delaying study, relocating). Migration policy theater: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Theater classification comes from high theater ratio (0.65), not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the policy serves different functions for different agents. For the Home Office, it is pure coordination: efficiently communicating the exclusion rule. For students and families, it is pure extraction: imposing separation costs with no coordination benefit. For universities, it is hybrid: they coordinate international recruitment while being extracted from via enrollment loss. The analytical risk is to naturalize the policy as inevitable (family separation inherent to border control), when it is actually a contingent institutional choice. The policy could achieve similar migration statistics through alternative mechanisms: income thresholds with dependent allowances, extended postgraduate work visas to offset study-time separation, or accepting that family reunion is a legitimate cost of international education. The snare classification is robust because the policy imposes costs (family separation) that do not serve the stated beneficiaries' interests (universities lose students, Home Office gains only accounting metrics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_enrollment_elasticity,
    'What is the actual enrollment decline in UK postgraduate taught programmes attributable to the dependent restriction policy?',
    'Time-series analysis of enrollment data for UK vs competitor nations (Canada, Australia, US) before/after policy implementation; disaggregation by source country and family status',
    'If elasticity > 0.3: policy causes significant outflow, snare classification confirmed. If elasticity < 0.1: policy''s extraction is minimal (many students were already separating), and the snare is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_enrollment_elasticity, empirical, 'Actual enrollment elasticity to dependent restriction policy').

omega_variable(
    net_migration_accounting_methodology,
    'How much of the net migration reduction claimed by the policy is real behavioral change vs accounting redefinition (dependent counts)?',
    'Decomposition of net migration statistics: separate dependent-only reduction from other flows; compare actual emigration rates of students before/after policy',
    'If accounting redefinition > 60% of claimed reduction: policy is piton (performative), not snare (extraction). Theater_ratio should be revised upward to 0.80+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(net_migration_accounting_methodology, empirical, 'Proportion of net migration reduction from accounting vs behavior').

omega_variable(
    family_separation_duration_harms,
    'What are the documented psychological and economic harms of family separation during postgraduate study (1-3 years)?',
    'Qualitative research: interviews with separated families; quantitative: completion rates, mental health metrics, career outcomes for separated vs co-located students',
    'If harms are severe and durable: justifies snare classification and high suppression (0.72). If harms are temporary and reversible: suggests extraction is moderate, not severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_separation_duration_harms, empirical, 'Documented harms of family separation during study period').

omega_variable(
    policy_intent_vs_mechanism_mismatch,
    'Is the stated intent (reduce net migration) genuinely served by the dependent restriction mechanism, or is the mechanism a proxy for political theater?',
    'Analysis of policy rationale documents, impact assessments, and comparative policy effectiveness; assessment of whether alternative mechanisms (income thresholds, course duration) would better serve the stated intent',
    'If mismatch confirmed: policy is piton (ritual) rather than snare (extraction). Theater_ratio should be elevated; classification may shift from snare to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_mechanism_mismatch, conceptual, 'Whether policy mechanism genuinely serves stated intent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_student_visa_dependents, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukvsd_tr_t0, uk_student_visa_dependents, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ukvsd_tr_t2, uk_student_visa_dependents, theater_ratio, 2, 0.55).
narrative_ontology:measurement(ukvsd_tr_t4, uk_student_visa_dependents, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(ukvsd_be_t0, uk_student_visa_dependents, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ukvsd_be_t2, uk_student_visa_dependents, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(ukvsd_be_t4, uk_student_visa_dependents, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_student_visa_dependents, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_student_visa_dependents, uk_postgraduate_visa_duration_cap).
narrative_ontology:affects_constraint(uk_student_visa_dependents, uk_skilled_worker_visa_dependent_income_threshold).

% DUAL FORMULATION NOTE:
% The dependent restriction policy is structurally distinct from the postgraduate visa duration cap (which affects work rights and career progression) and the skilled worker visa dependent income threshold (which affects settlement-track workers). However, all three operate within the same immigration policy ecosystem and share the constraint mechanism of dependent exclusion via income verification. Each has its own epsilon reflecting its target population's exit options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_student_visa_dependents, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
