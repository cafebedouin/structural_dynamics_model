% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Permanent Rent Claim on Departmental Resources
 *   domain: higher_education_governance/labor_economics
 *
 * SUMMARY:
 *   This story generates the institutional_extraction_reading of the
 *   tenure_contract kernel: tenure treated as a permanent property claim held
 *   by an incumbent cohort ('early winners') that fixes departmental resource
 *   allocation regardless of shifting enrollment, funding, or teaching
 *   demand. Two sibling readings of the same kernel text and practice —
 *   academic_freedom_reading (tenure as protection enabling risky inquiry)
 *   and demographic_reproduction_reading (tenure review as demographic
 *   gatekeeping) — are NOT part of this story; they are separate constraints
 *   linked via network.affects_constraints. This reading's ε is high and
 *   stable-to-rising because the extraction mechanism (permanent claim on a
 *   finite line, funded by growing reliance on cheap contingent instruction)
 *   has intensified over recent decades as tenure-track hiring has stagnated
 *   relative to enrollment and adjunct reliance has grown.
 *
 * KEY AGENTS:
 *   - tenured_faculty: institutional/arbitrage — holds the permanent claim, votes on resource allocation, largely insulated from institutional financial pressure
 *   - contingent_faculty: powerless/trapped — bears the flexibility cost the tenured claim cannot absorb, no governance voice
 *   - untenured_junior_faculty: moderate/constrained — competes for the shrinking pool of lines under rules set by incumbents
 *   - students: powerless/constrained — funds the arrangement through tuition without proportional instructional benefit
 *   - university_administration: institutional/constrained — nominally sets budgets but cannot unilaterally reallocate tenured lines
 *   - prospective_academic_labor_market_entrants: powerless/trapped, excluded — foreclosed from the tenure-track market entirely, no seat in governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.58).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Permanent Rent Claim on Departmental Resources").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0').
narrative_ontology:cs_kernel_codification('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', formalized).
narrative_ontology:cs_authority_grounding('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', practice).
narrative_ontology:cs_interpretation_layer_present('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0').
narrative_ontology:cs_reading_relation('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', foundational, permanence_decoupled_from_ongoing_justification).
narrative_ontology:cs_axiom_status(permanence_decoupled_from_ongoing_justification, holdable).
narrative_ontology:cs_axiom_grounding('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', permanence_decoupled_from_ongoing_justification, empirically_contingent).
narrative_ontology:cs_axiom('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', secondary, incumbent_claim_priority_over_reallocation_need).
narrative_ontology:cs_axiom_status(incumbent_claim_priority_over_reallocation_need, holdable).
narrative_ontology:cs_axiom_grounding('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', incumbent_claim_priority_over_reallocation_need, conventional).
narrative_ontology:cs_reference_frame('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', protective_permanence_against_retaliation).
narrative_ontology:cs_drift_state('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', contemporary_adjunctification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a1bb0bf-2826-465c-8dd2-5b4e8f52e3e0', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, untenured_junior_faculty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, untenured_junior_faculty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a permanent, near-unremovable claim on a departmental faculty line, teaching load allocation, and governance votes once granted tenure. They vote on hiring, curriculum, and resource allocation, and their positions are essentially immune to enrollment shifts, budget pressure, or performance decline after the tenure decision. Many can move to peer institutions if unhappy (arbitrage), but rarely need to since the position itself is secure regardless of departmental fortunes.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty, agenda_setter).

% Teach a large and growing share of instructional hours on semester-to-semester or year-to-year contracts, without job security, benefits parity, or governance voice. They absorb the flexibility that the institution needs precisely because tenured lines cannot be reallocated or eliminated when enrollment or funding shifts. Exit means leaving academia entirely for most, since the tenure-track hiring pipeline they would need to escape into is itself gated by the same tenured faculty who vote on new lines.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, national).

% Compete for a shrinking number of tenure-track lines, subject to review criteria set and administered by the existing tenured cohort. If they win tenure they join the beneficiary class; if not, they are pushed out entirely (up-or-out), often after 6-7 years of specialized investment with a narrow external market. Their exit options are constrained by field-specific skill investment and the small number of tenure-track openings nationally.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, untenured_junior_faculty, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, untenured_junior_faculty, beneficiary).

% Pay tuition that increasingly funds administrative overhead and legacy tenured salaries rather than proportionally funding instruction, while an increasing share of their actual classroom instruction is delivered by underpaid, high-turnover contingent faculty. They have some choice of institution but limited visibility into the tenure-driven cost structure behind tuition, and switching institutions after enrollment is costly.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    powerless, immediate, constrained, national).

% Sets budgets and negotiates with tenured faculty senates but cannot easily eliminate or reallocate tenured lines even during financial exigency without protracted governance battles and reputational risk. Increasingly relies on adjunct hiring as the only lever of flexibility left, which both accommodates and entrenches the tenure rigidity rather than challenging it.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% PhD holders and doctoral candidates who would compete for tenure-track lines that do not exist because incumbents occupy them indefinitely. They have no seat in departmental or university governance and their interests are not represented in tenure and promotion policy discussions.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, prospective_academic_labor_market_entrants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Tenure was intended to prevent politically or personally motivated dismissal of individual researchers, coordinating a norm of long-horizon inquiry protection across an institution.
% TRANSFER_FUNCTION: Moves job security, governance power, and a durable claim on departmental budget lines to the tenured cohort, funded by tuition increases, adjunct wage suppression, and foreclosed tenure-track openings for the next generation of scholars.
% ABSENT_VOICES: Contingent faculty have no vote in the tenure and promotion committees that set the rules governing their own precarity; prospective PhDs competing for future lines have no institutional voice at all in current resource allocation.
% DISAPPEARANCE_RATIONALE: If tenure's permanent claim disappeared overnight, departments could reallocate lines to shifting enrollment demand, contingent faculty could be converted to more stable multi-year roles funded by savings on legacy compensation, and the current bimodal structure (permanent haves / precarious have-nots) would likely compress toward a different equilibrium — though academic freedom protections would need a substitute mechanism.
% FOUNDING_PROBLEM: Early 20th-century academic freedom cases (e.g., faculty fired for unpopular economic or political views) created the perceived need for a durable employment protection insulating scholars from institutional or political retaliation for their research and teaching.
% FOUNDING_PROBLEM_CORROBORATION: Tenured faculty and faculty senates attest the academic-freedom problem remains live and cite ongoing political pressure on researchers. Independent labor economists, university financial officers, and AAUP contingent-faculty task force reports (produced partly by the affected precarious faculty themselves, so only partially outside the interested parties) document that the protective function has become decoupled from its cost structure — the same protection could plausibly be delivered at lower rent through post-tenure review or fixed-term renewable contracts, but no fully disinterested outside corroboration exists since the strongest institutional voices are the tenured beneficiaries themselves.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects that the tenured claim is decoupled from ongoing departmental need or performance once granted — it is a standing draw on the budget regardless of enrollment trend, teaching load, or research output post-tenure. Suppression (0.58) is moderate-high: the mechanism is enforced through governance rules (tenured faculty vote on their own committee structures and promotion/dismissal standards) and through up-or-out review that filters out challengers rather than incumbents. Theater ratio (0.40) captures that post-tenure review exists at many institutions but is widely reported as rarely leading to real consequences — a performative check layered onto an otherwise fixed claim. Accessibility collapse (0.50) is moderate: alternative employment models (renewable multi-year contracts, fixed-term research faculty) exist and are used at some institutions, so alternatives have not fully collapsed, but the tenure norm dominates prestige hiring at research universities. Resistance (0.55) reflects real, organized pushback from contingent-faculty unions and some administrators seeking flexibility, set against strong faculty-senate defense.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are declared beneficiaries: the constraint subsidizes them with a durable claim independent of ongoing performance, and their exit options (arbitrage — could move to another institution) keep their derived directionality low without requiring an override. Contingent faculty and untenured junior faculty are declared victims: they bear the flexibility costs the tenured claim displaces, and their exit options (trapped/constrained) push derived directionality toward the target end. Students are declared victims via the transfer function (tuition funding a rent structure rather than proportional instruction) despite having some institutional choice, because that choice is exercised with poor visibility into the underlying cost driver and exit is costly post-enrollment.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification must not treat all of tenure's original coordination function (protecting inquiry from retaliation) as fully evaporated — some academic freedom protection likely remains functionally live even under this reading, which is why founding_problem_status is authored as 'contested' rather than flatly 'dead.' What distinguishes this reading as extraction-dominant is the structural fact that the protective mechanism has been decoupled from a sunset or reallocation mechanism: even where academic freedom is no longer under acute threat for a given tenured individual, the resource claim persists unconditionally, which is the rent component this story isolates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_protection_separability,
    'Is the resource-extraction effect of tenure separable from its academic-freedom protective function, or are they mechanically the same lever (permanence) viewed from different angles?',
    'Compare institutions using post-tenure review with real consequences, renewable long-term contracts, or German-style habilitation-then-permanent-professorship models against traditional US tenure on measures of both academic freedom incidents and resource reallocation flexibility.',
    'If separable, the extraction component identified here could in principle be reformed (e.g. via periodic review with real teeth) without weakening protection, supporting a tangled_rope rather than pure snare classification. If inseparable, any reform that restores allocative flexibility necessarily weakens the protective function, making the sibling academic_freedom_reading and this reading two faces of one indivisible mechanism rather than genuinely decomposable constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_protection_separability, conceptual, 'Whether the extraction and protection functions of tenure share one mechanism or are structurally separable.').

omega_variable(
    tenure_line_scarcity_causal_direction,
    'Does the growth of contingent faculty reliance CAUSE the shrinking of tenure-track lines (institutions substituting cheap labor), or does declining public funding CAUSE both the freeze on tenure-track hiring and the rise of contingent hiring as independent effects of the same budget constraint?',
    'Panel analysis of institutional budgets, state appropriation levels, and tenure-track/contingent hiring ratios over multiple decades, controlling for public funding shocks.',
    'If tenure rigidity itself is the primary driver (administrations substitute contingent labor specifically because tenured lines cannot be touched), this reading''s extraction claim strengthens considerably. If external funding collapse is the dominant driver and tenure rigidity is a secondary amplifier, the extraction attributed to tenure specifically should be discounted relative to broader higher-ed funding austerity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tenure_line_scarcity_causal_direction, empirical, 'Whether tenure rigidity or external funding austerity is the primary driver of contingent faculty growth.').

omega_variable(
    early_winner_arbitrariness,
    'Is the tenured cohort''s status as ''early winners'' a matter of arbitrary timing (they entered during a more favorable hiring market) or does it track genuine, sustained differences in scholarly merit that would justify the permanence of their claim on ongoing grounds?',
    'Compare research productivity, citation trajectories, and teaching evaluations of tenured cohorts against contingent and untenured faculty controlling for years since terminal degree and field.',
    'If the tenured cohort''s permanent claim tracks no ongoing merit differential beyond initial hiring-market timing, the extraction framing is strongly supported. If it tracks a genuine and durable merit gap, some of the measured extraction may reflect legitimate differential value rather than pure rent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(early_winner_arbitrariness, empirical, 'Whether tenured incumbency reflects durable merit or arbitrary timing of entry into a favorable labor market.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__institutional_extraction_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__institutional_extraction_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__institutional_extraction_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__institutional_extraction_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__institutional_extraction_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__institutional_extraction_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__institutional_extraction_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__institutional_extraction_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__institutional_extraction_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__institutional_extraction_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__institutional_extraction_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__institutional_extraction_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the tenure_contract kernel. academic_freedom_reading treats the same permanence mechanism as coordination that decouples inquiry from institutional retaliation (low-to-moderate ε from that angle). demographic_reproduction_reading treats the peer-review gate within the same mechanism as demographic gatekeeping via unmoored 'fit' criteria. institutional_extraction_reading (this story) treats the resource-permanence effect as rent extraction by incumbents at the expense of contingent labor and students. All three read the identical tenure practice; ε differs sharply across readings because each isolates a different structural claim about what the permanence does. Link maintained per the ε-invariance principle: do not average or reconcile ε across these three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
