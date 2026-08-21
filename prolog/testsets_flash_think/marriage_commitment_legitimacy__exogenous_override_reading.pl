% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Coercion of Plural Marriage Practice (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the `exogenous_override_reading` of the
 *   `marriage_commitment_legitimacy` kernel, focusing on the federal
 *   government's coercive assertion of legal supremacy over the LDS Church's
 *   practice of plural marriage, while acknowledging the theological doctrine
 *   itself remained internally unchanged for adherents. The Manifesto of 1890
 *   is viewed as an act of institutional capitulation under duress, rather
 *   than a genuine internal reinterpretation of doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Coercion of Plural Marriage Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '7decf85e-5080-438b-8293-0955752cdc97').
narrative_ontology:cs_kernel_codification('7decf85e-5080-438b-8293-0955752cdc97', formalized).
narrative_ontology:cs_authority_grounding('7decf85e-5080-438b-8293-0955752cdc97', extraction).
narrative_ontology:cs_reading_relation('7decf85e-5080-438b-8293-0955752cdc97', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('7decf85e-5080-438b-8293-0955752cdc97', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('7decf85e-5080-438b-8293-0955752cdc97', foundational, federal_law_is_supreme_over_religious_practice).
narrative_ontology:cs_axiom_status(federal_law_is_supreme_over_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('7decf85e-5080-438b-8293-0955752cdc97', federal_law_is_supreme_over_religious_practice, conventional).
narrative_ontology:cs_axiom('7decf85e-5080-438b-8293-0955752cdc97', foundational, divine_command_is_immutable_and_unconditional).
narrative_ontology:cs_axiom_status(divine_command_is_immutable_and_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('7decf85e-5080-438b-8293-0955752cdc97', divine_command_is_immutable_and_unconditional, theological).
narrative_ontology:cs_reference_frame('7decf85e-5080-438b-8293-0955752cdc97', federal_legal_supremacy_over_religious_practice).
narrative_ontology:cs_drift_state('7decf85e-5080-438b-8293-0955752cdc97', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7decf85e-5080-438b-8293-0955752cdc97', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_institution).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserted legal supremacy over religious practice, using legislative and judicial power to suppress plural marriage. Benefited from establishing its authority and aligning social norms with broader societal expectations. Actively enforced anti-polygamy laws.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Faced severe legal and economic penalties, including disincorporation and confiscation of assets, leading to the official suspension of plural marriage practice. Capitulated under duress to preserve the institution, bearing the cost of altered practice and internal dissent.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_institution, payer,
    institutional, generational, constrained, national).

% Many members had deeply held theological commitments to plural marriage and faced personal disruption, legal persecution, and a crisis of faith as the practice was suspended. Their identity was often fused with the practice, making 'exit' from the faith unthinkable.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership_adherents, payer,
    powerless, biographical, identity_locked, national).

% A segment of the LDS membership and leadership who believed the doctrine of plural marriage was immutable and divinely commanded. They resisted the capitulation, often facing excommunication or forming splinter groups, but were excluded from the institutional decision-making that led to the Manifesto.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, theological_conservatives_within_lds, excluded,
    moderate, biographical, identity_locked, national).

% Historians, legal scholars, and journalists who analyzed the conflict between the federal government and the LDS Church, documenting the coercive tactics and the institutional response. They provide an external perspective on the power dynamics at play.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, secular_observers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal government coordinated its legal authority to enforce a uniform national standard of marriage, asserting its supremacy over religious practices deemed contrary to public law and morality.
% TRANSFER_FUNCTION: Transferred institutional autonomy and the right to practice plural marriage from the LDS Church to the federal government's legal and social control, extracting compliance and conformity.
% ABSENT_VOICES: Theological conservatives within the LDS church, who viewed plural marriage as a divine command, were effectively silenced or marginalized within the institutional decision to suspend the practice. They would have argued for continued adherence to doctrine regardless of federal pressure.
% DISAPPEARANCE_RATIONALE: If federal coercion had vanished overnight, the LDS Church would likely have continued or reinstated the practice of plural marriage, fundamentally altering the social and legal landscape of the American West and challenging the federal government's authority over religious institutions.
% FOUNDING_PROBLEM: The federal government's problem was the perceived challenge to its legal and moral authority posed by the practice of plural marriage within the Utah Territory, which was seen as an affront to national social norms and a barrier to statehood.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars (outside both the federal government and the LDS Church) corroborate the federal government's consistent efforts to assert legal supremacy and suppress plural marriage, indicating the underlying tension between religious freedom and federal authority remains a live issue in various forms.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the federal government successfully extracted institutional compliance and the abandonment of a core religious practice. Suppression is very high (0.92) due to the severe legal, economic, and social penalties imposed by the federal government, leaving the LDS Church with virtually no viable exit options. Theater ratio is moderate (0.40) as the public suspension of practice involved a degree of performative compliance to appease federal authorities, even as some private adherence or theological resistance persisted. Accessibility collapse is high (0.75) because the federal government effectively closed off all legal avenues for continuing the practice. Resistance is moderate (0.60) reflecting the internal struggle and initial defiance within the LDS community before capitulation.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a necessary assertion of law and order, a legitimate exercise of state power. From the perspective of the LDS Church and its adherents, particularly those who maintained the theological immutability of plural marriage, it was an act of coercion and duress, a forced suspension of divine command. The engine will compute this divergence from the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary (d=0.0) as it achieved its policy goals and asserted legal supremacy. The LDS Church institution and its adherents are the primary targets (d=1.0), bearing the full cost of forced compliance and doctrinal abandonment. Theological conservatives are excluded, their voices suppressed by the institutional capitulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare prevents mislabeling this as a Rope (genuine coordination) or even a Tangled Rope (some coordination with extraction). The high suppression and clear victims, coupled with the coercive nature of the federal action, firmly place it in the Snare category, highlighting the extractive nature of the federal government's assertion of authority over religious practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_integrity_vs_practice,
    'To what extent did the suspension of plural marriage practice under duress genuinely leave the theological doctrine ''unchanged'' in the minds of adherents, versus subtly altering its interpretation or perceived immutability over time?',
    'Longitudinal studies of LDS theological discourse, personal diaries, and oral histories from the post-Manifesto era, analyzing shifts in understanding of revelation and prophetic authority.',
    'If the doctrine''s internal integrity was significantly compromised, the ''exogenous override'' reading''s claim of ''doctrine unchanged'' would be weakened, potentially shifting towards a ''hybrid pragmatic'' or even ''endogenous reinterpretation'' reading over the long term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_integrity_vs_practice, empirical, 'Theological integrity of doctrine under coercive practice suspension.').

omega_variable(
    extent_of_federal_coercion,
    'What was the precise balance between federal coercive pressure and any internal institutional agency or strategic calculation within the LDS Church in the decision to issue the Manifesto?',
    'Access to previously unreleased archival documents from both federal and LDS Church sources, combined with detailed counterfactual historical analysis.',
    'If significant internal agency or strategic calculation is revealed, the ''exogenous override'' reading''s emphasis on pure coercion would be tempered, potentially strengthening the ''hybrid pragmatic'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_federal_coercion, empirical, 'Balance of external coercion vs. internal agency in institutional capitulation.').

omega_variable(
    suppression_internalized_vs_structural,
    'Was the suppression of plural marriage primarily structural (legal penalties, asset confiscation) or did it become internalized (cognitive patterns, identity shifts) within the LDS community, persisting after direct federal pressure eased?',
    'Post-1904 sociological and anthropological studies of LDS communities, examining the persistence of anti-polygamy norms and the social costs of deviation even in the absence of direct federal enforcement.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the community carried the suppression with them, impacting subsequent generations'' adherence to the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism for religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(marr_tr_t1892, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1892, 0.33).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1895, 0.36).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1898, 0.38).
narrative_ontology:measurement(marr_tr_t1901, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1901, 0.39).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1904, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.75).
narrative_ontology:measurement(marr_be_t1892, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1892, 0.78).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1895, 0.81).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1898, 0.83).
narrative_ontology:measurement(marr_be_t1901, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1901, 0.84).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(marr_su_t1892, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1892, 0.87).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1895, 0.89).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1898, 0.9).
narrative_ontology:measurement(marr_su_t1901, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1901, 0.91).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1904, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
