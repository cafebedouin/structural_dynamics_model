% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State-Hybrid Selective Sharia Codification
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This story instantiates the state_hybrid reading of the
 *   quran_hadith_substrate kernel: the claim that a state's legal legitimacy
 *   rests on selectively codifying classical rulings where cultural resonance
 *   is needed (family, criminal law) while adopting reformist or secular
 *   frameworks where economic integration is needed (commercial,
 *   administrative law), with legitimacy grounded in political sovereignty
 *   rather than doctrinal consistency. This is a distinct constraint from the
 *   traditionalist_taqlid reading (which holds the entire legal order should
 *   follow classical madhhab rulings) and the reformist_ijtihad reading
 *   (which holds contextual ijtihad should govern across all domains,
 *   including family and criminal law) — those are separate stories with
 *   separate ε values and separate stakeholder structures, linked here only
 *   by network reference. The state_hybrid reading's own ε is moderate-low
 *   (0.38): the arrangement functions as a real coordination solution to a
 *   genuine legitimacy/integration tension, but it extracts by fixing the
 *   domain most consequential to individual life outcomes (family status,
 *   criminal liability) as the site of doctrinal rigidity while granting
 *   flexibility precisely where capital, not vulnerable individuals, has
 *   interests at stake.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.52).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Hybrid Selective Sharia Codification").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "religious/legal/political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'eb62c549-0dc5-442d-ae5d-a405a84846ab').
narrative_ontology:cs_kernel_codification('eb62c549-0dc5-442d-ae5d-a405a84846ab', distributed).
narrative_ontology:cs_authority_grounding('eb62c549-0dc5-442d-ae5d-a405a84846ab', extraction).
narrative_ontology:cs_interpretation_layer_present('eb62c549-0dc5-442d-ae5d-a405a84846ab').
narrative_ontology:cs_reading_relation('eb62c549-0dc5-442d-ae5d-a405a84846ab', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('eb62c549-0dc5-442d-ae5d-a405a84846ab', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_axiom('eb62c549-0dc5-442d-ae5d-a405a84846ab', foundational, sovereign_discretion_over_doctrinal_scope).
narrative_ontology:cs_axiom_status(sovereign_discretion_over_doctrinal_scope, holdable).
narrative_ontology:cs_axiom_grounding('eb62c549-0dc5-442d-ae5d-a405a84846ab', sovereign_discretion_over_doctrinal_scope, conventional).
narrative_ontology:cs_axiom('eb62c549-0dc5-442d-ae5d-a405a84846ab', foundational, legitimacy_derives_from_selective_symbolic_codification_not_comprehensive_fidelity).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_selective_symbolic_codification_not_comprehensive_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('eb62c549-0dc5-442d-ae5d-a405a84846ab', legitimacy_derives_from_selective_symbolic_codification_not_comprehensive_fidelity, instrumental).
narrative_ontology:cs_reference_frame('eb62c549-0dc5-442d-ae5d-a405a84846ab', post_colonial_dual_legal_settlement).
narrative_ontology:cs_drift_state('eb62c549-0dc5-442d-ae5d-a405a84846ab', contemporary_globalized_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb62c549-0dc5-442d-ae5d-a405a84846ab', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_ruling_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, clerical_establishment_appointees).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_capital_interests).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_jurists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, women_under_codified_family_law).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, criminal_defendants_under_hudud_provisions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines which classical rulings get codified into family and criminal law (where cultural legitimacy is most needed) and which domains get reformist or secular treatment (commercial law, administrative regulation, foreign investment codes). Draws legitimacy from appearing to honor sharia while retaining full discretion to bend commercial law toward whatever policy serves state revenue and international integration. Can revise the boundary whenever politically convenient.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_ruling_elites, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, state_ruling_elites, beneficiary).

% State-salaried religious scholars staff the family and criminal courts, issuing fatwas that ratify the state's selective codification. Their institutional standing depends on state patronage; they gain authority and income from administering the family/criminal domain but have no jurisdiction over commercial law, which they are structurally excluded from contesting.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, clerical_establishment_appointees, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, clerical_establishment_appointees, agenda_setter).

% Domestic and foreign business actors benefit from secular or reformist commercial and administrative frameworks that allow interest-bearing finance, standardized contract law, and international arbitration — insulated entirely from classical fiqh prohibitions on riba and speculative contracts. They can relocate capital if commercial law drifts toward classical strictness, giving them leverage the family-law domain's captive population lacks.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_capital_interests, beneficiary,
    organized, biographical, mobile, global).

% Hold that sharia is a comprehensive legal order that cannot be selectively applied — an economy run on interest and administrative law untouched by fiqh is, on their own terms, a partial abandonment of the din. They are tolerated in family/criminal courts where their doctrine is instrumentalized, but their objections to secular commercial law are marginalized as politically inconvenient. They cannot exit the polity without losing their institutional base entirely.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    moderate, generational, constrained, national).

% Argue classical family and criminal rulings should themselves be subject to contextual ijtihad given their harsh application to women and defendants; the state suppresses this reading in precisely the domains where it codifies classical rulings, because reformist critique in family/criminal law threatens the legitimacy narrative the state depends on. Face censorship, loss of teaching posts, or prosecution for challenging codified rulings; their commercial-law latitude is irrelevant to them since their concern is human-status law.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_jurists, payer,
    powerless, biographical, trapped, national).

% Live under codified classical rulings on marriage, divorce, custody, and inheritance that the state enforces through family courts, while having no access to the flexible, reform-oriented legal reasoning the same state applies confidently in commercial contexts. Exit requires emigration or informal circumvention; the domain the state chose to freeze is precisely the one governing their personal status.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, women_under_codified_family_law, payer,
    powerless, biographical, trapped, national).

% Subject to codified classical criminal penalties (hudud, qisas) selectively retained for symbolic and legitimacy purposes even where evidentiary standards or contemporary circumstances would, under reformist or even classical procedural safeguards, counsel against application. Have no exit from the jurisdiction and no standing to contest the doctrinal selection that produced their sentence.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, criminal_defendants_under_hudud_provisions, payer,
    powerless, immediate, trapped, national).

% Document the disparate treatment between commercial-law modernization and family/criminal-law retention, but have no enforcement standing inside the state's domestic legal order; their critiques are absorbed as external interference rather than integrated into the domestic legitimacy calculus.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_human_rights_bodies, excluded,
    organized, generational, analytical, global).

% Periodically asked to adjudicate the boundary between sharia-derived and secular-derived law; their rulings can shift which domains fall under which framework but generally defer to the sovereign's discretion over the boundary itself rather than imposing doctrinal consistency.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_ruling_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working division of legal labor: family and criminal law draw on culturally resonant classical rulings that confer legitimacy and social stability, while commercial and administrative law adopt frameworks compatible with global trade, finance, and bureaucratic governance — allowing the state to participate in the international economy without triggering a legitimacy crisis over abandoning sharia wholesale.
% TRANSFER_FUNCTION: Moves legitimacy and political stability to the state and its clerical appointees (who administer the family/criminal domain as sharia-compliant) and moves economic flexibility to commercial capital, while moving the costs of doctrinal freezing onto those governed by family and criminal law (women, criminal defendants) and moves the cost of ideological marginalization onto both traditionalist and reformist scholars whose comprehensive visions are each partially suppressed.
% ABSENT_VOICES: Reformist jurists and traditionalist scholars are each present in the discourse but excluded from setting policy in the domain where their doctrine actually applies — traditionalists cannot extend fiqh into commercial law, reformists cannot extend contextual ijtihad into family/criminal law. Women subject to family courts and hudud defendants have essentially no voice in the boundary-setting process at all.
% DISAPPEARANCE_RATIONALE: If the selective hybrid arrangement collapsed, the state would have to choose either comprehensive classical codification (satisfying traditionalists, alienating commercial capital and international partners) or comprehensive secular/reformist codification (satisfying reformists, destabilizing the legitimacy narrative built on family/criminal sharia visibility) — either resolution would restructure court jurisdiction, commercial contract enforceability, and family status law simultaneously.
% FOUNDING_PROBLEM: Post-colonial and modernizing states needed a way to retain religious legitimacy among populations for whom sharia symbolizes cultural and political sovereignty, while simultaneously integrating into global commercial and financial systems that classical fiqh's stricter positions (on interest, contract form, evidentiary rules) would complicate or forbid.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and political scientists studying post-colonial legal pluralism (outside both the state elite and the clerical establishment) corroborate that the family/criminal-commercial split tracks state legitimacy and economic-integration incentives rather than doctrinal coherence; traditionalist and reformist scholars, from opposite directions, both independently corroborate that the split is instrumentalized rather than principled, even though they disagree about which domain should be extended.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) sits in the low-to-moderate band because the arrangement genuinely solves a real coordination problem — a state cannot simultaneously satisfy full classical codification and full commercial modernization, and some allocation of domains is unavoidable — but the specific allocation systematically shields capital's interests (mobile, exit-capable) while freezing the domain governing the least mobile, least powerful parties (women in family courts, criminal defendants). Suppression (0.52) is moderate and variable, reflecting the omega on regime-dependent enforcement intensity: some states enforce the split loosely, others prosecute reformist critique of family/criminal codification aggressively. Theater ratio (0.48) is elevated and rising because a growing share of the state's family/criminal-law codification functions performatively — signaling sharia fidelity to a domestic audience — rather than reflecting settled doctrinal conviction, while the commercial-law side quietly continues secularizing without matching rhetorical cover.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, this is a Rope — a pragmatic, defensible allocation of legal domains solving a real tension. From the traditionalist and reformist seats, it computes as extraction: each sees their comprehensive vision truncated by a boundary drawn for reasons external to doctrine. From the seat of women under family law and hudud defendants, the same arrangement is closer to a Snare — a fixed, unmovable domain of law imposed on them with no exit and no doctrinal flexibility, justified by a legitimacy logic they had no voice in constructing. The engine should compute these divergent seat classifications from the structural power/exit data rather than from any single narrative frame.
 *
 * DIRECTIONALITY LOGIC:
 *   State ruling elites and clerical appointees sit near the beneficiary end: they set the boundary and collect legitimacy/patronage from it, and can redraw the line whenever expedient (arbitrage-grade exit for the state itself). Commercial capital interests are also beneficiaries, but through a different mechanism — global mobility rather than domestic legitimacy capture. Traditionalist scholars and reformist jurists are both victims, but through opposite mechanisms: traditionalists are victimized by the arrangement's refusal to extend fiqh into commercial law (their vision is truncated), reformists are victimized by the arrangement's refusal to extend contextual reasoning into family/criminal law (their critique is suppressed exactly where it would matter most). Women and hudud defendants are the most trapped: their exit options are near-zero and the domain governing them is the one selected for maximal doctrinal freezing, which the derivation correctly pushes toward high effective extraction despite the modest base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling religious legitimacy with economic integration — remains genuinely live in most state contexts (hence founding_problem_status: live, not dead), which is what prevents this from being classified as pure inertial piton or pure snare. The hybrid is not simply extraction dressed as coordination: it does solve an actual governance problem states without a version of this split have struggled to solve. But mandatrophy risk is real and rising (theater_ratio climbing to 0.48): the original coordination logic (which domains need which framework, and why) is increasingly substituted by a purely political calculus of which domain codification maximizes short-term regime legitimacy, independent of any coherent theory of which areas of law actually require classical treatment. Tracking this drift required aligning theater_ratio, base_extractiveness, and suppression_requirement on a single time grid rather than measuring them independently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_variable_suppression_intensity,
    'Does the suppression intensity applied to reformist critique of family/criminal codification vary systematically with regime type (authoritarian vs. quasi-democratic) or with external legitimacy pressure (e.g., proximity to elections, international scrutiny)?',
    'Cross-national comparison of prosecution rates for reformist scholars, censorship incidents, and family-court doctrinal rigidity across regime types over a 20-30 year window.',
    'If suppression tracks regime insecurity rather than doctrinal commitment, it strengthens the reading that the hybrid split is a legitimacy-management tool rather than a principled jurisprudential position — reinforcing the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_variable_suppression_intensity, empirical, 'Whether suppression severity is regime-contingent rather than doctrine-contingent.').

omega_variable(
    coordination_or_capture_boundary_origin,
    'Was the specific family/criminal vs. commercial/administrative boundary chosen because those domains genuinely require different treatment, or because that particular split happened to protect capital mobility while capturing legitimacy through the domain most visible to the domestic public?',
    'Historical and comparative analysis of which domains states actually chose to hybridize versus alternative splits (e.g., some states codify classical commercial prohibitions like riba bans while secularizing family law) — if the family/criminal choice is near-universal despite varying local conditions, it suggests capture rather than domain-specific necessity.',
    'If the boundary is capture-driven rather than functionally necessary, the coordination function claimed in six_questions is substantially weaker than authored, and the constraint moves closer to snare for the vulnerable payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_capture_boundary_origin, conceptual, 'Whether the specific domain-split is functionally necessary or politically opportunistic.').

omega_variable(
    reading_stability_under_regime_change,
    'Does the state_hybrid reading persist as the dominant kernel-reading across regime transitions, or does it tend to collapse toward traditionalist_taqlid or reformist_ijtihad readings during periods of political instability?',
    'Track kernel-reading dominance across documented regime transitions (revolutions, coups, democratic transitions) in multiple state contexts.',
    'If the hybrid reading is inherently unstable and collapses toward one sibling reading under stress, this suggests the state_hybrid reading is a transitional equilibrium rather than a durable independent framework, which would bear on whether this constraint should itself carry sunset characteristics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_under_regime_change, empirical, 'Durability of the hybrid reading across political transitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.32).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__state_hybrid, theater_ratio, 8, 0.36).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__state_hybrid, theater_ratio, 16, 0.4).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__state_hybrid, theater_ratio, 24, 0.43).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__state_hybrid, theater_ratio, 32, 0.46).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__state_hybrid, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__state_hybrid, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__state_hybrid, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__state_hybrid, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__state_hybrid, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__state_hybrid, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__state_hybrid, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__state_hybrid, suppression_requirement, 32, 0.51).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_ijtihad).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_hadith_substrate kernel. traditionalist_taqlid claims comprehensive classical fiqh authority across all domains (low ε, high accessibility_collapse, framed as mountain by its own adherents). reformist_ijtihad claims contextual ijtihad should govern universally including family/criminal law (moderate-to-high ε reflecting contested authority, framed as rope/tangled_rope depending on state suppression). state_hybrid (this story) claims neither doctrinal position is primary; it grounds legitimacy in sovereignty's capacity to allocate frameworks domain-by-domain, producing a distinct ε (0.25-0.45 band, high cross-state variability) and a distinct victim set spanning both sibling readings' adherents. The three stories share the same underlying textual kernel (Quran/hadith corpus) but instantiate structurally different constraints with different beneficiaries, victims, and enforcement profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
