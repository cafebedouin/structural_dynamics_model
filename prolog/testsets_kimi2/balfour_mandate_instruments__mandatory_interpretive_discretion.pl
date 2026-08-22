% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion (Palestine Mandate)
 *   domain: international_law/colonial_administration
 *
 * SUMMARY:
 *   This constraint story instantiates the mandatory_interpretive_discretion
 *   reading of the balfour_mandate_instruments kernel. Under this reading,
 *   the operational constraint is not the Mandate text itself but the British
 *   Crown's unilateral authority to adjudicate between competing readings of
 *   that text without external review. Both Arab and Zionist communities are
 *   locked into a system where policy oscillates across White Papers and land
 *   regimes, generating strategic uncertainty that prevents either community
 *   from negotiating from a stable baseline. The British mandatory authority
 *   benefits from policy flexibility and divide-and-rule leverage. This is
 *   authored as a moderate-extractive snare: the coordination function
 *   (preventing immediate anarchy) is cover for sustained imperial extraction
 *   through interpretive monopoly.
 *
 * KEY AGENTS:
 *   - British mandatory authority: agenda_setter/beneficiary (institutional/arbitrage) â holds unreviewable interpretive discretion
 *   - Arab Palestinian community: primary payer (organized/trapped) â cannot secure fixed land or immigration rights
 *   - Zionist Jewish community: primary payer (organized/constrained) â cannot secure guaranteed national home development
 *   - League of Nations PMC: excluded (institutional/constrained) â structurally present but without binding review authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.6).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.7).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion (Palestine Mandate)").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, 'b3228277-cd18-46f2-bfb0-8707f67d6971').
narrative_ontology:cs_kernel_codification('b3228277-cd18-46f2-bfb0-8707f67d6971', fixed_text).
narrative_ontology:cs_authority_grounding('b3228277-cd18-46f2-bfb0-8707f67d6971', lineage).
narrative_ontology:cs_interpretation_layer_present('b3228277-cd18-46f2-bfb0-8707f67d6971').
narrative_ontology:cs_reading_relation('b3228277-cd18-46f2-bfb0-8707f67d6971', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('b3228277-cd18-46f2-bfb0-8707f67d6971', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('b3228277-cd18-46f2-bfb0-8707f67d6971', foundational, mandatory_interpretive_supremacy).
narrative_ontology:cs_axiom_status(mandatory_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b3228277-cd18-46f2-bfb0-8707f67d6971', mandatory_interpretive_supremacy, conventional).
narrative_ontology:cs_axiom('b3228277-cd18-46f2-bfb0-8707f67d6971', foundational, exclusion_of_external_mandate_review).
narrative_ontology:cs_axiom_status(exclusion_of_external_mandate_review, holdable).
narrative_ontology:cs_axiom_grounding('b3228277-cd18-46f2-bfb0-8707f67d6971', exclusion_of_external_mandate_review, conventional).
narrative_ontology:cs_reference_frame('b3228277-cd18-46f2-bfb0-8707f67d6971', mandatory_interpretive_supremacy).
narrative_ontology:cs_drift_state('b3228277-cd18-46f2-bfb0-8707f67d6971', late_mandate_1948, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b3228277-cd18-46f2-bfb0-8707f67d6971', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sole authority to interpret the Palestine Mandate instruments, issue binding policy through White Papers and administrative orders, and shift land and immigration regimes without external appellate review. Derives imperial strategic flexibility and divide-and-rule leverage from maintaining interpretive ambiguity.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority, beneficiary).

% Seeks fixed protection of land tenure and political rights under the Mandate text. Faces repeated unilateral reinterpretations of those rights through British policy oscillations. Cannot appeal to an external tribunal or enforce a stable textual reading. Geographically and politically confined within the mandate territory.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_community, payer,
    organized, generational, trapped, national).

% Seeks guaranteed facilitation of Jewish immigration and land settlement under the national home promise. Experiences the same interpretive oscillation as the Arab community, with British policy shifting between facilitation and restriction. Has international diplomatic channels but no exit from the mandatory legal framework itself.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_jewish_community, payer,
    organized, generational, constrained, national).

% Receives mandatory annual reports and may question British administrators in Geneva, but lacks binding authority to overturn interpretive decisions or adjudicate between competing readings of the Mandate text. Its recommendations are advisory and routinely disregarded when they constrain imperial flexibility.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_nations_pmc, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single imperial administrative authority to adjudicate between two incompatible national claims to the same territory, substituting procedural order for direct inter-communal anarchy in the immediate post-Ottoman vacuum.
% TRANSFER_FUNCTION: Transfers policy certainty, self-determination, and stable legal baselines from both Arab and Zionist communities to the British Crown, in exchange for provisional and reversible imperial decisions on land, immigration, and political institutions.
% ABSENT_VOICES: The League of Nations Permanent Mandates Commission is structurally present but excluded from binding review; both national communities are denied standing to compel a fixed textual reading or independent arbitration.
% DISAPPEARANCE_RATIONALE: If British interpretive discretion vanished overnight, the competing readings of the Mandate would be contested directly between the communities or through alternative arbitration mechanisms. The path-dependent lock-in and forced strategic ambiguity that structured three decades of negotiations would collapse, and the political field would reorganize around either direct bilateral contest or fixed textual enforcement.
% FOUNDING_PROBLEM: Prevention of immediate communal collapse and civil war in Palestine after the dissolution of Ottoman authority, by inserting an imperial administrative buffer between two incompatible national projects claiming the same territory under the same legal instruments.
% FOUNDING_PROBLEM_CORROBORATION: British official commissions (Shaw, Peel) attest to the reality of communal conflict from an imperial analytical seat. However, corroboration that unreviewable interpretive discretion was the necessary solution is absent outside the colonial administration; Zionist and Arab memoranda to the PMC argued that fixed textual obligations already existed and were being ignored by discretionary policy.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60 at interval end) is moderate because the British did provide real administrative order; however, the systematic denial of fixed meaning and external review extracts self-determination and strategic planning capacity from both communities. Suppression (0.70) is high because alternatives (binding international arbitration, fixed textual enforcement) are structurally barred. Theater_ratio rises to 0.48 by 1948 as the interpretive system becomes increasingly performative against the backdrop of collapsing control. Accessibility_collapse (0.75) reflects that once the mandatory system is accepted, there is no textual appeal; resistance (0.60) is substantial from both communities, though asymmetrical in form. The measurement series tracks policy oscillation and enforcement hardening across the mandate lifecycle on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the British imperial seat, the constraint appears as necessary administrative flexibility required to manage an intractable communal problem under international supervision. From both community seats, the same structure appears as arbitrary imperial extraction: policy shifts (1922 Churchill White Paper, 1930 Passfield, 1939 MacDonald) alter rights baselines without consent, and the absence of appellate review means every interpretation is provisional and reversible at imperial convenience. The engine computes this divergence from the structural data â same constraint, opposed directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   British mandatory authority is declared in beneficiaries and as agenda_setter, yielding a beneficiary directionality (d near 0.0). Both Arab and Zionist communities are declared in victims with constrained and trapped exit options respectively, yielding target directionality (d near 1.0). The League PMC is excluded and derives no directionality. Spatial scope is national for the communities and global for the imperial authority, amplifying the authority's subsidy and the communities' extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded as a temporary scaffold (mandatory tutelage under Article 22 of the Covenant) with an implicit sunset clause. However, the interpretive discretion system removed the accountability mechanism that would have triggered sunset: by treating the mandatory's own interpretation as the sole measure of progress, the administration could indefinitely extend the preparatory phase. This is a classic mandatrophy: the founding coordination purpose (tutelage toward self-government) is dead by the 1930s, but the constraint persists as a snare because the interpretive monopoly continues to benefit the imperial administrator. The R5 mismatch (founding_problem_status: contested / disappearance_verdict: world_rearranges) flags this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_discretion_naturality,
    'Is unreviewable interpretive discretion an inherent feature of League of Nations mandatory administration, or a colonial construction that exceeds the mandate''s legal architecture?',
    'Comparative legal analysis of other League mandates to see if equivalent discretion was exercised elsewhere; archival review of Covenant drafting intent.',
    'If inherent, the constraint is a structural feature of international law (mountain or tangled rope); if constructed, it is a colonial snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_discretion_naturality, conceptual, 'Whether interpretive discretion is legally inherent or colonially constructed').

omega_variable(
    textual_determinacy_of_mandate,
    'Do the Mandate instruments possess a fixed determinable meaning that British discretion systematically overrode, or is the text itself inherently contradictory?',
    'Forensic textual analysis and historical jurisprudence on the Balfour Declaration and Mandate text; comparison with sibling reading classifications.',
    'If determinate, the British discretion reading is a snare extracting from a fixed kernel; if indeterminate, the kernel itself is distributed and the constraint is the interpretation layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_determinacy_of_mandate, conceptual, 'Whether the mandate text has fixed meaning or inherent contradiction').

omega_variable(
    communal_oscillation_as_extraction,
    'Does the cyclical policy oscillation between pro-Zionist and pro-Arab positions constitute an intentional divide-and-rule mechanism, or an unplanned byproduct of imperial overreach and domestic lobbying?',
    'Archival evidence of British Cabinet deliberations; pattern analysis of land and immigration policy timing relative to communal tensions.',
    'If intentional, extraction is higher and the snare classification is reinforced; if unplanned, theater_ratio may overstate intentionality and extraction should be moderated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_oscillation_as_extraction, empirical, 'Whether policy oscillation was intentional extraction or unplanned drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.2).
narrative_ontology:measurement(balf_tr_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 4, 0.22).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 10, 0.28).
narrative_ontology:measurement(balf_tr_t14, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 14, 0.3).
narrative_ontology:measurement(balf_tr_t18, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 18, 0.38).
narrative_ontology:measurement(balf_tr_t21, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 21, 0.4).
narrative_ontology:measurement(balf_tr_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 24, 0.44).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 28, 0.48).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(balf_be_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(balf_be_t14, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 14, 0.5).
narrative_ontology:measurement(balf_be_t18, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(balf_be_t21, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 21, 0.55).
narrative_ontology:measurement(balf_be_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 28, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(balf_su_t4, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(balf_su_t14, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 14, 0.72).
narrative_ontology:measurement(balf_su_t18, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(balf_su_t21, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 21, 0.85).
narrative_ontology:measurement(balf_su_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 28, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the balfour_mandate_instruments kernel. The colloquial label 'Mandate for Palestine' conflates three structurally distinct claims: Jewish national home primacy, dual obligation to indigenous rights, and mandatory interpretive discretion. Each reading carries a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
