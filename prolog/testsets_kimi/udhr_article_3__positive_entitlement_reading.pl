% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 Positive Entitlement Reading
 *   domain: constitutional/law/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates the positive entitlement reading of UDHR
 *   Article 3, interpreting 'everyone has the right to life, liberty and
 *   security of person' as obligating states to actively provide material
 *   conditions â welfare, healthcare, housing â necessary for dignified
 *   survival. It is distinguished from the negative liberty reading
 *   (prohibition on state deprivation) and the procedural hybrid reading (due
 *   process guarantees without substantive resolution). The constraint
 *   operates through tax extraction from propertied classes and expressive
 *   restriction on speech rights holders, transferring resources to
 *   vulnerable populations via state administration. It is claimed by human
 *   rights institutions as coordination (material security for all) while
 *   functioning as asymmetric extraction on property and expression.
 *
 * KEY AGENTS:
 *   - State administrators: Agenda-setter (institutional/constrained) â administer extraction and provision
 *   - Vulnerable populations: Primary beneficiary (powerless/trapped) â receive material guarantees
 *   - Property rights holders: Primary target (powerful/mobile) â bear fiscal extraction
 *   - Expression rights holders: Secondary target (organized/constrained) â bear speech restrictions
 *   - Constitutional courts: Analytical observer (institutional/analytical) â interpret positive obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.78).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.74).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 Positive Entitlement Reading").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional/law/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '10e68c3f-35b9-4db2-8509-6d2ab3eea33d').
narrative_ontology:cs_kernel_codification('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', formalized).
narrative_ontology:cs_authority_grounding('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', lineage).
narrative_ontology:cs_interpretation_layer_present('10e68c3f-35b9-4db2-8509-6d2ab3eea33d').
narrative_ontology:cs_reading_relation('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', foundational, material_provision_constitutionally_mandated).
narrative_ontology:cs_axiom_status(material_provision_constitutionally_mandated, holdable).
narrative_ontology:cs_axiom_grounding('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', material_provision_constitutionally_mandated, deontological).
narrative_ontology:cs_reference_frame('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', social_rights_fulfillment_framework).
narrative_ontology:cs_drift_state('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', neoliberal_austerity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10e68c3f-35b9-4db2-8509-6d2ab3eea33d', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_rights_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer tax collection, welfare distribution, and housing programs under constitutional and international human rights obligations. They set the policy parameters for what counts as adequate material provision. They are politically and legally constrained by the human rights framework but also empowered by its expansion.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Depend on state provision of healthcare, housing, and welfare transfers for survival and security. They experience the constraint as a guarantee of material support, though access is often mediated by bureaucratic gatekeeping. Exit to private markets or informal support is limited by poverty and legal exclusion.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, national).

% Bear the fiscal extraction required to fund material provision through progressive taxation, eminent domain, and inflationary policy. They argue the constraint appropriates legitimately acquired holdings without proportional individual consent. Capital flight and tax arbitrage are partial but costly exits.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_rights_holders, payer,
    powerful, biographical, mobile, national).

% Bear restrictions on speech justified as necessary for the psychological security and dignity of vulnerable groups. They experience the constraint as a transfer of expressive liberty in exchange for state-guaranteed material conditions. Legal challenge is the primary exit path.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_rights_holders, payer,
    organized, biographical, constrained, national).

% Interpret the scope of Article 3, increasingly reading positive obligations into the text in jurisdictions adopting this reading. They do not collect or pay, but determine the constraint's boundaries and enforcement modalities.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of ensuring minimum material conditions for life and security across populations who cannot secure them through market mechanisms or private charity alone, by centralizing redistribution and provision through state apparatus.
% TRANSFER_FUNCTION: Moves material resources and restricted liberties from propertied and expressive classes to vulnerable populations via taxation, eminent domain, and speech regulation, administered by state bureaucracies and validated by constitutional courts.
% ABSENT_VOICES: Classical liberal and libertarian theorists who reject positive obligations as rights violations; also taxpayers in non-democratic jurisdictions who are subject to extraction without representative consent. They are present in academic and political discourse but structurally excluded from the human rights institutional framework that validates this reading.
% DISAPPEARANCE_RATIONALE: If the positive entitlement obligation vanished overnight, welfare guarantees would collapse into charity or market provision, vulnerable populations would lose legally enforceable claims to material security, tax structures would flatten, and hate speech restrictions justified by dignity would narrow â the political economy would reorganize around purely negative liberty and procedural protections.
% FOUNDING_PROBLEM: Post-war recognition that formal liberty is meaningless without material conditions; mass deprivation, displacement, and statelessness revealed that non-interference alone does not secure life or personhood.
% FOUNDING_PROBLEM_CORROBORATION: Post-WWII framers such as Cassin and Malik attested the material security problem from within the drafting process. Contemporary libertarian economists and legal scholars attest from outside the beneficiary set that the founding problem is either solved by market growth or was misdiagnosed as requiring state provision; the World Bank and Amnesty International offer partially corroborating but ideologically divergent empirical assessments.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint authorizes substantial redistribution and expressive restriction that is decoupled from individualized consent. Suppression is high (0.74) because the arrangement requires active enforcement â tax collection, penalties for non-compliance, and speech regulation â to persist. Theater is elevated (0.55) because rights inflation and expansive judicial interpretation have outpaced actual state delivery capacity, producing performative declarations of entitlement without proportional material realization. Accessibility collapse is moderate (0.62): private and charitable alternatives are not eliminated but are crowded out and delegitimized once the state assumes responsibility. Resistance is substantial (0.68) from libertarian, classical liberal, and propertied constituencies. The temporal series show extraction and theater ratcheting upward over the post-war interval as the welfare state expanded and then shifted toward rights-declaration without delivery.
 *
 * PERSPECTIVAL GAP:
 *   The vulnerable populations seat should compute toward rope or scaffold â the constraint delivers genuine material coordination from their perspective. The property and expression rights holder seats should compute toward snare or tangled rope â they experience raw extraction and restriction with no individualized exit. The state administrator seat sits between: it expands power through the constraint but is also bound by it. The engine derives this divergence from the structural data (beneficiary vs victim roles, exit options, and scope) rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are declared beneficiaries with trapped exit and local/national scope, placing directionality near the full-beneficiary end (low d, low or negative effective extraction). Property and expression rights holders are declared victims with constrained or mobile exit, placing directionality near the full-target end (high d, amplified effective extraction). State administrators are not declared in either beneficiary or victim arrays; structurally they administer rather than collect, so directionality reverts to the institutional power-atom fallback, leaving them nearer symmetric but slightly toward the beneficiary side due to institutional empowerment.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling pure coordination as extraction by preserving the genuine welfare function: post-war material provision solved real deprivation problems. It also prevents mislabeling extraction as coordination by requiring declared victims (property and expression rights holders) and active enforcement. If the coordination function were absent â if the state extracted taxes and restricted speech without delivering material security â the structural data would satisfy snare criteria. If extraction were absent â if material security were provided without coercion â the constraint would read as rope. The tangled rope classification captures that both are present and co-constituted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positive_vs_negative_liberty_framing,
    'Does Article 3''s ''right to life, liberty and security of person'' entail positive state obligations to provide material conditions, or only negative prohibitions against state deprivation?',
    'Comparative constitutional analysis across jurisdictions adopting positive versus negative readings; judicial reasoning tracing the travaux prÃ©paratoires of the UDHR.',
    'If the negative reading is structurally dominant, this constraint''s high extractiveness is an unauthorized overreach; if the positive reading is textually grounded, the extraction is at least partially mandated by the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positive_vs_negative_liberty_framing, conceptual, 'Core textual ambiguity between positive and negative readings of Article 3').

omega_variable(
    coordination_extraction_boundary,
    'What proportion of taxation and expressive restriction under this reading is necessary coordination cost for material provision versus asymmetric extraction?',
    'Marginal analysis of welfare state efficiency; comparison of tax-and-transfer outcomes with market-based social insurance and private provision models.',
    'A high necessary-cost proportion would shift the constraint toward rope classification; a low proportion confirms tangled rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Boundary between genuine coordination cost and extractive overhead').

omega_variable(
    founding_problem_obsolescence,
    'Has the post-WWII founding problem â mass material deprivation rendering formal liberty meaningless â been sufficiently solved that the constraint now extracts without fully coordinating?',
    'Socioeconomic indicators of absolute deprivation in high-entitlement jurisdictions versus low-entitlement jurisdictions; measurement of welfare-state delivery gaps.',
    'If deprivation is largely solved, the constraint may be drifting toward piton or snare (persistence without live founding function); if deprivation persists, the coordination function remains structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem still justifies the constraint''s scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(udhr_tr_t15, udhr_article_3__positive_entitlement_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(udhr_tr_t30, udhr_article_3__positive_entitlement_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(udhr_tr_t45, udhr_article_3__positive_entitlement_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__positive_entitlement_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(udhr_tr_t75, udhr_article_3__positive_entitlement_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(udhr_be_t15, udhr_article_3__positive_entitlement_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(udhr_be_t30, udhr_article_3__positive_entitlement_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(udhr_be_t45, udhr_article_3__positive_entitlement_reading, base_extractiveness, 45, 0.74).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__positive_entitlement_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(udhr_be_t75, udhr_article_3__positive_entitlement_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(udhr_su_t15, udhr_article_3__positive_entitlement_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(udhr_su_t30, udhr_article_3__positive_entitlement_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(udhr_su_t45, udhr_article_3__positive_entitlement_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__positive_entitlement_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(udhr_su_t75, udhr_article_3__positive_entitlement_reading, suppression_requirement, 75, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested UDHR Article 3 kernel. The positive entitlement reading interprets 'life and security' as requiring active state provision of material conditions, structurally distinct from the negative liberty reading (prohibition on state deprivation) and the procedural hybrid reading (due process guarantees without resolving the substantive liberty/welfare contest).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
