% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Conceptualization Boundary (Theoretical Emergence Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel
 *   'digital_money_emergence_boundary': the CONCEPTUALIZATION READING.
 *   Digital money, under this reading, emerged when it became theoretically
 *   thinkable — specifically, when Chaum's 1985 DigiCash formalization and
 *   the preceding 1960s telecommunications infrastructure advances
 *   established the mathematical and technical foundations for digital
 *   currency as a coherent concept. The reading is not about consumer
 *   adoption, functional monetary use, or regulatory recognition; it is about
 *   the moment the research community achieved conceptual consensus that
 *   'digital money' was a well-defined object of study. This reading
 *   prioritizes theoretical formalization as the constitutive boundary,
 *   making the academic cryptography community the primary authority on
 *   emergence. The constraint operates as a rope because it solves a genuine
 *   coordination problem (establishing shared conceptual reference points)
 *   while asymmetrically benefiting the academic community whose priority
 *   claim is recognized.
 *
 * KEY AGENTS:
 *   - Academic cryptography community: sets and enforces the conceptualization boundary through peer review, publications, and citation authority
 *   - Protocol researchers: benefit from the established boundary as a foundation for research
 *   - Theoretical finance scholars: benefit from a coherent intellectual tradition and historical narrative
 *   - Telecommunications infrastructure operators: enabled the technological possibility but are not recognized as the innovation boundary
 *   - Central banks: excluded from setting the boundary but later constrained by its implications for monetary authority
 *   - Financial regulators: excluded from the boundary-setting and later face regulatory ambiguity
 *   - Competing theoretical frameworks: excluded from legitimate standing in the consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.31).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.18).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Conceptualization Boundary (Theoretical Emergence Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '30b66771-3d8c-4311-9ae0-bf3c589dddc7').
narrative_ontology:cs_kernel_codification('30b66771-3d8c-4311-9ae0-bf3c589dddc7', formalized).
narrative_ontology:cs_authority_grounding('30b66771-3d8c-4311-9ae0-bf3c589dddc7', expertise).
narrative_ontology:cs_interpretation_layer_present('30b66771-3d8c-4311-9ae0-bf3c589dddc7').
narrative_ontology:cs_reading_relation('30b66771-3d8c-4311-9ae0-bf3c589dddc7', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('30b66771-3d8c-4311-9ae0-bf3c589dddc7', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('30b66771-3d8c-4311-9ae0-bf3c589dddc7', foundational, theoretical_formalization_constitutive).
narrative_ontology:cs_axiom_status(theoretical_formalization_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('30b66771-3d8c-4311-9ae0-bf3c589dddc7', theoretical_formalization_constitutive, conventional).
narrative_ontology:cs_axiom('30b66771-3d8c-4311-9ae0-bf3c589dddc7', foundational, academic_consensus_authority_on_emergence).
narrative_ontology:cs_axiom_status(academic_consensus_authority_on_emergence, holdable).
narrative_ontology:cs_axiom_grounding('30b66771-3d8c-4311-9ae0-bf3c589dddc7', academic_consensus_authority_on_emergence, conventional).
narrative_ontology:cs_reference_frame('30b66771-3d8c-4311-9ae0-bf3c589dddc7', pre_formalization_conceptual_incoherence).
narrative_ontology:cs_drift_state('30b66771-3d8c-4311-9ae0-bf3c589dddc7', post_chaum_1985_consensus, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('30b66771-3d8c-4311-9ae0-bf3c589dddc7', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, protocol_researchers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, theoretical_finance_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, telecommunications_infrastructure_operators).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, mathematical_formalization_enables_innovation).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, protocol_priority_grants_intellectual_property_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes the conceptual boundary through peer-reviewed publications, protocol specifications, and academic priority claims. Chaum's 1985 DigiCash formalization is the canonical reference point they defend. They set the definition of what counts as 'digital money' by publishing the mathematical foundations and establishing research priority. Benefit accrues through career advancement, citation priority, and grant funding tied to foundational contributions.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community, agenda_setter,
    organized, generational, mobile, global).

% Conduct research on digital money protocols and publish findings. Benefit from the established conceptual boundary because it legitimates their research domain and their work builds on acknowledged foundations. Their contribution is recognized within a defined intellectual tradition.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, protocol_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Study digital money as an economic phenomenon. Benefit from a clear theoretical boundary because it enables coherent historical narratives and peer review against a shared reference frame. Their scholarship relies on the consensus about when digital money became conceptually distinct.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, theoretical_finance_scholars, beneficiary,
    moderate, biographical, mobile, global).

% 1960s telecom advances enabled the conceptualization (long-distance digital transmission, packet switching) but receive no direct recognition or benefit from the academic claim to digital money's origin. They bear the cost of infrastructure without claiming the innovation boundary. The boundary locates innovation in the theoretical formalization, not the enabling technology.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, telecommunications_infrastructure_operators, payer,
    institutional, generational, constrained, global).

% Have vested interest in defining what counts as money and from when. Are structurally excluded from setting this academic boundary — monetary policy rests on their authority to define money, but the academic conceptualization boundary operates in a separate epistemic domain. They would object to the priority given to cryptographic formalization over monetary function and legal status, but their voice is not seated in the academic consensus-setting process.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_banks, excluded,
    institutional, generational, trapped, national).

% Later tasked with regulating digital money instruments, but excluded from the academic boundary-setting. Would prefer a boundary that aligns with functional monetary capacity or legal currency status, not theoretical formalization. The early conceptualization boundary constrains their regulatory frame retroactively.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, financial_regulators, excluded,
    institutional, generational, trapped, national).

% Alternative theoretical approaches to digital money (Austrian school, commodity-backed frameworks, behavioral finance models) are excluded from setting the boundary. The cryptographic consensus mechanism prioritizes mathematical formalization and makes competing theoretical vocabularies subordinate. Researchers working in excluded frameworks have constrained academic voice.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, competing_theoretical_frameworks, excluded,
    moderate, biographical, constrained, global).

% Observer seat: this boundary is set before consumer digital money use becomes meaningful. Consumer interests in functionality, accessibility, and protections are not parties to the conceptualization phase. They lack standing in the academic consensus-setting but will later be governed by the boundary's downstream implications.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, consumer_advocates, observer,
    powerless, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared conceptual framework and canonical reference points (Chaum 1985 formalization, cryptographic protocols) enabling coherent discourse across research communities. Allows researchers to build on acknowledged foundations rather than re-deriving fundamentals. Solves the problem of what counts as 'digital money' so the research domain has bounded scope.
% TRANSFER_FUNCTION: Transfers intellectual priority and academic authority from telecommunications infrastructure developers (who made it technically possible) to cryptography researchers (who formalized it theoretically). Academic citations, grant funding, and career advancement flow to the academic community whose boundary definition is accepted. Central banks and financial regulators lose the authority to retroactively define when digital money 'really' began.
% ABSENT_VOICES: Central banks, financial regulators, and consumer interests are excluded. They would argue that digital money emerges when it functions as a medium of exchange and store of value accessible to users, not when it becomes theoretically formalized. Infrastructure operators who enabled the 1960s advances are not seated in the academic consensus. Competing theoretical frameworks in finance are subordinated to the cryptographic consensus.
% DISAPPEARANCE_RATIONALE: If this academic boundary disappeared, the historical narrative about digital money's origins would reorganize around alternative framings: infrastructure operators might claim 1960s telecommunications advances as the origin; central banks might assert that digital money emerged only when they recognized it; consumer historians might locate emergence in 1990s e-purses and real-world use. The research domain's coherence depends on accepting this boundary — removing it would fragment the intellectual tradition and require competing framings to negotiate legitimacy independently.
% FOUNDING_PROBLEM: In the 1960s-1980s, the theoretical possibility of digital money existed (telecommunications enabled it, cryptography could formalize it) but no consensus existed about what counted as 'digital money' conceptually. Different disciplinary communities (computer scientists, cryptographers, economists, central bankers) used different definitions. Researchers could not build systematically on acknowledged foundations because the foundations were contested.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians and philosophy of science scholars (outside the cryptography community establishing the boundary) confirm that conceptual incoherence existed until the mid-1980s formalization. Regulatory historians document that central banks and financial regulators independently developed competing definitions through the 1980s-1990s. No single corroborating authority: the problem status is evidenced by the multiplicity of competing definitions across disciplinary domains.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).
:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.31 at interval end) because the constraint is primarily coordination (establishing shared terminology and conceptual foundations) rather than pure rent extraction. However, it is not zero because the boundary confers priority and authority on the academic community, which accrues career and funding benefits denied to competing framings. Suppression is low (0.18) because the boundary holds through intellectual consensus rather than coercive enforcement — researchers can technically contest it, though doing so costs academic standing. Theater ratio is minimal (0.12) because the constraint's function (establishing conceptual reference) is genuine and performative activity is limited; the boundary's legitimacy depends on its correspondence to actual theoretical work, not theatrical maintenance. Accessibility collapse is low (0.22) because alternative theoretical boundaries are technically possible and some researchers do pursue them — the conceptualization reading is dominant but not inevitable. Resistance is moderate (0.41) because central banks, regulators, and infrastructure-focused historians genuinely contest this boundary and advocate for alternative emergence points. The measurement trajectory shows extraction rising from 1960 to 1985 as the Chaum formalization consolidates consensus, then plateauing after 1985 because the boundary stabilizes — later contestation (infrastructure reading, consumer holdings reading) does not retroactively dislodge the already-canonized conceptualization.
 *
 * PERSPECTIVAL GAP:
 *   The academic agenda-setter (cryptography community) experiences this constraint as genuine coordination: establishing shared reference frames that enabled systematic research progress. From this seat, the boundary is a neutral, successful consensus about what counts as 'digital money.' From the excluded seats (central banks, regulators, infrastructure operators), the constraint operates asymmetrically: the academic boundary retroactively defines when 'digital money' began, constraining later regulatory and policy narratives. Central banks cannot later say 'digital money only emerged when it had legal status' if the academic consensus has already canonized the 1985 theoretical formalization as the boundary. This asymmetry is not enforced coercively but through epistemic authority — the excluded seats lack standing in the consensus-setting process.
 *
 * DIRECTIONALITY LOGIC:
 *   The academic cryptography community benefits directly from the boundary (careeradvancement, research legitimacy, grant funding tied to foundational contributions) and sits at high power with mobile exit — they could pursue different theoretical projects if they chose, so their directionality is low (full beneficiary). Protocol researchers and theoretical finance scholars benefit from the boundary without setting it, so their directionality is moderate (beneficiaries with constrained exit — they depend on the established boundary to do coherent work). Telecommunications operators bear a cost (their enabling infrastructure is not credited as the emergence point) but have high power and exit options outside this constraint (they continue operating infrastructure regardless), so their directionality is slightly above symmetric. Central banks and regulators are extracted from (their authority to define emergence is preempted) but have high institutional power and operate in different domains, so their directionality is constrained above the beneficiary end — they are partially targets but not fully trapped. The consumer advocates are analytically outside the constraint; their directionality is neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy. The founding problem (conceptual incoherence across disciplinary approaches to digital money) remains live — competing frameworks continue to debate the emergence boundary. The boundary is maintained not by theatrical performance but by active intellectual work (conferences, publications, peer review). The constraint persists because the coordination function it performs (enabling coherent discourse across the research community) remains valuable. If the founding problem became dead (if, say, all stakeholders converged on a unified definition of digital money emergence), the constraint would transform into a piton sustained by historical inertia — but that has not occurred. The coexistence of three competing readings (conceptualization, infrastructure, consumer holdings) in ongoing scholarly debate indicates the founding problem is genuinely contested, not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptualization_vs_infrastructure_boundary,
    'Is the boundary between theoretical formalization (Chaum 1985) and enabling infrastructure (1960s telecommunications) materially distinct, or is the ''theoretical'' boundary merely a retrospective articulation of what the infrastructure made possible?',
    'Historical analysis of Chaum''s cited references and acknowledgments; investigation of whether the cryptographic formalization could have occurred without the 1960s telecommunications advances. If telecommunications infrastructure was strictly prerequisite, the boundary may be conventional (located post-hoc at formalization) rather than constitutive.',
    'If the infrastructure is strictly prerequisite, the infrastructure reading gains structural validity — digital money emerged when it became technically possible, not when it was formalized. If formalization added genuinely new conceptual content, the conceptualization reading retains distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptualization_vs_infrastructure_boundary, empirical, 'Whether formalization is an independent milestone or a retrospective articulation of infrastructure possibility.').

omega_variable(
    academic_consensus_vs_regulatory_definition,
    'What constitutes a ''real'' boundary: academic consensus about what digital money is conceptually, or regulatory/legal recognition of what counts as money for policy purposes?',
    'Examination of central bank internal documents, regulatory deliberations, and monetary policy documents from the 1980s-1990s. Comparison of academic definitions with monetary authority definitions to determine whether they converged or remained independent.',
    'If regulatory definitions diverged substantially from academic consensus, the constraint''s authority rests on epistemic authority alone (research community consensus), not on functional monetary governance. If they converged, the constraint is embedded in broader institutional legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(academic_consensus_vs_regulatory_definition, empirical, 'Whether academic boundary-setting is independent from regulatory/monetary authority or embedded within it.').

omega_variable(
    priority_claim_extraction_mechanism,
    'Is the measured extraction (0.31) legitimately attributed to coordination benefits, or does it primarily reflect the asymmetric authority granted to the academic community to claim ''first mover'' innovation status?',
    'Analysis of who captures the benefits of the priority claim: do academic researchers receive material benefits (grants, patents, licensing fees) tied to Chaum''s formalization, or is the benefit purely epistemic (citation priority, disciplinary authority)? If material benefits accrue, extraction is higher; if purely epistemic, extraction may be lower.',
    'If extraction is primarily from authority/priority-claiming rather than coordination function, the constraint should be classified as tangled_rope or snare rather than rope — coordination exists but is subordinate to asymmetric benefit distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priority_claim_extraction_mechanism, empirical, 'Whether extraction reflects coordination cost or authority rents.').

omega_variable(
    reading_relation_foreclosure,
    'Does the conceptualization reading logically foreclose the infrastructure reading and consumer holdings reading, or do all three remain coherently holdable within different interpretive traditions?',
    'Examination of whether accepting the conceptualization boundary requires rejecting the infrastructure and consumer readings, or whether an interpreter can coherently hold that digital money had multiple emergence points depending on the analytical frame (theory/infrastructure/consumer access).',
    'If readings coexist coherently (each true within its frame), the relations are ''coexists_with''. If the conceptualization reading''s epistemic claims directly contradict the others'' core premises, the relation is ''forecloses''. This affects the constraint''s stability and the kernel''s resolution trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_foreclosure, conceptual, 'Logical compatibility among the three readings of the emergence boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(digi_tr_t1980, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.11).
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(digi_tr_t1995, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1995, 0.12).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(digi_be_t1980, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(digi_be_t1995, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1995, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(digi_su_t1980, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1990, 0.17).
narrative_ontology:measurement(digi_su_t1995, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1995, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.05).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The kernel 'digital_money_emergence_boundary' has been decomposed into three constraint stories, each instantiating a distinct reading of when digital money emerged. This story (conceptualization_reading) privileges theoretical formalization; the infrastructure_reading privileges technical capability; the consumer_holdings_reading privileges user accessibility. The three readings coexist in scholarly debate with no single winning reading. Each story has its own ε (extractiveness), beneficiary structure, and type classification. They are linked via network.affects_constraints to enable cross-reading analysis and contamination propagation study.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__conceptualization_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
