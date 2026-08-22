% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout
 *   domain: intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the HYBRID CARVEOUT READING of the derivative
 *   work statutory boundary kernel. The reading holds that copyright law
 *   properly distinguishes derivative works by commercial exploitation:
 *   non-commercial transformative uses are NOT derivative works and proceed
 *   without authorization; commercial transformative uses ARE derivative
 *   works requiring licensing. This creates a two-tier system where
 *   non-commercial creators (fan artists, academic remixers, small hobbyists)
 *   enjoy exemption while commercial transformative developers (published fan
 *   fiction, monetized mods, commercial remix licensing) face licensing
 *   requirements and costs. The original copyright holders and licensing
 *   intermediaries benefit from this boundary because it exempts unprofitable
 *   uses from policing while capturing licensing revenue from commercial
 *   uses. Commercial transformative developers and boundary-straddlers
 *   (independent publishers, platform creators) bear costs. The constraint is
 *   CLAIMED as tangled_rope (genuine coordination function + asymmetric
 *   extraction) and the authored metrics describe measurably extractive
 *   operation with significant suppression and theater elements—this
 *   divergence is data, not error.
 *
 * KEY AGENTS:
 *   - original_copyright_holders: institutional beneficiary (retain licensing authority, collect selective fees)
 *   - licensing_intermediaries: institutional beneficiary (operate dual market, reduce transaction burden)
 *   - commercial_transformative_developers: payer (face mandatory licensing, licensing delays/refusals)
 *   - non_commercial_transformative_users: exempt beneficiary (low friction, but powerless and vulnerable)
 *   - independent_small_publishers: trapped payer (face retroactive licensing burden as commercial intent emerges)
 *   - non_profit_cultural_institutions: conditional beneficiary (gain exemption but remain boundary-vulnerable)
 *   - rights_advocacy_organizations: analytical observer (dispute the boundary location)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.62).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, 'f3be35de-4165-4077-8605-cd7c11fd9663').
narrative_ontology:cs_kernel_codification('f3be35de-4165-4077-8605-cd7c11fd9663', fixed_text).
narrative_ontology:cs_authority_grounding('f3be35de-4165-4077-8605-cd7c11fd9663', lineage).
narrative_ontology:cs_interpretation_layer_present('f3be35de-4165-4077-8605-cd7c11fd9663').
narrative_ontology:cs_reading_relation('f3be35de-4165-4077-8605-cd7c11fd9663', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3be35de-4165-4077-8605-cd7c11fd9663', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('f3be35de-4165-4077-8605-cd7c11fd9663', foundational, commercial_exploitation_licensing_criterion).
narrative_ontology:cs_axiom_status(commercial_exploitation_licensing_criterion, holdable).
narrative_ontology:cs_axiom_grounding('f3be35de-4165-4077-8605-cd7c11fd9663', commercial_exploitation_licensing_criterion, conventional).
narrative_ontology:cs_axiom('f3be35de-4165-4077-8605-cd7c11fd9663', foundational, non_commercial_transformative_exemption).
narrative_ontology:cs_axiom_status(non_commercial_transformative_exemption, holdable).
narrative_ontology:cs_axiom_grounding('f3be35de-4165-4077-8605-cd7c11fd9663', non_commercial_transformative_exemption, instrumental).
narrative_ontology:cs_reference_frame('f3be35de-4165-4077-8605-cd7c11fd9663', commercial_derivative_licensing_regime).
narrative_ontology:cs_drift_state('f3be35de-4165-4077-8605-cd7c11fd9663', digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3be35de-4165-4077-8605-cd7c11fd9663', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, original_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, independent_small_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_profit_cultural_institutions).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, non_profit_cultural_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive right to authorize derivative works. Under the hybrid carveout, they retain licensing authority over commercial transformations while non-commercial uses proceed without authorization or royalty. They benefit from selective enforcement: monetizing high-value commercial adaptations while avoiding the cost of policing amateur remixes and scholarly transformations.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, original_copyright_holders, beneficiary,
    powerful, generational, arbitrage, global).

% License derivative rights on behalf of rights holders or broker licensing deals. The hybrid boundary creates a dual market: they collect licensing fees from commercial developers (who have no exemption) while avoiding the administrative burden of licensing amateur creators (who don't require it). Transaction costs drop while licensing volume stays high in the commercial segment.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries, beneficiary,
    institutional, generational, mobile, global).

% Create commercially distributed derivative works (fan fiction published for sale, game mods monetized through platforms, music remixes sold). They must secure licensing for any use of copyrighted expression in their derivative, even substantially transformative works. Licensing costs, delays, and refusals create friction; their exit alternatives are limited (cannot simply reuse the original expression, cannot avoid licensing by claiming transformativeness).
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_developers, payer,
    moderate, biographical, constrained, global).

% Create derivative works for non-commercial purposes: academic remixing, fan art without commercial intent, scholarly commentary with embedded clips, amateur video essays. The carveout exempts them from derivative-work liability and licensing requirements. They benefit from low friction, but lack enforcement infrastructure if the boundary between commercial and non-commercial is disputed or if their use begins to generate indirect revenue.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users, beneficiary,
    powerless, biographical, mobile, global).

% Operate at the commercial/non-commercial boundary: online magazines monetized via subscription or ads, small presses experimenting with format innovation, independent game studios creating derivative content. The hybrid carveout forces a stark choice: remain non-commercial and unlicensed, or move toward commercial monetization and face licensing requirements retroactively. Capital constraints make licensing inaccessible; scale constraints make policing uncertain.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, independent_small_publishers, payer,
    powerless, biographical, trapped, national).

% Libraries, museums, archives, and educational institutions engage in derivative-work-like uses (creating accessible formats, remix for classroom teaching, archival adaptation). Many of these activities are nominally non-commercial under the carveout, though institutional purposes may blur the boundary. They benefit from the exemption's existence but remain vulnerable to enforcement disputes when their activities generate institutional revenue or serve educational missions that monetize indirectly.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_profit_cultural_institutions, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, non_profit_cultural_institutions, payer).

% Digital rights, free culture, and open licensing advocates; they argue the boundary is drawn at the wrong place or applied inconsistently. They produce analysis, litigation support, and legislative testimony challenging the hybrid frame. They do not directly benefit or pay but shape the constraint's legitimacy through discourse.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_advocacy_organizations, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, original_copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates allocation of the right to create new works building on existing ones: establishes a clear bright-line rule (non-commercial creators exempt; commercial creators require authorization) reducing friction for low-value uses while maintaining licenseability for high-value ones.
% TRANSFER_FUNCTION: Transfers licensing authority and revenue rights from non-commercial transformative creators (who are exempt) to licensing intermediaries and original copyright holders (who collect fees from commercial transformative developers). The transfer is categorical: no licensing fee for non-commercial use; licensing required and fees extracted from commercial use.
% ABSENT_VOICES: Creators at the commercial/non-commercial boundary (online subscription magazines, hybrid independent publishing, platform-dependent creators) are structurally excluded from the beneficiary set—they bear licensing burdens of commercial developers without the scale to negotiate favorable terms. Rival copyright regimes (open-source projects, Creative Commons, public domain advocates) that would argue for broader exemptions are not in the room.
% DISAPPEARANCE_RATIONALE: If the hybrid carveout vanished and the derivative work right reverted to either full enclosure (all transformations require authorization) or full coordination (only fixed substantial recastings are derivative), the licensing market would collapse or explode: all non-commercial uses would either become infringing (under enclosure) or flood into exemption (under coordination). Digital culture would reorganize around whichever boundary replaced it.
% FOUNDING_PROBLEM: Early copyright law did not distinguish between different types and scales of derivative uses; rights holders faced impossible policing choices (pursue every remix and adaptation or cede control entirely). The hybrid carveout solves this by drawing a bright-line rule: non-commercial uses are not derivative works; commercial uses are.
% FOUNDING_PROBLEM_CORROBORATION: Original copyright holders and licensing organizations attest the founding problem is live—they face ongoing derivative-use disputes. Digital rights advocates and independent developers attest the problem was partially solved (non-commercial exemption works) but the solution creates new extraction at the boundary. Academic IP analysis (Lessig, Vaidhyanathan) corroborates that the boundary reflects not technical necessity but policy choices that benefit concentrated interests.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 at interval end because the arrangement extracts licensing fees from commercial developers while avoiding the cost of policing non-commercial use—the boundary creates a licensing bottleneck. Suppression is high (0.62) because the constraint's persistence depends on enforcement machinery distinguishing commercial from non-commercial intent, which requires active legal surveillance and the threat of infringement liability for miscategorized uses. Theater is moderate (0.41): there is a real coordination function (avoiding blanket policing), but growing enforcement activity defends the commercial/non-commercial boundary itself rather than serving original coordination purpose. The measurement series shows extraction and suppression rising through mid-interval (time 12) as platform economies blur the boundary and enforcement intensifies, then stabilizing as legislative and licensing practices clarify. The series is authored on a single shared time grid (every metric sampled at 0, 3, 7, 12, 18, 25) so temporal analysis can treat all three metrics coherently.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is categorical: from the agenda-setter position (original copyright holders + licensing intermediaries), the arrangement is genuine coordination—it provides certainty and reduces friction while enabling revenue where value is captured. From the payer seats (commercial developers, boundary-straddlers), the same structure operates as forced licensing with suppression of exit alternatives. From the exempt non-commercial seats, the arrangement appears as carve-out beneficence masking the extraction imposed on commercial derivatives. The engine computes this divergence from the structural data: beneficiaries at institutional power with arbitrage/mobile exit see low d (beneficiary end); payers at moderate-to-powerless with constrained/trapped exit see high d (target end). The commercial/non-commercial categorical split is the key: no smooth gradient, but a stark rule that differentiates exit options and enforceability.
 *
 * DIRECTIONALITY LOGIC:
 *   Original copyright holders: beneficiary role, institutional power, arbitrage exit (they set licensing terms and can shift strategies) → d near 0.1 (strong beneficiary end). Licensing intermediaries: beneficiary role, institutional power, mobile exit (they operate in a competitive market but capture licensing rents) → d near 0.15. Commercial transformative developers: payer role, moderate power, constrained exit (must license or infringe, cannot reuse original expression, licensing costs high relative to revenue) → d near 0.75 (strong target end). Non-commercial users: beneficiary role, powerless, mobile exit (exempted but vulnerable, no enforcement infrastructure if boundary is disputed) → d near 0.35 (moderate beneficiary, but powerless). Small independent publishers: payer role, powerless, trapped exit (caught between commercial and non-commercial intent, cannot exit without abandoning business model, licensing inaccessible at scale) → d near 0.85 (strong target end). The asymmetry is structural: categorical exemption + institutional enforcement create a tier system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to allocate derivative-work authority without impossible policing) is contested in status but still live: original copyright holders and licensing organizations say policing remains necessary; digital-rights advocates say the problem was overstated and the solution now serves extraction rather than coordination. The disappearance verdict is world_rearranges: if the hybrid carveout vanished, the boundary would shift, licensing markets would reorganize, and institutional incentives would reorient. The constraint avoids mandatrophy misclassification because it has identifiable beneficiaries collecting real licensing revenue (not theatrical); the non-commercial exemption is genuine (not pure extraction dressed as coordination); and the founding problem, while contested, has not been completely superseded. The theater ratio remains moderate rather than high: the coordination function is partially real (reducing policing friction for low-value uses) but increasingly performs extraction defense (maintaining the commercial/non-commercial boundary against blurring).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_noncommercial_boundary_stability,
    'What counts as ''commercial'' in the hybrid carveout? Is the boundary stable, or does it shift as digital platforms blur direct monetization with indirect revenue capture?',
    'Case-law development and legislative clarification; empirical documentation of how courts and rights holders apply the boundary to platform-mediated revenue (ad-supported, subscription tiers, engagement metrics, data extraction).',
    'If the boundary remains stable (direct payment = commercial, everything else = non-commercial), the categorical split holds and the two-tier system persists. If the boundary erodes (engagement and platform revenue count as commercial), more uses shift into the licensing regime and extractiveness increases. If the boundary collapses entirely, the system reverts to either enclosure or coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_noncommercial_boundary_stability, empirical, 'Stability of the commercial/non-commercial boundary under digital platform economics.').

omega_variable(
    carveout_misclassification_risk,
    'Creators operating at the boundary (hybrid monetization, transitional revenue models, platform-mediated micropayment) face liability risk if their classification is disputed. Is the non-commercial exemption robust, or is it a privilege that can be revoked retroactively?',
    'Examination of enforcement patterns and licensing-demand letters; analysis of creator exit behavior (do creators stay non-commercial to avoid licensing, or do they price in licensing risk?); jurisdiction-specific case law on reclassification.',
    'High retroactive risk reinforces suppression (creators self-censor toward non-commercial or abandon the boundary entirely, reducing derivative-work production). Low risk would stabilize the boundary and reduce extraction. The theater ratio would increase if enforcement focuses on boundary clarification rather than original infringement prevention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carveout_misclassification_risk, empirical, 'Retroactive reclassification risk for creators at the commercial/non-commercial boundary.').

omega_variable(
    carveout_vs_enclosure_foreclosure,
    'Does the hybrid carveout reading''s core premise (commercial exploitation is the legitimate licensing criterion) logically foreclose the enclosure reading''s premise (any derivative use requires authorization)?',
    'Jurisprudential analysis: if both readings can coexist within copyright doctrine (one as legislative policy, one as common-law interpretation), they coexist; if one reading''s adoption requires the other''s abandonment within a single framework, foreclosure holds.',
    'If foreclosure applies, the sibling readings form a zero-sum contest (enclosure and carveout cannot both be copyright law''s governing principle). If coexistence holds, the readings represent stable competing interpretations held by different jurisdictions or institutions. Classification of the relationship shapes litigation strategy and legislative positioning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carveout_vs_enclosure_foreclosure, conceptual, 'Logical foreclosure between hybrid-carveout and enclosure readings.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.62) primarily structural (legal liability risk, licensing gatekeeping) or internalized (creators have accepted the boundary and self-regulate)? If creators were assured no enforcement, would they reclassify their use as commercial?',
    'Qualitative research with boundary-straddle creators; natural experiments from jurisdictions with lighter enforcement or different legal regimes; analysis of stated reasoning for non-commercial positioning versus market realities.',
    'If suppression is structural, the constraint''s effective extraction persists after enforcement is removed—creators remain locked into non-commercial status because the legal framework exists and licensing costs are prohibitive. If internalized, suppression carries forward even outside the enforcement context (creators believe the boundary is natural or legitimate). The distinction informs redesign feasibility: structural suppression can be reformed by law change; internalized suppression requires narrative shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression in the derivative-work carveout is structural gatekeeping or internalized legitimacy acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t3, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(deri_tr_t3, observed).
narrative_ontology:measurement(deri_tr_t7, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 7, 0.37).
narrative_ontology:measurement_basis(deri_tr_t7, observed).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(deri_tr_t12, observed).
narrative_ontology:measurement(deri_tr_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 18, 0.44).
narrative_ontology:measurement_basis(deri_tr_t18, projected).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(deri_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t3, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 3, 0.47).
narrative_ontology:measurement_basis(deri_be_t3, observed).
narrative_ontology:measurement(deri_be_t7, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement_basis(deri_be_t7, observed).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(deri_be_t12, observed).
narrative_ontology:measurement(deri_be_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement_basis(deri_be_t18, projected).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(deri_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t3, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement_basis(deri_su_t3, observed).
narrative_ontology:measurement(deri_su_t7, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement_basis(deri_su_t7, observed).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(deri_su_t12, observed).
narrative_ontology:measurement(deri_su_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 18, 0.67).
narrative_ontology:measurement_basis(deri_su_t18, projected).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(deri_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.18).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, fair_use_transformative_doctrine__scope_and_licensing_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the derivative_work_statutory_boundary kernel. The coordination_reading emphasizes transformative-use jurisprudence and minimizes licensing scope. The enclosure_reading emphasizes copyright protection and maximizes licensing scope. This hybrid_carveout_reading splits the difference through commercial-exploitation categorization. All three stories share the same kernel (statutory definition of derivative work) but instantiate different ε values, beneficiary/victim structures, and type classifications. The readings coexist in actual legal practice across jurisdictions and institutional contexts; they are linked via affects_constraints to enable contamination propagation analysis when one reading's legitimacy or enforceability shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
