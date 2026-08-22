% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__public_safety_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__public_safety_coordination, []).

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
 *   constraint_id: licensing_statute_mandate__public_safety_coordination
 *   human_readable: Statutory Licensing Mandate for Consumer Protection
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   Statutory credential requirements in regulated professions (medicine,
 *   law, construction, plumbing, electrician work, etc.) exist ostensibly to
 *   prevent consumer harm through minimum competence standards. Under this
 *   reading, the constraint solves an information asymmetry problem:
 *   consumers cannot verify practitioner competence and would face high harm
 *   rates in an unregulated market. A government regulatory authority
 *   administers examinations, sets competence thresholds, maintains
 *   registries, and disciplines practitioners who fail to meet standards. The
 *   constraint is claimed as ROPE: consumers benefit from the quality floor;
 *   incompetent practitioners are barred from the market; competent
 *   practitioners benefit from a signal that distinguishes them. This reading
 *   contrasts with two sibling readings: (1) the rent_seeking_suppression
 *   reading, which argues credential requirements function primarily to
 *   restrict labor supply and extract rents for incumbents, and (2) the
 *   graduated_access_filter reading, which argues competence requirements
 *   function as class-sorting mechanisms. This story instantiates the
 *   public_safety_coordination reading only — it models the constraint as
 *   genuine coordination around a shared quality floor, not as cover for rent
 *   extraction or class filtering.
 *
 * KEY AGENTS:
 *   - Consumers of regulated services (powerless, constrained exit, national scope) — benefit from the quality floor
 *   - Licensing regulatory authority (institutional, administrative mandate) — sets and enforces thresholds
 *   - Competent practitioners (powerful, mobile exit, national scope) — benefit from credential as market signal
 *   - Incompetent practitioners / would-be entrants (moderate power, constrained exit, national scope) — excluded from regulated market
 *   - Potential entrants lacking resources (powerless, trapped exit, excluded from conversation) — face barriers that may exceed competence costs
 *   - Consumer harm researchers and regulatory economists (analytical seats) — measure empirical grounding of founding problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__public_safety_coordination, 0.28).
domain_priors:suppression_score(licensing_statute_mandate__public_safety_coordination, 0.22).
domain_priors:theater_ratio(licensing_statute_mandate__public_safety_coordination, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, extractiveness, 0.28).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__public_safety_coordination, rope).
narrative_ontology:human_readable(licensing_statute_mandate__public_safety_coordination, "Statutory Licensing Mandate for Consumer Protection").
narrative_ontology:topic_domain(licensing_statute_mandate__public_safety_coordination, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__public_safety_coordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__public_safety_coordination, '325e8cbe-0c66-4448-b4e4-6bc050dc0a16').
narrative_ontology:cs_kernel_codification('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', formalized).
narrative_ontology:cs_authority_grounding('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', lineage).
narrative_ontology:cs_interpretation_layer_present('325e8cbe-0c66-4448-b4e4-6bc050dc0a16').
narrative_ontology:cs_reading_relation('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_reading_relation('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', foundational, consumer_competence_verification_solves_information_asymmetry).
narrative_ontology:cs_axiom_status(consumer_competence_verification_solves_information_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', consumer_competence_verification_solves_information_asymmetry, empirically_contingent).
narrative_ontology:cs_axiom('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', foundational, credential_threshold_tracks_competence_not_barrier_height).
narrative_ontology:cs_axiom_status(credential_threshold_tracks_competence_not_barrier_height, holdable).
narrative_ontology:cs_axiom_grounding('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', credential_threshold_tracks_competence_not_barrier_height, empirically_contingent).
narrative_ontology:cs_reference_frame('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', consumer_protection_through_verified_competence).
narrative_ontology:cs_drift_state('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', contemporary_occupational_licensing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('325e8cbe-0c66-4448-b4e4-6bc050dc0a16', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, consumers_of_regulated_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, competent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, incompetent_or_unqualified_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals seeking professional services benefit from statutory assurance that practitioners meet minimum competence thresholds. They cannot easily verify competence and rely on the credential signal to avoid harm (botched medical treatment, faulty legal advice, unsafe construction). Their exit options are limited: self-educate to assess competence (prohibitively costly in time and expertise), seek informal referrals (information-poor), or accept unverified practitioners (high harm risk). The credential is their primary quality signal.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumers_of_regulated_services, beneficiary,
    powerless, biographical, constrained, national).

% Administers the credential system by statutory mandate: sets competence thresholds, conducts or oversees examinations, maintains practitioner registries, enforces continuing education requirements, and disciplines or revokes licenses for incompetence or malfeasance. Derives legitimacy from consumer protection mandate, not from revenue (licensing fees typically cover administrative costs only, not operate as a profit center). Faces political pressure from consumers (demand for competence assurance), professional associations (demand for barrier maintenance or lowering), and incumbent practitioners (demand for supply protection).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, licensing_regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Practitioners who meet the competence threshold benefit from the credential as a market signal that distinguishes them from incompetent competitors and from unverified practitioners in adjacent unregulated markets. The licensing requirement protects their reputation and market position; customers will pay a premium for licensed practitioners. They can exit into unregulated adjacent markets (sometimes) or move to different jurisdictions (often), but the credential enhances their value and market power in the regulated sector.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, competent_practitioners, beneficiary,
    powerful, generational, mobile, national).

% Individuals who cannot meet the competence threshold face multiple barriers: cost of education and training, examination fees, apprenticeship time commitments, lost income during training. They are excluded from marketing themselves as licensed practitioners in the regulated profession. Exit options are constrained: pursuing unregulated adjacent work (if available and not also licensed), operating in the profession illegally (with criminal/civil liability risk), retraining in a different field (expensive opportunity cost). The constraint prevents them from credentialing and thus from participating in the regulated market.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, incompetent_or_unqualified_practitioners, payer,
    moderate, biographical, constrained, national).

% Professional associations, licensing boards composed of practitioners, and union representatives monitor credential requirements and maintain formal or informal influence over examination standards, renewal costs, disciplinary procedures, and continuing education mandates. They represent incumbent practitioners' collective interests and can advocate for maintaining high barriers or raising them. They are not the statutory agenda-setter but participate in the rule-making and enforcement process, often through board representation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, professional_associations_and_incumbents, observer,
    organized, generational, mobile, national).

% Individuals from lower-income backgrounds or without access to capital for training and examination fees face structural barriers to credentialing that go beyond competence verification: cost of education, opportunity cost during training, examination fees, apprenticeship wage deprivation, lack of mentorship networks. They would argue that credential requirements, even if competence-based on paper, function as class filters that sort by socioeconomic status rather than competence. They are outside the formal regulatory conversation, not represented on licensing boards, and their objection — that competence requirements are decoupled from access barriers — is not answered by the authority's competence-focused mandate.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, potential_entrants_lacking_resources, excluded,
    powerless, biographical, trapped, national).

% Independent researchers, regulatory economists, audit bodies, and public health analysts measure the rate of consumer harm in licensed vs. unlicensed professions, the correlation between licensing and harm reduction, and whether harm reduction justifies the barrier to entry. They are analytical seats that inform the empirical grounding of the founding problem and test the causality omega.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, consumer_harm_researchers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__public_safety_coordination, diffuse).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__public_safety_coordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared quality floor below which practitioners are excluded from the regulated market for professional services. Solves the information asymmetry problem where consumers cannot directly verify practitioner competence: a statutory credential signals that a government body has assessed the practitioner against a defined competence standard and found them acceptable. This eliminates the need for individual consumers to conduct expensive competence verification themselves or rely solely on reputation networks.
% TRANSFER_FUNCTION: Transfers market access from practitioners who cannot meet the competence threshold to those who do. Practitioners who pass the examination gain the credential and its market premium; those who fail or cannot afford to attempt are excluded from the regulated market. Consumers transfer their trust to the regulatory authority (the endorsement mechanism) rather than relying on their own judgment or informal reputation signals. Competent practitioners capture a reputation premium from the credential; incompetent practitioners lose the opportunity to market themselves in the regulated profession.
% ABSENT_VOICES: Potential entrants lacking resources to meet the threshold barriers are excluded; they would argue that the competence requirements function as class filters. Unregulated practitioners in adjacent markets (who possess competence but no credential) are excluded; they would argue for recognizing de facto competence demonstrated through market survival. Low-income consumers who cannot afford practitioners whose costs reflect the credentialing barrier are excluded; they would dispute the benefit-cost ratio of the quality assurance. Regulatory capture skeptics would argue that incumbent practitioners have influenced the authority to maintain higher barriers than necessary, and that the competence rationale masks labor-supply restriction.
% DISAPPEARANCE_RATIONALE: If the statutory licensing requirement disappeared overnight, the regulatory infrastructure dissolves and practitioners would no longer be barred from the market on competence grounds. Consumer information asymmetry would re-emerge — consumers would need to verify competence themselves through alternative signals (reputation, prior relationships, bonding/insurance, apprenticeship records). Harm rates would likely increase in some professions (those where incompetence directly harms consumers, like medicine and construction) and remain stable or even improve in others (professions with strong reputational mechanisms or repeat-customer dynamics, like plumbing or hairdressing). Practitioner incomes would shift as unqualified competitors entered and competed on price. The market structure would reorganize around alternative quality signals (malpractice insurance, professional liability bonding, guild membership, online reviews, credentialing from professional schools). Consumer harm would spike in information-poor settings and decline in reputation-rich settings.
% FOUNDING_PROBLEM: Early-stage or unregulated professional markets produced consumer harm through practitioner incompetence, fraud, or inadequate training: medical malpractice from untrained surgeons, botched legal representation from unqualified attorneys, unsafe construction from builders without engineering knowledge. Consumers cannot easily verify competence and make decisions under information asymmetry. The founding problem is the market's inability to generate credible quality signals efficiently, leading to high harm rates and market failure.
% FOUNDING_PROBLEM_CORROBORATION: Consumer advocates, medical boards, and public health agencies attest the founding problem remains live and that licensing correlates with harm reduction in some professions (medicine, structural engineering, electrical work — documented reduction in mortality, injury, and property damage). Labor economists and occupational licensing researchers attest that while incompetence harm is real in some professions, the magnitude of harm prevented by licensing is empirically contested and varies substantially by profession — some licensed professions show minimal harm reduction attributable to licensing (cosmetology, interior design), while others show clear correlation (surgery, bridge engineering). Incumbent practitioners and professional associations attest the problem is live; independent researchers attest the problem justifies SOME credentialing but dispute whether current barriers are proportionate to the harm they prevent or whether barriers have drifted beyond competence verification into labor-supply restriction. No independent researcher attests that the founding problem has been fully solved; contestation is over magnitude and remedy proportionality.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__public_safety_coordination, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__public_safety_coordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__public_safety_coordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__public_safety_coordination, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__public_safety_coordination, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__public_safety_coordination_tests).
:- end_tests(licensing_statute_mandate__public_safety_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as 0.28 at interval end, reflecting low extraction under this reading: the constraint's core function is coordination (quality threshold), not wealth transfer. The extraction present is modest: incompetent practitioners lose market access (harm to their income, which is real), but no concentrated beneficiary captures rent — consumers collect the benefit diffusely (lower harm risk, quality assurance), and competent practitioners benefit through reputation enhancement but also bear costs (examination compliance, continuing education). Suppression is low (0.22) because the constraint excludes but does not coerce: incompetent practitioners are barred from the regulated market, but they can exit to unregulated adjacent work, retrain, or operate illegally (with legal risk). Accessibility collapse is high (0.68) because once consumers understand the credential requirement, alternatives (unverified practitioners, self-verification, unregulated markets) genuinely collapse in terms of marketability — the credential becomes the entrance gate. Theater ratio is low (0.12) because the regulatory authority's enforcement activity is mostly substantive (examination administration, record-keeping, disciplinary hearings) rather than performative. The measurement series shows mild extractiveness growth from t=0 to t=20 (as barriers accumulate through regulatory creep, examination costs rise, prerequisite requirements expand), then stabilizes at t=25–30, suggesting the constraint reaches an equilibrium barrier height. This trajectory is consistent with a rope that has drifted slightly toward higher extraction as incumbent practitioners influence barrier maintenance, but remains fundamentally coordinative.
 *
 * PERSPECTIVAL GAP:
 *   From the consumer and regulatory authority seat, the constraint is genuinely coordinative: consumers benefit from the quality floor, the authority administers an impartial standard, and incompetence is a real harm to avoid. From the competent-practitioner seat, the constraint is market-protective and beneficial (distinguishes them from unqualified competitors). From the incompetent-practitioner or would-be-entrant seat, the constraint is exclusionary and potentially unjust, especially when barriers accumulate beyond the minimum needed to verify competence. From the resource-poor-entrant seat (excluded), the constraint is a class filter: it appears competence-based but functions as a barrier that wealthy, connected, or already-educated people can clear more easily. From the rent-seeking-suppression reading (a sibling constraint), the barrier height exceeds competence costs because incumbent practitioners influence the regulatory authority to maintain high barriers that suppress labor supply and sustain high practitioner incomes — competence becomes the stated rationale for barriers that serve rent extraction. The engine computes these per-seat divergences from the structural data; this reading claims only the public_safety_coordination frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers are beneficiaries (d ≈ 0.1–0.2): they gain the quality-floor benefit and bear only diffuse, indirect cost (higher practitioner prices reflecting credentialing barriers). The licensing regulatory authority is symmetric to slightly beneficiary (d ≈ 0.3–0.4): it administers the mandate but derives legitimacy from consumer protection, not from collecting fees, and faces political pressure from multiple sides. Competent practitioners are near-symmetric to slightly beneficiary (d ≈ 0.4–0.5): they benefit from the market-clearing signal but also bear compliance costs. Incompetent practitioners or would-be entrants are the structural targets (d ≈ 0.8–0.9): they are excluded, pay the cost of barriers, and gain nothing from the constraint. Potential entrants lacking resources (excluded from the formal seat set) would, if present, be targets (d ≈ 0.9): they face the barrier cost without the option to comply (resource constraint) and have no exit except into unregulated work or illegal operation. The directionality profile supports a rope classification for seats that collect (consumers, competent practitioners) and a snare-like relationship for those excluded (resource-poor entrants). No override is needed because the structural data derives the correct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer harm from unverified practitioner incompetence) remains live in some professions and contested in others. Medical practice, structural engineering, and electrical work show documented harm reduction correlated with licensing (though causality is disputed). Cosmetology and interior design show minimal evidence of harm reduction attributable to licensing; barriers persist anyway, suggesting mandate drift. The measurement trajectory (extractiveness plateaus at t=20+) is consistent with the constraint maintaining its coordination function while barriers stabilize — not the escalating extraction pattern that would indicate mandatrophy. However, the omegas document that harm-causality and barrier-height are contested; if barriers exceed competence-verification costs, mandatrophy has occurred and the constraint has drifted toward snare or tangled_rope (extraction without coordination justification). This reading does not resolve that; it asserts the public_safety_coordination framing. The divergence between this reading and the rent_seeking_suppression reading is exactly what mandatrophy analysis tests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_threshold_vs_barrier_to_entry,
    'To what extent does the competence threshold coincide with barriers to entry (cost, time, prior resource access) that are unrelated to actual competence?',
    'Empirical comparison: (1) measure harm rates in licensed vs. unlicensed professions and in licensed professions before/after licensing expansion; (2) analyze the barrier-height correlation with harm reduction; (3) conduct quasi-experimental analysis of jurisdiction-specific threshold changes. If harm reduction plateaus while barriers continue rising, the additional barriers are not justified by competence-related harm reduction.',
    'If barriers substantially exceed competence-verification costs, the constraint''s classification shifts toward snare or tangled_rope (barriers create extraction; competence becomes cover story). If barriers track competence costs tightly, the rope classification holds. Under this reading, low extraction (0.28) assumes barriers track competence; if barriers exceed costs, actual extractiveness would be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_threshold_vs_barrier_to_entry, empirical, 'Whether credential requirements track actual competence verification costs or exceed them through class-filtering barriers.').

omega_variable(
    harm_causality_vs_correlation,
    'Does licensing CAUSE harm reduction, or does licensing correlate with harm reduction because high-harm-risk professions are more likely to be licensed?',
    'Quasi-experimental design: analyze jurisdictions that recently adopted or repealed licensing in specific professions; measure harm-rate changes; control for profession-specific risk factors. Randomized natural experiments (inconsistent licensing across state lines) enable causal identification.',
    'If licensing causes harm reduction, the coordination function is real and extraction measured (0.28) is justified as coordination cost. If correlation is spurious or driven by selection, the constraint is more extractive than this reading suggests, and rent_seeking_suppression framing is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_causality_vs_correlation, empirical, 'Whether licensing causes consumer harm reduction or merely correlates with lower-risk professions.').

omega_variable(
    kernel_reading_framing,
    'Is the licensing statute kernel best read as a commitment to competence-based screening (public_safety_coordination, this reading), a commitment to protecting labor market access (rent_seeking_suppression sibling), or a historical institutional layering (piton reading)?',
    'Textual analysis of statutory language and legislative history; audit of how the regulatory authority allocates enforcement effort (competence-testing rigor vs. barrier-maintenance effort); interviews with regulators and professional associations about the actual priority of competence vs. labor-supply protection.',
    'If the commitment is genuinely competence-based, this reading''s rope classification holds. If labor-market-protection, rent_seeking_suppression is more accurate. If institutional drift, the piton reading emerges. This omega documents that the kernel framing is contested and that different readings produce structurally different constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'What the kernel commitment actually is: competence-based screening, labor-market protection, or institutional drift.').

omega_variable(
    resource_access_decoupling_from_competence,
    'How strongly do credential-pathway resource requirements (cost of education, examination fees, opportunity cost during training) correlate with competence, vs. correlate with socioeconomic status?',
    'Analyze examination pass rates, harm rates, and disciplinary rates by socioeconomic background of credentialed practitioners; control for actual practice environment (comparing competence outcomes for practitioners from different backgrounds in the same setting). If pass rates diverge from harm outcomes (high-SES entrants pass easily but show no better harm reduction than lower-SES entrants would), resource barriers decouple from competence.',
    'If resource barriers decouple from competence, the graduated_access_filter sibling reading (class sorting) is more accurate. If they correlate strongly, this reading''s competence framing is supported. Decoupling would suggest the barrier serves class filtering, not competence verification, and the constraint drifts toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_access_decoupling_from_competence, empirical, 'Whether credential-pathway barriers correlate with actual competence or primarily with socioeconomic status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__public_safety_coordination, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__public_safety_coordination, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(lice_tr_t0, observed).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__public_safety_coordination, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(lice_tr_t5, observed).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__public_safety_coordination, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(lice_tr_t10, observed).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__public_safety_coordination, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(lice_tr_t15, observed).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__public_safety_coordination, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(lice_tr_t20, observed).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__public_safety_coordination, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(lice_tr_t25, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__public_safety_coordination, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(lice_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(lice_be_t0, observed).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(lice_be_t5, observed).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(lice_be_t10, observed).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 15, 0.27).
narrative_ontology:measurement_basis(lice_be_t15, observed).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 20, 0.29).
narrative_ontology:measurement_basis(lice_be_t20, observed).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(lice_be_t25, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(lice_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(lice_su_t0, observed).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(lice_su_t5, observed).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(lice_su_t10, observed).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(lice_su_t15, observed).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(lice_su_t20, observed).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(lice_su_t25, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(lice_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__public_safety_coordination, resource_allocation).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__public_safety_coordination, 0.12).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-constraint family decomposing the contested kernel 'licensing_statute_mandate'. The public_safety_coordination reading (this constraint) models the statute as genuine coordination around a shared competence floor (low extractiveness, 0.28). The rent_seeking_suppression sibling reading (linked constraint) models the statute as capture by incumbent practitioners to restrict labor supply and extract rents (higher extractiveness, ~0.60+). The graduated_access_filter sibling reading models the statute as a class-sorting mechanism where credential pathways select for prior resource access rather than competence (extractiveness depends on measurement frame). These readings share a referent (the standing statutory arrangement) but instantiate different ε values and beneficiary/victim structures. The omegas in this file document the empirical and conceptual uncertainties that distinguish the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
