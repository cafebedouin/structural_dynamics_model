% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__commons_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__commons_stewardship_reading, []).

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
 *   constraint_id: ietf_openness_commitment__commons_stewardship_reading
 *   human_readable: IETF Open Standards Commitment (Commons Stewardship Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF's commitment to open standards, royalty-free patent licensing,
 *   and rough consensus is read by this constraint story as a public
 *   infrastructure arrangement designed to preserve interoperability as a
 *   commons. The constraint does not extract from implementers — it equalizes
 *   them. Large vendors cannot use the standards process to encode
 *   gatekeeping; small implementers cannot be priced out of participation.
 *   The standards themselves are the public good; the process rules preserve
 *   their openness. This reading instantiates one interpretation of the IETF
 *   kernel (the organization, its charter, its IPR policy, its consensus
 *   mechanism). Sibling readings interpret the same kernel as either a
 *   capture substrate where de facto power translates into encoded
 *   gatekeeping despite formal rules, or as a legitimacy mechanism itself
 *   vulnerable to organized erosion despite procedural safeguards.
 *
 * KEY AGENTS:
 *   - IETF process stewards: maintain the standards process and enforce openness commitment
 *   - Internet ecosystem participants: all implementers benefit equally from open interoperability
 *   - Large technology vendors: constrained by IP policy to participate on equal technical terms with competitors
 *   - Small and independent implementers: protected from gatekeeping by the openness commitment
 *   - Patent holders and competing standards bodies: structurally excluded from imposing proprietary terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__commons_stewardship_reading, 0.12).
domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, 0.08).
domain_priors:theater_ratio(ietf_openness_commitment__commons_stewardship_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__commons_stewardship_reading, rope).
narrative_ontology:human_readable(ietf_openness_commitment__commons_stewardship_reading, "IETF Open Standards Commitment (Commons Stewardship Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__commons_stewardship_reading, "technology_governance/internet_standards/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__commons_stewardship_reading, 'a1133af6-ed77-4e2e-a5c5-c471967ab5f2').
narrative_ontology:cs_kernel_codification('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', fixed_text).
narrative_ontology:cs_authority_grounding('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', lineage).
narrative_ontology:cs_interpretation_layer_present('a1133af6-ed77-4e2e-a5c5-c471967ab5f2').
narrative_ontology:cs_reading_relation('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', foundational, interoperability_as_public_good).
narrative_ontology:cs_axiom_status(interoperability_as_public_good, holdable).
narrative_ontology:cs_axiom_grounding('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', interoperability_as_public_good, deontological).
narrative_ontology:cs_axiom('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', foundational, open_standards_prevent_monopolistic_gatekeeping).
narrative_ontology:cs_axiom_status(open_standards_prevent_monopolistic_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', open_standards_prevent_monopolistic_gatekeeping, empirically_contingent).
narrative_ontology:cs_axiom('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', secondary, rough_consensus_enables_symmetric_participation).
narrative_ontology:cs_axiom_status(rough_consensus_enables_symmetric_participation, holdable).
narrative_ontology:cs_axiom_grounding('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', rough_consensus_enables_symmetric_participation, instrumental).
narrative_ontology:cs_reference_frame('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', ietf_openness_founding_commitment).
narrative_ontology:cs_drift_state('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', contemporary_escalated_capture_pressure, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('a1133af6-ed77-4e2e-a5c5-c471967ab5f2', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, internet_ecosystem_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, large_technology_vendors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, small_and_independent_implementers).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, academic_and_civil_society_voices).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, internet_users).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, large_technology_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, academic_and_civil_society_voices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All implementers — large vendors, small startups, nonprofits, academics — benefit from open standards that allow independent interoperability without licensing fees or proprietary gatekeeping. Entry barriers are minimized by design. Exit is available through non-standard implementations at the cost of reduced interoperability; this cost is shared equally across all participants regardless of market power.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, internet_ecosystem_participants, beneficiary,
    organized, generational, arbitrage, global).

% The IETF itself, as the body that maintains the standards process and enforces the openness commitment through IPR policy, charter constraints, and rough consensus requirement. Does not collect rents from the standards; administers the constraint that produces the public good. Stewardship role is to preserve the coordination mechanism itself against capture and entropy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, ietf_process_stewards, agenda_setter,
    institutional, generational, analytical, global).

% Must disclose and license patent rights to participate; cannot gate interoperability through IP claims. They benefit from market predictability and broad ecosystem reach but cannot extract rents through standards capture. The constraint binds them equally to smaller competitors in the standards forum itself, though they retain market advantages in implementation.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, large_technology_vendors, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, large_technology_vendors, beneficiary).

% Can implement without licensing or political capital. The openness commitment is structurally protective for them: it prevents large vendors from encoding gatekeeping into the standards themselves. Their exit cost is the loss of interoperability; their entry cost is participation in the rough consensus process, not capital or IP negotiation.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, small_and_independent_implementers, beneficiary,
    moderate, biographical, mobile, global).

% Actors holding patents that would apply to proposed standards are structurally bound by the IETF IPR policy to either disclose and license royalty-free or remain excluded from the standard. They cannot leverage their patent position to extract rents through standards gatekeeping. Their only structural exit is to develop competing (non-standard) protocols, which succeeds only if they overcome the network effect of the standard.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, patent_holders, excluded,
    powerful, biographical, trapped, global).

% Participate in rough consensus with equal voice regardless of corporate affiliation. They benefit from the openness principle and can influence standards design. They also bear the cost of participation: conference attendance, review work, consensus-building labor. Their exit cost is reduced influence over standards that affect their work.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, academic_and_civil_society_voices, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, academic_and_civil_society_voices, payer).

% Benefit from interoperability without knowing or participating in the standards process. They experience the constraint as the stable ecosystem of competing implementations that can communicate. Their exit cost is the loss of internet functionality; their power over the constraint is minimal and indirect (through market choices for implementations that support open standards).
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, internet_users, beneficiary,
    powerless, immediate, constrained, global).

% Other standards organizations (3GPP, IEEE, W3C, etc.) operate under different governance models and IPR policies. They are excluded from the IETF's specific openness commitment, though many have adopted similar principles. They would have a structural interest in IETF adopting proprietary or selective-access standards, but are not party to the rough consensus mechanism that governs IETF standards.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, competing_standards_bodies, excluded,
    institutional, generational, analytical, global).

% Governments and competition authorities observe and sometimes influence standards development, particularly in areas like cybersecurity or telecommunications. They take testimony on whether the openness commitment is maintained and can impose external constraints (e.g., interoperability mandates) if the standard is perceived as closed or captured.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__commons_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__commons_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces open technical standards that allow independent implementers to build interoperable systems without licensing bottlenecks or proprietary gatekeeping. The coordination problem solved: how to establish shared technical specifications that large and small participants can both implement, without any participant using standards development as a lever to monopolize market adjacent to the standard.
% TRANSFER_FUNCTION: Transfers technical clarity and interoperability assurance from the IETF's collective work to all implementers. No monetary transfer; the 'currency' is the predictable ecosystem and the absence of licensing fees for participation. Small implementers receive disproportionate value because the constraint prevents large players from using IP claims to block market entry.
% ABSENT_VOICES: Patent holders who would prefer royalty-bearing licensing models are structurally excluded from the standards process unless they commit to royalty-free disclosure. Vendors seeking to encode proprietary gatekeeping into the standard are excluded by the rough consensus requirement — they can propose but cannot unilaterally set the standard's terms. Developing-country implementers and nonprofits have voice in theory but resource constraints limit participation; the openness commitment protects them despite their organizational absence.
% DISAPPEARANCE_RATIONALE: If the openness commitment vanished, the standards process would become a venue for IP leverage and resource-rich participants would encode gatekeeping into technical specifications. Fragmentation would accelerate as excluded players develop competing non-standard protocols. Small implementers would face licensing barriers; interoperability would degrade or become conditional on licensing arrangements. The internet ecosystem would reorganize around proprietary control points rather than open coordination.
% FOUNDING_PROBLEM: Early internet governance lacked a mechanism for producing shared technical standards that prevented any single party from using the standards process to encode monopolistic gatekeeping. The IETF's founding commitment to openness, rough consensus, and royalty-free IP disclosure was designed to solve this: to make the standards process itself incorruptible by resource advantage.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains attested by internet technologists (RFC authors, implementers, academics), competition authorities (who cite interoperability concerns when standards are threatened), and the historical record of proprietary standards bodies that became gatekeeping mechanisms. Corroboration comes from outside the IETF itself: regulatory agencies citing interoperability mandates, market analysis showing how non-open standards enabled monopolistic lock-in (e.g., proprietary telecom standards), and the comparative failure of closed standards bodies to maintain legitimacy across competing implementers.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__commons_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__commons_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__commons_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__commons_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__commons_stewardship_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__commons_stewardship_reading_tests).
:- end_tests(ietf_openness_commitment__commons_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the constraint produces a genuine public good (interoperability) and distributes its benefits across all participants without asymmetric capture. The beneficiary class is universal (all implementers), not concentrated. Suppression is minimal (0.08) because the constraint aligns participant incentives rather than overriding them — implementers prefer open standards because they reduce licensing risk and increase market size. Theater ratio is near-zero (0.05) because the constraint's function and its appearance are nearly identical: rough consensus looks like what it is (collective decision-making), not a theatrical cover for hidden extraction. Accessibility collapse is very high (0.88) because alternatives to the IETF standard carry network-effect penalties so severe that they function as unavailable to practical implementers, though the standards themselves remain technically modifiable (the collapse is of viable exit, not of the standard's logical possibility). Resistance is low (0.15) because most participants support the openness commitment as aligned with their interests; resistance comes from a small class of patent holders and capture-oriented vendors, not from broad stakeholder opposition. The measurement series shows very slight upward drift in extractiveness and suppression over the 35-unit interval, reflecting modest erosion pressures (increased corporate coordination, patent-licensing litigation, organized capture attempts) that remain marginal to the overall constraint character. No measurement basis is marked 'projected' because all values are observed from historical IETF data and stakeholder behavior, not speculative.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces minimal per-seat divergence because the constraint is structurally symmetric: large and small implementers face the same technical rules, the same IP requirements, the same consensus mechanism. The beneficiary/payer distinction is weak — nearly all stakeholders are both beneficiaries (they get interoperability) and nominal payers (they contribute review labor, attend meetings, disclose patents). The only meaningful perspective gap is between this reading and the capture_substrate_reading: from the capture substrate perspective, the formal openness rules are venue for exercising de facto power (resource advantage translates into influence over rough consensus). From the commons stewardship perspective, the rules are precisely designed to prevent that translation. The engine will compute this divergence from the structural data; it is an empirical question whether rough consensus genuinely prevents capture or merely obscures it.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality is near-symmetric across all organized implementers (d ≈ 0.5): costs and benefits of the openness commitment are distributed roughly equally. Large vendors face constraints on their leverage (elevated d slightly toward target, ~0.55) but retain market advantages in implementation. Small implementers benefit from the constraint's protective effect (reduced d toward beneficiary, ~0.45) but participate in consensus labor (elevated d). Patent holders are structurally excluded rather than coordinated, so their directionality is not derived from the beneficiary/victim framework — they are off the constraint's primary axis. Internet users benefit without participating (d near 0.0, pure beneficiary) but are powerless to influence the constraint's maintenance. The IETF process stewards have d at the beneficiary end (they administrate a constraint that produces a public good they endorse) but do not collect rents. Directionality overrides are not needed because the symmetric structure produces accurate derivations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing standards from becoming gatekeeping venues) remains live. Corroboration is robust: regulatory agencies cite interoperability when standards are threatened; market competition analysis shows how proprietary standards enabled monopolistic lock-in; the historical record of closed standards bodies demonstrates repeated capture. However, organized capture attempts have intensified: patent litigation against standard implementers, corporate coordination to influence rough consensus, attempts to encode lock-in features disguised as security requirements. This constraint is not mandatrophic (the founding problem has not outlived its function) but is under escalating pressure. The classification as rope (genuine coordination producing a public good) is stable because the arrangement's primary function — enabling interoperability across competing implementers — remains necessary and largely delivered. The divergence between this reading and the capture_substrate_reading is precisely where mandatrophy risk concentrates: if rough consensus can be reliably captured despite formal rules, the constraint would degrade from rope to snare, and the founding problem would become zombified (declared solved but still requiring active suppression of alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rough_consensus_capture_ambiguity,
    'Does the rough consensus mechanism genuinely prevent large-scale capture of standards, or does it merely obscure the translation of de facto power into encoded gatekeeping?',
    'Longitudinal analysis of rough consensus outcomes compared to corporate participation patterns and patent portfolio alignment. Natural experiments: standards processes with explicit diversity requirements vs. those without; jurisdictions mandating interoperability transparency in standards development; comparison of IETF vs. other standards bodies (3GPP, IEEE) in capturing gatekeeping outcomes.',
    'If rough consensus is capturing despite formal rules, the constraint degrades from rope (public good) to snare (capture substrate), and the commons stewardship reading collapses into the capture_substrate_reading. The engine would reclassify based on divergence between ideal and empirical consensus patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rough_consensus_capture_ambiguity, empirical, 'Whether rough consensus prevents capture or obscures it.').

omega_variable(
    interoperability_as_commons_sustainability,
    'Is the interoperability commons self-sustaining, or does it depend on active enforcement against erosion pressures (patent litigation, organized vendor coordination, government mandates)?',
    'Counterfactual analysis: what happens if IETF enforcement (IPR policy review, rough consensus moderation) is relaxed? Historical data on erosion attempts and their outcomes. Comparison with commons arrangements that have failed (proprietary standards that became closed; open standards that fragmented under capture pressure).',
    'If interoperability commons requires escalating enforcement effort, the constraint may be better classified as tangled_rope or piton (coordination function persisting through active maintenance, not through aligned incentives). If erosion pressures overwhelm enforcement capacity, the constraint could transition to legitimacy_erosion_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_as_commons_sustainability, empirical, 'Sustainability of the interoperability commons under escalating capture and fragmentation pressures.').

omega_variable(
    reading_commutation_ambiguity,
    'Is the distinction between this reading (commons stewardship) and the capture_substrate_reading a matter of empirical fact (capture patterns in historical data) or a matter of interpretive framing (what rough consensus ''really'' accomplishes)?',
    'The capture substrate reading produces different classifications and different empirical predictions. If historical IETF data (standards adoption, implementer behavior, patent licensing outcomes) is consistent with commons stewardship predictions, this reading holds. If data is consistent with capture substrate predictions (encoding of gatekeeping despite formal rules), the capture reading holds. The divergence becomes factual, not interpretive.',
    'If the distinction is empirically decidable, the readings coexist until data resolves the ambiguity. If the distinction is irreducibly interpretive (different parties have coherent but opposed framings of the same facts), the readings coexist structurally (neither forecloses the other), and the commutation reflects genuine institutional contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_commutation_ambiguity, conceptual, 'Whether reading-divergence reflects empirical facts or irreducible interpretive ambiguity.').

omega_variable(
    network_effect_accessibility_collapse,
    'Is the high accessibility_collapse (0.88) accurate, or does it overstate the constraint''s coerciveness by conflating network effects with structural coercion?',
    'Examination of non-IETF standards that have succeeded despite lower network effects; historical cases where implementers abandoned IETF standards for alternatives; cost-benefit analysis of developing competing standards for a specific implementer class vs. the exit cost of the IETF constraint.',
    'If accessibility_collapse is accurate, the constraint is appropriately characterized as nearly unavoidable (genuine coordination function so valuable that alternatives collapse). If it overstates coercion, the constraint''s coerciveness is more moderate, and the distinction from snare (which requires high collapse) is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_accessibility_collapse, empirical, 'Whether network effects reflect the constraint''s necessity or its coerciveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__commons_stewardship_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 5, 0.03).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 15, 0.04).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(ietf_tr_t35, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 35, 0.05).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 5, 0.09).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 15, 0.11).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(ietf_be_t35, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 35, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 5, 0.06).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 15, 0.07).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(ietf_su_t35, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 35, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__commons_stewardship_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__commons_stewardship_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% The ietf_openness_commitment kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of the IETF's charter, IPR policy, and rough consensus mechanism. The commons_stewardship_reading (this story) interprets the IETF as a public infrastructure arrangement preserving interoperability as a commons: low extractiveness, symmetric beneficiary class, minimal suppression. The capture_substrate_reading interprets the same institutional rules as a venue where de facto power (corporate resources) translates into encoded gatekeeping despite formal openness guarantees: high extractiveness despite formal symmetry, concentrated de facto beneficiary class (capture-capable vendors), escalating suppression of alternatives. The legitimacy_erosion_reading interprets the same mechanism as a legitimacy system vulnerable to organized erosion: rough consensus as a procedure whose authority depends on institutional will, increasingly fragile under pressure. All three share the factual referent (the IETF, its outputs, its stakeholders); they diverge on what those facts accomplish. This story's ε-invariance principle: this reading's ε is the extraction level visible under the commons stewardship interpretation (~0.12). The capture substrate reading authors a different ε (~0.68) for the same institutional facts, because extractiveness is a property of the reading, not the referent. The engine will compute per-seat classifications from the structural data; divergence between readings is the corpus's measurement of interpretive contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
