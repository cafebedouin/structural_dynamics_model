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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ietf_openness_commitment__commons_stewardship_reading
 *   human_readable: IETF Open Standards Interoperability Commitment (Commons Stewardship Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The Internet Engineering Task Force (IETF) maintains a publicly-stated
 *   commitment to open standards development: rough consensus via
 *   meritocratic working groups, freely implementable specifications, and
 *   mandatory patent licensing on Reasonable and Non-Discriminatory (RAND)
 *   terms for any patented technique that reads on a published RFC. Under the
 *   commons stewardship reading, this commitment is understood as a sustained
 *   public infrastructure constraint that preserves interoperability for all
 *   implementers—large and small, incumbent and startup, wealthy and
 *   resource-constrained. No party collects rents from the openness itself;
 *   the constraint redistributes the benefits of standardization horizontally
 *   rather than funneling gains to a gatekeeping authority. The constraint's
 *   persistence depends not on coercive enforcement but on the deeply
 *   embedded technical fact that interoperable networks are more valuable
 *   than fragmented ones, and the institutional commitment to reproduce that
 *   value through continued openness.
 *
 * KEY AGENTS:
 *   - IETF Standards Body: institutional agenda-setter; maintains the process and publishes RFCs; does not extract from the standard's adoption
 *   - All Internet Implementers: primary beneficiary class; all implementers—small and large—operate on the same technical plane once a standard is published
 *   - Small Startups: secondary beneficiary; depend on the openness constraint to enter infrastructure markets without licensing barriers
 *   - Developing-Country Operators: secondary beneficiary; build sovereign internet infrastructure using open standards without vendor lock-in
 *   - Large Technology Incumbents: constrained payers; must license patented techniques on RAND terms and cannot gatekeep the protocol itself
 *   - Patent Holders: constrained payers; receive broad market access through mass interoperability in exchange for loss of monopoly upside
 *   - Alternative Standards Bodies: structurally excluded but contextually-present alternatives (3GPP, ISO, ITU); their existence demonstrates the IETF's openness is a choice
 *   - Internet Governance Regulators: analytical observers; monitor standards for public-interest alignment in interoperability, access, and security
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__commons_stewardship_reading, 0.18).
domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, 0.12).
domain_priors:theater_ratio(ietf_openness_commitment__commons_stewardship_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__commons_stewardship_reading, rope).
narrative_ontology:human_readable(ietf_openness_commitment__commons_stewardship_reading, "IETF Open Standards Interoperability Commitment (Commons Stewardship Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__commons_stewardship_reading, "technology_governance/internet_standards/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__commons_stewardship_reading, 'c7f3b18f-ca84-49a1-846f-6c0e191c1677').
narrative_ontology:cs_kernel_codification('c7f3b18f-ca84-49a1-846f-6c0e191c1677', formalized).
narrative_ontology:cs_authority_grounding('c7f3b18f-ca84-49a1-846f-6c0e191c1677', distributed).
narrative_ontology:cs_reading_relation('c7f3b18f-ca84-49a1-846f-6c0e191c1677', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7f3b18f-ca84-49a1-846f-6c0e191c1677', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('c7f3b18f-ca84-49a1-846f-6c0e191c1677', foundational, interoperability_as_public_good).
narrative_ontology:cs_axiom_status(interoperability_as_public_good, holdable).
narrative_ontology:cs_axiom_grounding('c7f3b18f-ca84-49a1-846f-6c0e191c1677', interoperability_as_public_good, instrumental).
narrative_ontology:cs_axiom('c7f3b18f-ca84-49a1-846f-6c0e191c1677', foundational, openness_as_structural_interoperability_mechanism).
narrative_ontology:cs_axiom_status(openness_as_structural_interoperability_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c7f3b18f-ca84-49a1-846f-6c0e191c1677', openness_as_structural_interoperability_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('c7f3b18f-ca84-49a1-846f-6c0e191c1677', rough_consensus_open_standards_framework).
narrative_ontology:cs_drift_state('c7f3b18f-ca84-49a1-846f-6c0e191c1677', contemporary_platform_dominance_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('c7f3b18f-ca84-49a1-846f-6c0e191c1677', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, all_internet_implementers).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, small_startups).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, developing_country_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, standards_body_volunteers).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, large_technology_incumbents).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, standards_body_volunteers).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, patent_holders).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, rough_consensus_principle).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, interoperability_as_public_good).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, patent_pooling_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the standards development process, enforces IPR rules (Reasonable and Non-Discriminatory licensing), publishes RFCs, and adjudicates technical merit through working groups. Operates as a volunteer-driven meritocracy without formal hierarchy. Does not implement standards itself or collect fees for their use.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, ietf_standards_body, agenda_setter,
    institutional, generational, analytical, global).

% Access published, freely implementable standards that define how the internet routes, encrypts, compresses, and names resources. Small and large implementers operate on the same technical plane once a standard is published. The constraint forces interoperability: implement the published standard, and your implementation talks to all others.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, all_internet_implementers, beneficiary,
    organized, generational, mobile, global).

% Can enter infrastructure markets (DNS hosting, VPN, CDN, security appliances) without reverse-engineering proprietary protocols or licensing from large incumbents. The open standard is their entry key: implement RFC-compliant software, interoperate with the internet, compete on performance and price.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, small_startups, beneficiary,
    moderate, biographical, mobile, global).

% Build national internet infrastructure using open standards without vendor lock-in or prohibitive licensing. They depend on the openness constraint to avoid sovereign connectivity risk: if standards were proprietary and gatekept, building independent networks would require renegotiating with incumbent corporations for every layer.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, developing_country_operators, beneficiary,
    moderate, generational, constrained, global).

% Must publish implementations under IETF standards and permit others to implement their patented techniques under RAND terms when those patents read on an RFC. They compete on implementation quality, scale, and integration, not on owning the protocol itself. Their advantage is engineering excellence and distribution, not protocol gatekeeping.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, large_technology_incumbents, payer,
    powerful, generational, constrained, global).

% Contribute technical work without compensation, driven by professional reputation, employer support, or intrinsic mission commitment. They receive the benefit of participating in the process that shapes internet architecture; they bear the cost of time and opportunity. Employer support is substantial, so the personal burden is often spread.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, standards_body_volunteers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, standards_body_volunteers, payer).

% Must license their inventions on RAND terms if the patent reads on a published RFC and was disclosed during the standards process. They cannot practice outright exclusion or discriminatory licensing. The constraint trades their patent monopoly upside for broad market access to their technology through mass interoperability.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, patent_holders, payer,
    powerful, generational, constrained, global).

% Proprietary or closed consortia (3GPP, ISO/IEC, ITU) that develop competing technical standards outside the IETF process. They are structurally excluded from the IETF's rough-consensus decision system but exist as alternatives. Their existence means the IETF's open commitment is a choice, not a monopoly.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, alternative_standards_bodies, excluded,
    organized, generational, constrained, global).

% Monitor IETF standards to ensure they serve public interest in interoperability, access, and security. They do not set technical standards but enforce antitrust law, cybersecurity mandates, and digital sovereignty principles that intersect with how standards are developed.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, internet_governance_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__commons_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__commons_stewardship_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solve the coordination problem of internet protocol development: without a shared open standard for TCP/IP, DNS, TLS, and HTTP, implementers would fork into incompatible networks. The IETF process produces a single converged technical design through rough consensus, accessible to all implementers at zero royalty cost.
% TRANSFER_FUNCTION: Moves the cost of protocol innovation and standardization from individual implementers (who would each reverse-engineer, fork, and maintain incompatible versions) into a shared volunteer process. Large technology companies subsidize IETF participation (engineer time); all implementers (including small startups and developing-country operators) receive the output as public infrastructure.
% ABSENT_VOICES: Proprietary standards consortia and closed-door implementer alliances are structurally excluded. They would argue for confidential, IP-protected standards development and discriminatory licensing. Developing-country regulators and small implementers are present but often under-resourced for continuous participation.
% DISAPPEARANCE_RATIONALE: If the IETF's openness commitment evaporated overnight—if standards became proprietary, locked behind licensing, or gatekept by resource-advantaged implementers—the internet would fragment into incompatible zones. Startups could not enter infrastructure markets. Developing countries would lose connectivity sovereignty. Large technology companies would compete through protocol control rather than implementation excellence. The unified global internet would become a negotiated archipelago of proprietary networks.
% FOUNDING_PROBLEM: The early internet (1970s–1990s) faced the risk of protocol fragmentation: multiple vendors each developing incompatible TCP/IP stacks, DNS implementations, and routing protocols would have prevented the network from scaling beyond proprietary islands. The IETF was founded to establish a common, open, meritocratic process where any implementer could participate and the resulting standards would be freely implementable.
% FOUNDING_PROBLEM_CORROBORATION: Independent internet historians and the Internet Society (founding organization of the IETF) attest that fragmentation risk persists: every proprietary protocol fork (e.g., proprietary DNS systems, incompatible VPN protocols) that emerges outside the IETF proves that without the commitment to openness, network effects favor closed systems. Large technology companies' own patent disclosures to the IETF and their participation in the standards process corroborate that they regard openness as a constraint they accept to maintain interoperability, not a natural law.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__commons_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__commons_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__commons_stewardship_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__commons_stewardship_reading, 'none', 1).

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
 *   Extractiveness is very low (0.18 at interval end) because no identified beneficiary class accrues concentrated gain from the constraint's operation. The cost of standardization work is distributed across many company participants and volunteers; the benefit of interoperability accrues diffusely to all implementers. Unlike the platform commission constraint (example), there is no single 'taker' concentrating the extraction. Suppression is minimal (0.12) because the constraint's persistence does not depend on coercively preventing exit or alternatives—it depends on the technical fact that interoperable networks are more valuable than fragmented ones. Theater ratio is low (0.22) because the functional work (drafting, testing, publishing technical specifications) is the core activity; while ceremonial elements exist (working group rituals, consensus calls), they are a modest proportion of the actual time spent. The measurements run flat after year 15 because the constraint's operation stabilized: extractiveness asymptoted as the volume of patent licensing under RAND increased (spreading the cost more widely), theater ratio reached equilibrium as the number of review cycles standardized, and suppression requirement plateaued because the structural gatekeeping pressure remained constant. Accessibility collapse is high (0.88) because once the internet exists, the alternative to using open standards is to either accept fragmentation or build one's own proprietary stack—both vastly inferior to interoperability. The constraint is nearly inevitable given the network-effect logic; alternatives have collapsed not through suppression but through technical necessity. Resistance is very low (0.15) because even powerful implementers do not resist the openness commitment—they participate in it, pay the cost of RAND licensing, and benefit from the resulting interoperability.
 *
 * DIRECTIONALITY LOGIC:
 *   All primary and secondary beneficiaries (all_internet_implementers, small_startups, developing_country_operators) have their directionality derived from beneficiary status + mobile or constrained (but non-trapped) exit: they gain diffuse access to a level technical playing field without being forced to participate, so d approaches the beneficiary end (0.0–0.2 range). Large technology incumbents and patent holders have higher d (0.5–0.7 range) because while they benefit from interoperability, they are constrained by RAND licensing requirements and cannot gatekeep the protocol—they bear a cost (inability to monopolize) alongside the benefit (market access). The IETF standards body itself sits near analytical d (0.5) because it administers the process without extracting from it; volunteers sit near symmetric or slightly toward beneficiary because they contribute time but receive professional reputation and participation benefit. Alternative standards bodies are excluded rather than coordinated, so their d is not computed (they are not stakeholders in this reading).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy: the founding problem (protocol fragmentation risk) remains live, the mechanism (rough-consensus open standards development) is still solving it, and the coordination function is not atrophied. There is no evidence that the IETF has become primarily theatrical or that standards are published while the real gatekeeping happens elsewhere. However, the capture_substrate_reading (sibling) argues that gatekeeping HAS moved: from the standard itself to the working-group process through which the standard is defined. Under that reading, the founding problem is formally addressed (the standard is published and implementable) but the real constraint (asymmetric access to standards development) persists in a latent form. This story does not adopt that reading, but it is the reason an omega variable is necessary: the boundary between 'true interoperability' (commons stewardship reading) and 'staged interoperability with process gatekeeping' (capture substrate reading) is empirically contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    working_group_accessibility_vs_formal_openness,
    'Is the IETF standards development process genuinely accessible to all potential contributors (commons stewardship claim), or does asymmetric participation (resource requirements, cultural barriers, working-group politics) constitute a form of latent gatekeeping that encodes resource advantage into the standard itself (capture substrate claim)?',
    'Empirical audit of working-group participation by company size, country, and resource level over a decade; measurement of technical complexity thresholds in RFC requirements; analysis of feature inclusion/exclusion decisions correlated with participant power distribution; post-RFC implementation barriers correlated with company size.',
    'If participation is symmetric and technical requirements track genuine interoperability needs rather than incumbent advantage, the commons_stewardship reading is vindicated. If participation is heavily skewed toward incumbents and technical complexity tracks resource advantage rather than functional necessity, the capture_substrate reading gains empirical support and the constraint''s extractiveness would reclassify upward (0.18 → 0.45+) along with suppression (0.12 → 0.35+).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(working_group_accessibility_vs_formal_openness, empirical, 'Whether the formal openness of the IETF process translates to genuine accessibility for all potential contributors or whether asymmetric participation encodes resource advantage.').

omega_variable(
    rough_consensus_vulnerability_to_capture,
    'Does the ''rough consensus and running code'' decision mechanism effectively prevent organized capture by resource-advantaged actors, or is the consensus model itself a vulnerability—a procedural screen that looks like collective agreement while reflecting the preferences of whoever is most organized and persistent?',
    'Historical case analysis of RFC decisions where the final outcome clearly favored one participant class over another despite stated disagreement; measurement of who ''ran code'' and controlled reference implementations; game-theoretic analysis of incentives in the rough-consensus process; interviews with long-term IETF participants about decision mechanics.',
    'If rough consensus is robust against capture, the legitimacy_erosion reading is overstated. If rough consensus is vulnerable and has been systematically bent toward incumbent interests, the constraint''s claimed type would shift from rope toward tangled_rope (genuine coordination function compromised by asymmetric power) and extractiveness might increase. This is the core of the legitimacy_erosion reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rough_consensus_vulnerability_to_capture, empirical, 'Whether rough consensus is a genuine democratic mechanism or a procedural vulnerability to organized capture.').

omega_variable(
    patent_rand_licensing_effectiveness,
    'Does mandatory RAND patent licensing actually prevent gatekeeping, or do ambiguities in ''reasonable rates'' and ''non-discriminatory terms'' allow patent holders to price-discriminate, selectively withhold licenses for ''strategic'' competitors, or create de facto barriers through litigation threat?',
    'Audit of patent licensing disputes under IETF standards; measurement of licensing rates across company types; analysis of litigation patterns; interviews with small implementers about licensing experience; comparison to explicit open-source licensing (Apache 2.0 w/ patent clause) where terms are defined ex ante.',
    'If RAND licensing works as intended (rates are genuinely non-discriminatory, terms are predictable), the commons_stewardship reading is supported. If RAND licensing is systematically exploited to create soft gatekeeping, extractiveness increases and the constraint approaches tangled_rope or snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_rand_licensing_effectiveness, empirical, 'Whether RAND patent licensing prevents or merely obscures gatekeeping.').

omega_variable(
    kernel_contest_coexistence,
    'Can the commons_stewardship reading (interoperability as public infrastructure) and the capture_substrate reading (standards process as gatekeeping substrate) both be true of the same institutional arrangement, or must one reading foreclose the other?',
    'Structural analysis: if working groups are genuinely open AND participation is asymmetric, both readings describe the same system from different vantage points (commons stewardship describes the formal commitment; capture substrate describes the operation). If one reading''s empirical claims directly contradict the other''s, they do not coexist. Document which structural elements each reading explains and which each leaves opaque.',
    'If the readings coexist (different parties experience the same constraint differently), the engine should compute per-seat classifications that diverge: small implementers see rope; incumbents see snare or tangled_rope. If the readings foreclose, the corpus must choose which reading to instantiate. The current story adopts commons_stewardship; the capture_substrate sibling story instantiates the alternative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_coexistence, conceptual, 'Whether the two readings of IETF openness are complementary descriptions of the same system or mutually exclusive claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__commons_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(ietf_tr_t0, observed).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(ietf_tr_t5, observed).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(ietf_tr_t10, observed).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(ietf_tr_t15, observed).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(ietf_tr_t20, observed).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(ietf_tr_t25, observed).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(ietf_tr_t30, observed).
narrative_ontology:measurement(ietf_tr_t40, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(ietf_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(ietf_be_t0, observed).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement_basis(ietf_be_t5, observed).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(ietf_be_t10, observed).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement_basis(ietf_be_t15, observed).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(ietf_be_t20, observed).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement_basis(ietf_be_t25, observed).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(ietf_be_t30, observed).
narrative_ontology:measurement(ietf_be_t40, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(ietf_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(ietf_su_t0, observed).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement_basis(ietf_su_t5, observed).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(ietf_su_t10, observed).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(ietf_su_t15, observed).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(ietf_su_t20, observed).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement_basis(ietf_su_t25, observed).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(ietf_su_t30, observed).
narrative_ontology:measurement(ietf_su_t40, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(ietf_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__commons_stewardship_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__commons_stewardship_reading, 0.08).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% The IETF openness commitment kernel has three structurally distinct readings: commons_stewardship (this story) understands the constraint as a public infrastructure mechanism preserving interoperability for all implementers with negligible extractiveness and no structural beneficiary class. capture_substrate (sibling) argues that asymmetric participation in the standards process encodes resource advantage into the standard itself, making the constraint a substrate for latent gatekeeping with substantially higher extractiveness. legitimacy_erosion (sibling) argues that the rough-consensus mechanism is vulnerable to organized capture despite procedural safeguards, undermining the constraint's legitimacy foundation. These three readings have different ε values, different beneficiary/victim structures, and different persistence mechanisms; they are NOT observations of the same constraint from different angles. Each instantiates a different constraint with different classification properties. The three stories form a constraint family linked by network.affects_constraints; the commons_stewardship reading assumes the founding problem (protocol fragmentation risk) is still live and the mechanism (openness) is solving it. The capture_substrate reading assumes the formal problem is solved but a latent gatekeeping problem persists. The legitimacy_erosion reading disputes whether the procedural mechanism (rough consensus) is credible at all. All three readings reference the same kernel (IETF's stated commitment to open standards), but they instantiate it differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
