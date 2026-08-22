% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__commons_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commons_stewardship, []).

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
 *   human_readable: IETF Open Standards Commitment — Commons Stewardship Reading
 *   domain: institutional/technology governance/commons
 *
 * SUMMARY:
 *   The Internet Engineering Task Force operates under an explicit openness
 *   commitment: standards are developed through open, public working groups
 *   using rough consensus decision-making; all implementations must be
 *   royalty-free; standards cannot encode proprietary gatekeeping. This
 *   constraint story instantiates the COMMONS STEWARDSHIP READING of the
 *   contested IETF openness kernel. In this reading, open standards function
 *   as public infrastructure that preserves interoperability for all
 *   implementers — large and small — and prevents any single vendor from
 *   using standards capture to lock in competitive advantage. The constraint
 *   is examined from the perspective that sees open standards as a
 *   coordination mechanism WITH NO STRUCTURAL BENEFICIARY CLASS: the rules
 *   apply equally to all implementers; no party collects rents from the
 *   constraint's operation; the value accrues to interoperability itself.
 *   This contrasts sharply with the CAPTURE SUBSTRATE READING (where
 *   standards are seen as coordination surfaces that large vendors use to
 *   encode their technical advantages) and the LEGITIMACY EROSION READING
 *   (where rough consensus is seen as vulnerable to organized capture despite
 *   procedural safeguards). The claim/metric gap is deliberate:
 *   extractiveness is CLAIMED as rope (genuine coordination) and the authored
 *   metrics confirm low extraction (0.18), low suppression (0.12), low
 *   theater (0.08) — the metrics and claim align because this reading's
 *   structural analysis sees no hidden extraction. That alignment does not
 *   mean the other readings are wrong; it means the engine will show per-seat
 *   classification divergence depending on the actor's power level and
 *   position in the standards process.
 *
 * KEY AGENTS:
 *   - IETF Working Groups: distributed technical authority; no single actor controls outcomes
 *   - Large Technology Firms: cannot use capture to lock in advantages; must compete on implementation quality
 *   - Small and Startup Implementers: gain preservation of market access; benefit from equal procedural standing
 *   - End Users and Network Operators: depend on interoperability; openness secures their choice set
 *   - Implementer Development Community: open standards enable free implementation and modification
 *   - IETF/IESG Governance: stewards the procedural openness that prevents organized capture
 *   - Patent Holders with Essentials: cannot demand exclusive licensing; bound by FRAND commitments
 *   - Proprietary Vendors: excluded from capturing the standard to encode their approaches; may view openness as hostile
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__commons_stewardship_reading, 0.18).
domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, 0.12).
domain_priors:theater_ratio(ietf_openness_commitment__commons_stewardship_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__commons_stewardship_reading, rope).
narrative_ontology:human_readable(ietf_openness_commitment__commons_stewardship_reading, "IETF Open Standards Commitment — Commons Stewardship Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__commons_stewardship_reading, "institutional/technology governance/commons").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__commons_stewardship_reading, '9afb1ac4-54f9-4c9d-92b6-bf775721b61b').
narrative_ontology:cs_kernel_codification('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', formalized).
narrative_ontology:cs_authority_grounding('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', distributed).
narrative_ontology:cs_reading_relation('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_reading_relation('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', foundational, open_procedures_equalize_influence).
narrative_ontology:cs_axiom_status(open_procedures_equalize_influence, holdable).
narrative_ontology:cs_axiom_grounding('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', open_procedures_equalize_influence, conventional).
narrative_ontology:cs_axiom('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', foundational, interoperability_is_nonexcludable_public_good).
narrative_ontology:cs_axiom_status(interoperability_is_nonexcludable_public_good, holdable).
narrative_ontology:cs_axiom_grounding('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', interoperability_is_nonexcludable_public_good, deontological).
narrative_ontology:cs_reference_frame('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', open_participation_procedural_authority).
narrative_ontology:cs_drift_state('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9afb1ac4-54f9-4c9d-92b6-bf775721b61b', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, large_technology_firms).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, small_and_startup_implementers).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, end_users_and_network_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, implementer_development_community).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, open_source_communities).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, academic_and_research_institutions).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, large_technology_firms).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, patent_holders_with_essentials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author standards through open working groups using rough consensus. Participants come from competing implementer organizations but work toward specification that all can implement without gatekeeping. The group's work is the constraint's operation — no individual agent profits from the standard; the standard's value accrues to all implementers equally.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, ietf_working_groups, agenda_setter,
    organized, generational, analytical, global).

% Have resources to participate in standards-setting and to implement standards once finalized. Under open standards constraint, they cannot use control of standards process to encode proprietary gatekeeping into the specification. They benefit from interoperability with competitors (larger market, reduced fragmentation costs) and bear the cost of not being able to lock in through standards capture.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, large_technology_firms, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, large_technology_firms, payer).

% Depend on open standards to enter markets where large firms already operate. Without openness commitment, they would face proprietary standards locked in by large firms, making interoperability impossible. The constraint's core function is to preserve their exit option into standardized markets. Participate in standards-setting to protect this interest, but have less technical capacity than large firms to influence outcomes.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, small_and_startup_implementers, beneficiary,
    moderate, biographical, constrained, global).

% Depend on interoperability across implementations for internet function. Open standards mean they can choose implementations from competing vendors without lock-in to proprietary extensions. Lock-in to single vendors or incompatible extensions would fragment the internet and raise their costs or reduce their choice.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, end_users_and_network_operators, beneficiary,
    organized, generational, constrained, global).

% Engineers, researchers, and open-source developers who build implementations (libraries, servers, clients, routers, etc.). Open standards enable them to build without reverse-engineering proprietary systems. The accessibility and patent-clarity commitments are core to their ability to contribute.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, implementer_development_community, beneficiary,
    organized, biographical, mobile, global).

% Stewards the standards process rules and rough consensus principle. Maintains the procedural openness that prevents capture by organized factions. Has the formal authority to block standards that violate IPR commitments or exclude required implementers.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, ietf_iesg_and_governance, agenda_setter,
    institutional, generational, analytical, global).

% Hold patents essential to standards implementation. Under the open commitment, they must license on FRAND (fair, reasonable, and non-discriminatory) terms rather than being able to set exclusive terms or block competitors. This is a real cost: they cannot maximize extraction from their patent position when the standard is open.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, patent_holders_with_essentials, payer,
    powerful, biographical, constrained, global).

% Would benefit from proprietary standards that encode their technical approach and lock in users. The openness commitment excludes this strategy — they can participate, but cannot control outcomes to encode their advantages. Some choose not to participate or participate minimally, treating standards as constraints on their business model rather than opportunities.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, proprietary_technology_vendors, excluded,
    powerful, biographical, trapped, global).

% Depend on standards that permit free implementation and modification. Open standards with permissive patent licensing enable open-source projects to implement without legal risk. Proprietary or restricted standards would exclude them or require expensive licensing.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, open_source_communities, beneficiary,
    moderate, biographical, mobile, global).

% Use and extend standards in research and education. Open standards enable freely available implementations and documentation. Proprietary or licensing-restricted standards would create barriers to research and teaching.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, academic_and_research_institutions, beneficiary,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__commons_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__commons_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation problem: without coordination on standards, each implementer would develop proprietary extensions, incompatible protocol variants, and enclosed ecosystems. Open standards specify a shared technical target that all implementers can reach, enabling interoperability across independently developed systems. The rough consensus process distributes technical authority across all participants rather than concentrating it in any single vendor's hands.
% TRANSFER_FUNCTION: The constraint moves authority and gate-control AWAY from any single actor. Large firms transfer the ability to encode their technical approach into the spec (which they could do through a proprietary standard) and patent holders transfer exclusive licensing control (which they could exert through a proprietary standard). In exchange, all implementers receive the ability to operate in markets without proprietary lock-in. The transfer is asymmetric in scope but symmetric in direction: everyone loses exclusive control, everyone gains market access.
% ABSENT_VOICES: Proprietary technology vendors who view open standards as hostile to their business model often opt out of participation. The internet standards process is deeply embedded in the open-source and academic communities; some corporate R&D groups and technology vendors view it as structurally biased against proprietary approaches. Their objection would be that the 'openness' requirement itself is a form of gatekeeping that excludes non-open business models.
% DISAPPEARANCE_RATIONALE: If the IETF's openness commitment disappeared overnight — if standards could be captured by organized factions and proprietary patent licensing could control implementer access — the internet would fragment into incompatible protocol ecosystems controlled by individual technology vendors. Users and operators would lose the ability to mix-and-match implementations. Interoperability would collapse to vendor-sanctioned combinations. The global internet's architecture depends on the assumption that core protocols are open and royalty-free.
% FOUNDING_PROBLEM: Early internet growth created ad-hoc protocols and incompatible systems. TCP/IP won not because of technical superiority alone but because it was open, royalty-free, and implementable by anyone. As the internet grew commercially, pressure mounted to capture standards for competitive advantage — to encode proprietary approaches and extract licensing rents from implementers. The openness commitment was established to prevent standards from becoming vehicles for competitive capture.
% FOUNDING_PROBLEM_CORROBORATION: Multiple attempts at proprietary standards (ATM, some wireless protocols, cell-phone standards) demonstrate the founding problem's persistence: when openness is not enforced, technology vendors do encode their approaches into standards and extract licensing rents. Internet operators and equipment manufacturers testify repeatedly that open standards reduce their costs and preserve their choice set. Academic networks and open-source communities attest that proprietary standards would exclude them from participation. The only dissenting voices are vendors who profit from proprietary standards — and they do not claim the problem is solved, only that their model is preferable.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__commons_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__commons_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__commons_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__commons_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__commons_stewardship_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.18) under this reading because the constraint's operation does not concentrate gains in any seat: the standard specifies a technical target; all implementers reach it; no one collects rents from the constraint's operation. The value is public (interoperability) not private (captured by an agenda-setter). Suppression is very low (0.12) because the rough consensus rule does not suppress alternatives — it prevents any faction from suppressing other factions' participation. Patent holders and large firms are constrained (they cannot license exclusively or capture the standard), but constraint is not suppression; suppression would require active coercion to prevent exit or preserve alternatives. The theater ratio is minimal (0.08) because the working group's actual function (writing interoperable specifications) is identical to its stated function; there is minimal performative overhead. The measurement series shows slight upward drift in extraction (0.14 to 0.18-0.19 mid-interval, settling at 0.18) driven by increasing standardization complexity and the growing resource advantage of large firms in working group participation — a real dynamic, but within the low-extraction zone. Suppression stays constant because the procedural safeguards (openness requirement, rough consensus, IPR policies) persist unchanged across the interval.
 *
 * PERSPECTIVAL GAP:
 *   This reading predicts minimal seat divergence: from all seats (large firms, small firms, users, operators, open-source community), the constraint appears as coordination that enables rather than extracts. The small firms and open-source community see it as enabling; large firms see it as preventing capture but accept it as the cost of operating in internet standards. From the CAPTURE SUBSTRATE READING, divergence would be sharp: large firms would compute as beneficiaries (their technical advantage encodes into the standard even if they cannot formally lock it); small firms would compute as targets (they cannot compete with large-firm technical resources in working groups). From the LEGITIMACY EROSION READING, the divergence would center on whether procedural safeguards actually prevent capture or merely disguise it — that reading would see suppression and extraction hidden within the rough consensus process itself. The commons-stewardship reading sees no such hidden structure; the engine will compute whether that is correct by examining how actual working-group decisions distribute across power classes and how small-firm input translates to standard content.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is truly non-extractive under this reading because there is no structural beneficiary extracting value from constrained actors. Large firms are not beneficiaries collecting rents — they are constrained actors who cannot use the standard to lock in advantages, and they also gain from interoperability. Small firms and users are beneficiaries who gain interoperability preservation. Patent holders are constrained but not suppressed — they can still participate; they cannot extract exclusive licensing value. No actor sits at d ≈ 1.0 (full target). All actors sit in the d ∈ [0.1, 0.4] zone (mild constraint with offsetting benefit, or modest benefit with modest constraint). This distribution — no clear target, no concentrated beneficiary — is exactly what rope-with-no-extraction looks like.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate is live: the founding problem (vendors capturing standards for competitive advantage) remains live; the interoperability-preservation function still solves a real coordination problem. The founding_problem_status is live (not dead or contested in this reading's frame). The rough consensus rule still operates as intended: it distributes technical authority, prevents factions from unilaterally controlling outcomes, and preserves the possibility of dissent-and-fork if rough consensus collapses. There is no mandatrophy in this reading. The LEGITIMACY EROSION READING would claim mandatrophy (rough consensus is theatrically maintained but actually captured by organized factions) — that is a different claim requiring a different constraint story. This story does not resolve the mandatrophy claim; it denies it on structural grounds: if rough consensus were captured, the standards would encode vendor-specific advantage, which they do not systematically (interoperable implementations exist from multiple vendors with incompatible business models).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rough_consensus_capture_vulnerability,
    'Does the rough consensus rule actually prevent organized factions from controlling outcomes, or is it structurally vulnerable to capture by coordinated working-group participants?',
    'Post-hoc analysis of working-group decision records: do small-firm and open-source-community proposals survive rough consensus, or are they consistently displaced by large-firm consensus-blocking? Do organizational affiliations of consensus-declaring chairs bias outcomes? Do documented dissents change outcomes when raised?',
    'If rough consensus proves vulnerable to organized capture, the extractiveness estimate would rise substantially (0.18 → 0.40+) and the beneficiary structure would shift (no current beneficiary → large firms). The constraint would reclassify from rope to tangled_rope or snare depending on the degree of suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rough_consensus_capture_vulnerability, empirical, 'Whether rough consensus procedurally prevents capture or provides theatrical concealment of capture.').

omega_variable(
    patent_licensing_frand_enforcement,
    'Are FRAND commitments from patent holders actually enforced, or are they routinely evaded through discriminatory licensing terms, blocking, or design-around requirements?',
    'Analysis of licensing disputes and enforcement actions; survey of implementers about actual licensing experience vs. committed FRAND terms; legal precedent from patent litigation against standards implementers.',
    'If FRAND commitments are evaded, the extractiveness estimate would rise (0.18 → 0.35+) because patent holders are extracting more value than the constraint''s nominal structure allows. Suppression would rise as well if licensing disputes create barriers to implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_licensing_frand_enforcement, empirical, 'Whether FRAND patent commitments function as nominal vs. enforced constraints on licensing extraction.').

omega_variable(
    participation_equity_vs_resource_advantage,
    'Does technical resource advantage (large firms'' ability to field more engineers, more expensive analysis tools, more time) systematically translate to disproportionate influence on standard content, or are standards robust to resource asymmetry?',
    'Meta-analysis of technical proposals by firm size and affiliation: do large-firm proposals receive more adoption? Do small-firm edge cases and use cases get included in specifications? Post-standardization implementation success rates by firm size (smaller firms'' interoperability with large-firm implementations).',
    'If resource advantage translates systematically to disproportionate influence, the commons-stewardship reading''s core claim (equal constraint on all implementers) would weaken. Extractiveness would rise modestly (0.18 → 0.25+) and small firms would compute as mild targets rather than beneficiaries. The constraint would be rope-with-extraction rather than pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_equity_vs_resource_advantage, empirical, 'Whether open procedures equalize influence despite resource asymmetry, or whether large firms'' technical resources translate to structural advantage.').

omega_variable(
    commons_stewardship_vs_capture_kernel_framing,
    'Is the IETF openness commitment grounded in a genuine shared commitment to open standards as public infrastructure, or is it primarily a legitimation cover for a deeper arrangement where large firms accept openness procedurally in exchange for technical complexity that de facto requires their resources to navigate?',
    'History of IETF governance decisions: have working groups ever rejected proposals from large firms on procedural openness grounds? Have standards been simplified or kept accessible to small-firm implementers when doing so would benefit large firms? Do large firms participate in ''defeating'' rival large firms'' proposals (cross-firm alliance against single-firm capture attempts)?',
    'If the commitment proves to be primarily legitimation cover, the commons-stewardship reading would degrade and the capture-substrate reading would strengthen. Extractiveness would rise (0.18 → 0.35+), the beneficiary structure would shift toward large firms, and the constraint would reclassify as tangled_rope (coordination function + asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_stewardship_vs_capture_kernel_framing, conceptual, 'Whether the openness commitment reflects genuine shared commitment to commons stewardship or functions as legitimation cover for resource-advantage-based gatekeeping.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Can the commons-stewardship reading and the capture-substrate reading coherently coexist within a single party''s commitment framework, or do they logically foreclose each other?',
    'Examine whether the two readings can both be held by the same actor (e.g., a large firm) as empirical hypotheses about the same procedural mechanism. If they can (the firm can honestly say ''openness is real procedurally but resource advantage translates to influence''), they coexist; if the firm must choose one or the other, they foreclose.',
    'If the readings foreclose each other, the kernel reading_relations should declare forecloses rather than coexists_with. This affects how the engine''s kernel analysis treats the three readings: mutual exclusion vs. live parallelism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the commons-stewardship and capture-substrate readings logically foreclose each other or represent live alternative hypotheses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__commons_stewardship_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(ietf_tr_t35, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 35, 0.08).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(ietf_be_t35, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 35, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(ietf_su_t35, ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 35, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__commons_stewardship_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__commons_stewardship_reading, 0.05).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% The IETF openness kernel admits three structurally distinct constraint stories. The commons-stewardship reading (this constraint) models open standards as public infrastructure with low extractiveness and no structural beneficiary. The capture-substrate reading models the same procedural mechanism as a coordination surface where resource advantage encodes gatekeeping — same kernel, different ε, different beneficiary structure. The legitimacy-erosion reading models rough consensus as vulnerable to organized capture despite procedural safeguards — same kernel, extractiveness hidden within procedure. The three constraints are linked because any empirical resolution of the ambiguities in omegas 1, 4, and 5 would differentially support one reading over the others. Network directionality: commons-stewardship is the baseline coordination claim; capture-substrate and legitimacy-erosion are both 'counterfactual' hypotheses that require the baseline to exist before they can challenge it. Empirically, all three remain live — the corpus exists to measure which reading's structural account is more accurate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
