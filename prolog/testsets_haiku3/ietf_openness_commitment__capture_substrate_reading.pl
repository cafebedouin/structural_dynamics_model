% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Standards Process as Capture Substrate
 *   domain: technology governance / institutional economics
 *
 * SUMMARY:
 *   The IETF's rough-consensus standards process is presented as an open,
 *   transparent mechanism for Internet technical coordination. This
 *   constraint story instantiates the CAPTURE SUBSTRATE READING: the
 *   standards process itself is reframed as a coordination mechanism whose
 *   benefits are captured by large platform operators who can afford to
 *   participate at scale. Resource advantage (ability to employ standards
 *   specialists, attend meetings, fund working group infrastructure,
 *   implement competing variants) translates into the ability to encode
 *   architectural preferences as mandatory requirements in the standard.
 *   Small implementers and open-source projects then face a choice: implement
 *   the proprietary extensions that large operators use (contradicting the
 *   standard's openness) or fragment from interoperability. The
 *   rough-consensus procedure, meant to prevent capture, becomes the theater
 *   covering it — the procedure looks open (anyone can propose, meetings are
 *   on-the-record, mailing lists are public) while structural resource
 *   inequality determines outcomes. This reading competes with the COMMONS
 *   STEWARDSHIP READING (standards as genuine public infrastructure, rough
 *   consensus as effective safeguard) and the LEGITIMACY EROSION READING (the
 *   procedure itself as fundamentally vulnerable to organized pressure). The
 *   kernel is the IETF's commitment to openness and consensus; the readings
 *   diverge on whether that commitment is honored, captured, or eroded.
 *
 * KEY AGENTS:
 *   - large_platform_operators: Institutional power, arbitrage exit, global scope — employ armies of standards specialists and can implement proprietary extensions; they are both beneficiaries (their preferences get encoded) and agenda-setters (they shape the standard's direction).
 *   - small_implementers: Powerless, constrained exit, global scope — must implement to interoperate but lack resources to participate; they bear the cost of proprietary-extension fragmentation.
 *   - open_source_projects: Moderate power, identity-locked exit, global scope — ideologically committed to openness but trapped between implementing proprietary extensions (betraying their mission) or fragmenting from compatibility.
 *   - end_users: Powerless, trapped exit, global scope — benefit from the standard's interoperability premise but experience fragmentation when proprietary extensions split implementations.
 *   - IETF working groups: Organized power, constrained exit, global scope — nominally independent but resource-dependent on large-operator participation and funding.
 *   - standards_advocacy_groups: Moderate power, constrained exit, global scope — excluded from most negotiations despite having legitimate representation interests (accessibility, interoperability, equity).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.62).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.48).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Standards Process as Capture Substrate").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology governance / institutional economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'a43568a3-2e99-4382-bc1c-b168050e5e7a').
narrative_ontology:cs_kernel_codification('a43568a3-2e99-4382-bc1c-b168050e5e7a', formalized).
narrative_ontology:cs_authority_grounding('a43568a3-2e99-4382-bc1c-b168050e5e7a', distributed).
narrative_ontology:cs_reading_relation('a43568a3-2e99-4382-bc1c-b168050e5e7a', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('a43568a3-2e99-4382-bc1c-b168050e5e7a', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('a43568a3-2e99-4382-bc1c-b168050e5e7a', foundational, resource_advantage_translates_to_agenda_control).
narrative_ontology:cs_axiom_status(resource_advantage_translates_to_agenda_control, holdable).
narrative_ontology:cs_axiom_grounding('a43568a3-2e99-4382-bc1c-b168050e5e7a', resource_advantage_translates_to_agenda_control, empirically_contingent).
narrative_ontology:cs_axiom('a43568a3-2e99-4382-bc1c-b168050e5e7a', secondary, openness_procedure_masks_captured_outcomes).
narrative_ontology:cs_axiom_status(openness_procedure_masks_captured_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('a43568a3-2e99-4382-bc1c-b168050e5e7a', openness_procedure_masks_captured_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('a43568a3-2e99-4382-bc1c-b168050e5e7a', open_rough_consensus_interoperability).
narrative_ontology:cs_drift_state('a43568a3-2e99-4382-bc1c-b168050e5e7a', contemporary_platform_dominance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a43568a3-2e99-4382-bc1c-b168050e5e7a', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, open_source_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, end_users).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, rough_consensus_proceduralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major technology companies (Google, Apple, Microsoft, Amazon, Meta) employ large teams of standards specialists and can absorb the cost of RFC authorship, meeting attendance, and implementation of competing specifications. They shape the agenda through volume of participation, funding of infrastructure (IETF chairs, working group editors), and ability to implement multiple protocol variants simultaneously. They benefit from standards that encode their existing architectural choices or create compatibility lock-in requiring proprietary extensions to interoperate.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary).

% Small teams, startups, and single-engineer projects must implement to the same standard but lack resources to participate in the standards process. They must either implement exactly what the large operators' implementations do (proprietary extensions included) to achieve interoperability, or invest disproportionate effort reverse-engineering undocumented behavior. Non-participation in standard-setting means they become victims of decisions they cannot influence.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    powerless, biographical, constrained, global).

% OSS projects implement standards in response to community demand, not to derive direct revenue. They face pressure to implement proprietary extensions (encryption schemes, codec optimizations, API shims) that large operators use to differentiate, even when those extensions contradict the stated openness of the standard. Refusing to implement creates feature parity gaps that harm their users; implementing adds unmaintained complexity and technical debt.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, open_source_projects, payer,
    moderate, biographical, identity_locked, global).

% End users benefit from a common standard (ability to use any application on any device) but bear the cost when proprietary extensions fragment the standard's benefit. A browser that does not support a proprietary encryption or codec cannot reach content; applications that are incompatible with open-source clients cannot serve users outside proprietary ecosystems. Users cannot participate in standards bodies and experience fragmentation as inevitable feature limitation.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, end_users, beneficiary).

% Volunteer and semi-volunteer working group chairs and editors do the actual standards drafting. They operate under a rough-consensus decision rule and are nominally independent of commercial interests. However, they depend on large-operator participation for implementation feedback, rely on large-operator funding for infrastructure, and face resource constraints that advantage large-operator delegates who can attend every meeting and draft comprehensive implementation reports.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_working_groups, agenda_setter,
    organized, biographical, constrained, global).

% The IETF Chair and Area Directors oversee working groups and enforce procedural rules (rough consensus, open process, on-the-record decisions). They are theoretically independent but operationally dependent on large-operator funding, participate in the same meetings where large-operator standards specialists are present, and lack resources to conduct independent technical review of competing proposals.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_leadership, observer,
    institutional, generational, analytical, global).

% Civil society, consumer advocacy, and developing-country technology communities would argue for accessibility, interoperability, and non-discriminatory patent licensing in standards. They are structurally underrepresented in IETF proceedings (cost of travel, English-language meetings, Western institutional affiliation norms) and largely excluded from the informal coalition-building where standards outcomes are decided.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, standards_advocacy_groups, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A common Internet standards process solves the coordination problem of interoperability: all implementers working from the same specification reduces fragmentation, lowers implementation cost, and enables networks of diverse devices to communicate. The rough-consensus procedure is meant to ensure that no single party dominates the standard's design.
% TRANSFER_FUNCTION: Moves the ability to set technical requirements (encoded in the standard's normative language, mandatory extensions, and compatibility requirements) from the implementation-level detail to the standards body. Large operators with resources to participate in standards bodies extract the ability to encode their architectural choices as mandatory requirements, shifting implementation cost to smaller operators and open-source projects that must reverse-engineer or reimplement to achieve compatibility.
% ABSENT_VOICES: Small implementers, developing-world technologists, end-user advocates, and civil-society organizations concerned with access and interoperability are structurally absent from most standards negotiations. They would argue against proprietary extensions encoded as normative requirements, for simpler baseline standards with well-specified extension points, and for mandatory patent licensing. Their absence means standards decisions are made by and for large implementers.
% DISAPPEARANCE_RATIONALE: If the IETF standards process and its rough-consensus rule disappeared overnight, the Internet would fragment into proprietary protocol ecosystems (corporate-controlled stacks, regional standards bodies, incompatible implementations). The coordination problem would not solve itself; interoperability would become a commercial negotiation between large operators, not a technical design. Some form of standards body would be reinvented within months, likely with less pretense of openness.
% FOUNDING_PROBLEM: In the 1980s and 1990s, incompatible vendor-specific protocol implementations fragmented the nascent Internet, raising interoperability costs and preventing widespread adoption. The IETF's rough-consensus process was designed to ensure that standards reflected genuine technical needs rather than vendor lock-in, that participation was open and transparent, and that no single commercial interest could dominate design decisions.
% FOUNDING_PROBLEM_CORROBORATION: The IETF and large operators attest the founding problem remains live: vendors still try to diverge from standards, and rough consensus prevents proprietary capture. Small implementers, open-source maintainers, and competition analysts attest the founding problem is partially solved (base interoperability exists) but the process itself has become the vector for modern capture: resource advantage in the standards process translates to encoded gatekeeping that fragments implementations around proprietary extensions (testimony from Mozilla developers, Tor Project, and academic research on standards capture in Barbosa et al., 2023). The empirical split between the two readings is now the substance of internet governance debate.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) and rising over the interval because the ability to encode proprietary requirements in a standard is a form of rent extraction: small implementers must pay the cost of reimplementing or must accept proprietary extensions. Suppression is moderate (0.48) because the process is genuinely open (not closed-door) but resource constraints systematically exclude smaller parties; the suppression is structural rather than overt — small players can show up and speak, but the agenda-setting happens in the resource-intensive preparation meetings and multi-year RFC drafting cycles they cannot afford to participate in. Theater is moderately high (0.58) and rising because the rough-consensus procedure creates the appearance of openness and fairness (all voices heard, decisions made transparently) while structural inequality determines whose voice matters. The measurement series tracks the interval from the early 2010s (when the capture became visible to academics and OSS communities) through 2035, showing steadily rising extractiveness as large operators learned to use the standards process as a bundling mechanism (encode compatibility requirements that force small implementers to implement proprietary extensions to interoperate). Accessibility collapse is low-moderate (0.41) because alternatives exist (proprietary stacks, regional standards bodies, refusal to standardize) but are costly enough that the IETF remains the path of least resistance. Resistance is high (0.73) because OSS communities and academics consistently challenge large-operator agenda-setting, and there is recurring pressure for reform of the standards process — but resistance has not yet changed the structural dynamics.
 *
 * PERSPECTIVAL GAP:
 *   From the large_platform_operators' seat, the constraint is genuine coordination: the standard solves real interoperability problems, the rough-consensus process prevents any single operator from dominating (because large operators disagree with each other on some issues), and their market participation legitimately influences standards they will implement at scale. From the small_implementers' and OSS_projects' seats, the same constraint is extracted coordination: the standard appears open but is captured by resource advantage, small players have no real voice despite the open process, and the encoding of large-operator preferences as mandatory requirements is a form of regulatory capture. The engine should compute different types from these different seats: the large-operator seat should compute as ROPE or low-TANGLED-ROPE (genuine coordination with side-effect extraction), while the small-implementer seat should compute as SNARE or high-TANGLED-ROPE (pure extraction disguised as coordination). This divergence is the diagnostic payload: when the same constraint structure produces different types from different seats, it is exactly the signature of a capture substrate — the beneficiary seats experience coordination, the victim seats experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The large_platform_operators seat experiences this constraint as moderate beneficiary (d near 0.3–0.4): they participate in setting the standard, their preferences get encoded, they benefit from compatibility codified around their architecture, but they also bear costs (maintaining compatibility with smaller implementers, defending their preferences against criticism). The small_implementers seat experiences it as pure target (d near 0.8–0.9): they must implement to standards they did not shape, absorb the cost of proprietary-extension fragmentation, and have no exit (not implementing means no market access). The open_source_projects seat is the most complex (d near 0.6–0.7): they genuinely believe in openness and participate in the standards process from civic commitment, but their identity is locked into openness and they are systematically victimized by the capture dynamic (pressured to implement proprietary extensions). The end_users seat is diffusely victimized (d near 0.7); they do not participate and experience the constraint's cost indirectly through fragmented implementations. The IETF_working_groups seat is split: they are nominally the decision-makers (low d, beneficiaries of the legitimacy the openness narrative confers) but operationally constrained by large-operator resource dominance and funding dependence (medium d, moderately victimized by capture dynamics).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (vendor fragmentation, proprietary lock-in, incompatibility costs) was genuine in the 1980s–1990s and is partially solved by the existence of the IETF and rough-consensus standards. However, the founding problem as originally conceived (any single large operator trying to dominate) is not the problem the current constraint solves — the current constraint solves the problem of COORDINATING LARGE OPERATORS while externalizing the fragmentation cost to smaller implementers. The rough-consensus procedure, meant to prevent capture, has become the theater covering capture. The mandate has not disappeared (interoperability remains necessary) but its beneficiary structure has inverted: the standard still enables interoperability, but in a fragmented way that requires proprietary extension compatibility to achieve. A mandatrophy analysis would ask: does fixing this constraint (making the standards process truly open to small implementers, forbidding proprietary mandatory extensions, enforcing patent-free licensing) solve a problem that still needs solving, or does it just redistribute the coordination cost? The COMMONS STEWARDSHIP READING answers 'the standard is doing fine, just enforce openness more strictly'; the CAPTURE SUBSTRATE READING answers 'the constraint has been repurposed to extract from smaller parties and cannot be fixed without restructuring participation'. The LEGITIMACY EROSION READING answers 'the procedure is so damaged by organized pressure that even good-faith enforcement cannot restore it.' These are three different diagnoses of the same founding problem's obsolescence or inversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_procedure_vs_captured_outcomes,
    'Is the appearance of openness (transparent process, on-the-record decisions, formal consensus rules) sufficient to ensure unbiased outcomes, or does the procedure itself require equal resource access to function as designed?',
    'Randomized trial or controlled comparison: provide proportional funding to small-implementer and OSS delegations to achieve parity in meeting attendance and RFC drafting capacity, then measure whether standards outcomes shift toward different technical choices.',
    'If outcomes shift significantly toward different technical requirements when resource parity is achieved, then the openness is procedural theater and the constraint is SNARE. If outcomes remain unchanged, then large operators are genuinely making better technical arguments and the constraint is ROPE despite resource inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_procedure_vs_captured_outcomes, empirical, 'Whether open procedure yields unbiased outcomes without equal resource access.').

omega_variable(
    proprietary_extension_necessity,
    'Are the proprietary extensions large operators encode in standards technically necessary for interoperability, or are they compatibility mechanisms designed to lock in switching costs?',
    'Technical analysis of extension specifications against stated interoperability goals; measurement of interoperability achievable with baseline standard alone vs. with proprietary extensions required.',
    'If extensions are necessary (backward-compatibility with legacy systems, handling edge cases), then the extraction is a side effect of genuine coordination. If extensions are design choices that could be replaced with simpler alternatives, then they are rent-seeking mechanisms and the constraint is SNARE.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proprietary_extension_necessity, empirical, 'Whether proprietary extensions in standards serve technical coordination or switching-cost lock-in.').

omega_variable(
    kernel_reading_divergence,
    'Is the observed capture the result of the rough-consensus procedure itself failing (LEGITIMACY_EROSION_READING), or the result of resource inequality corrupting an otherwise sound procedure (CAPTURE_SUBSTRATE_READING)?',
    'Historical analysis of standards outcomes before and after periods of large-operator dominance; audit of working groups with and without large-operator participation; measurement of correlation between operator participation and outcome alignment with operator preferences.',
    'If large-operator exit from a working group shifts standards outcomes away from their preferences, then the procedure itself is sound and capture is remediable (CAPTURE_SUBSTRATE_READING true). If standards outcomes track large-operator interests even when they are minimally present, then the procedure has been compromised and is not remediable by participant reform (LEGITIMACY_EROSION_READING true).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Whether capture is remediable through resource equity or reflects irrecoverable procedural compromise.').

omega_variable(
    rough_consensus_definition_stability,
    'What constitutes ''rough consensus'' in the standards process? Is this definition stable, consistently applied, or has it drifted to accommodate large-operator preferences?',
    'Analysis of rough-consensus determination across RFC 2026 definition vs. actual practice in recent standards (2015–2025); coding of consensus assessments by independent reviewers to detect drift or bias.',
    'If consensus determination has drifted to favor large-operator positions (e.g., defining ''consensus'' as ''no major implementer objects'' where major=resource-rich), then the procedure itself is corrupted. If consensus definition is stable, then outcomes reflect genuine preference aggregation, and divergence comes from resource inequality rather than procedural corruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rough_consensus_definition_stability, empirical, 'Whether the rough-consensus rule is stable and consistently applied or has drifted to accommodate capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 5, 0.47).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 15, 0.56).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__capture_substrate_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% The IETF's openness commitment decomposes into three structurally distinct constraint stories corresponding to three incompatible readings of the kernel. The CAPTURE_SUBSTRATE_READING (this file) models the standards process as a resource-inequality mechanism where participation openness masks outcome capture. The COMMONS_STEWARDSHIP_READING models the same process as working coordination, with rough consensus effectively preventing any single operator from dominating. The LEGITIMACY_EROSION_READING models rough consensus itself as procedurally compromised and unable to resist organized pressure. The three readings are not observationally equivalent — they make different predictions about what would happen if resource parity were achieved among participants, whether proprietary extensions are technically necessary, and whether the rough-consensus definition has drifted over time. They have been separated into three constraint files (one per reading) to allow the engine to compute per-seat classifications and detect misalignment between the readings' structural predictions and empirical outcomes. The three readings coexist as live positions in current internet governance debate; no reading has been formally foreclosed by the IETF.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__capture_substrate_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
