% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Imposition of Practice: Hybrid Legitimacy Through Elite Modeling and Ideological Messaging
 *   domain: political/cultural
 *
 * SUMMARY:
 *   A state apparatus imposes a new practice (dress code, calendar system,
 *   literacy standard) on a population with established traditions. The
 *   constraint succeeds not through pure decree alone, but through
 *   scaffolding infrastructure (state schools, official ceremonies, elite
 *   modeling) combined with ideological messaging that frames the imposed
 *   practice as development, modernization, or cultural progress. Urban
 *   elites benefit from adopting the practice—it signals modernity, opens
 *   employment pathways, and confers status. Rural populations and
 *   traditional practice holders bear the cost without the benefit
 *   infrastructure. This constraint is a reading of the contested kernel
 *   'legitimacy_of_imposed_practice,' specifically the reading that hybrid
 *   scaffolding (combining top-down mandate with quasi-endogenous pull
 *   through ideology and infrastructure) achieves partial displacement of
 *   traditional practice by making adoption appear voluntary while
 *   maintaining suppression of resistance.
 *
 * KEY AGENTS:
 *   - Urban elites: beneficiaries of adoption, administrators of scaffolding, visible modelers of the imposed practice
 *   - State apparatus: agenda setter, mandate issuer, suppression infrastructure maintainer
 *   - Rural populations: targets of the mandate, excluded from scaffolding infrastructure, bear costs without corresponding benefits
 *   - Traditional practice holders: identity-locked victims, authority eroded by state displacement of their role
 *   - State schools: institutional intermediary that makes adoption appear developmental rather than coercive
 *   - Excluded rival authorities: religious institutions and guilds that would contest the framing but are marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.71).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Imposition of Practice: Hybrid Legitimacy Through Elite Modeling and Ideological Messaging").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political/cultural").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '249ed86e-292e-4ab2-a345-44e074b515c9').
narrative_ontology:cs_kernel_codification('249ed86e-292e-4ab2-a345-44e074b515c9', distributed).
narrative_ontology:cs_authority_grounding('249ed86e-292e-4ab2-a345-44e074b515c9', extraction).
narrative_ontology:cs_interpretation_layer_present('249ed86e-292e-4ab2-a345-44e074b515c9').
narrative_ontology:cs_reading_relation('249ed86e-292e-4ab2-a345-44e074b515c9', legitimacy_of_imposed_practice__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('249ed86e-292e-4ab2-a345-44e074b515c9', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('249ed86e-292e-4ab2-a345-44e074b515c9', foundational, scaffolding_generates_quasi_endogenous_pull).
narrative_ontology:cs_axiom_status(scaffolding_generates_quasi_endogenous_pull, holdable).
narrative_ontology:cs_axiom_grounding('249ed86e-292e-4ab2-a345-44e074b515c9', scaffolding_generates_quasi_endogenous_pull, empirically_contingent).
narrative_ontology:cs_axiom('249ed86e-292e-4ab2-a345-44e074b515c9', foundational, legitimacy_requires_appearance_of_voluntary_adoption).
narrative_ontology:cs_axiom_status(legitimacy_requires_appearance_of_voluntary_adoption, holdable).
narrative_ontology:cs_axiom_grounding('249ed86e-292e-4ab2-a345-44e074b515c9', legitimacy_requires_appearance_of_voluntary_adoption, instrumental).
narrative_ontology:cs_reference_frame('249ed86e-292e-4ab2-a345-44e074b515c9', state_administrative_consolidation_through_unified_practice).
narrative_ontology:cs_drift_state('249ed86e-292e-4ab2-a345-44e074b515c9', contemporary_resistance_and_partial_internalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('249ed86e-292e-4ab2-a345-44e074b515c9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_practice_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt the imposed practice (Western dress, new calendar, official literacy) and gain access to state employment, international trade networks, social prestige, and political authority. They are visible modelers of the practice—their adoption in official ceremonies, schools, and public institutions creates the scaffolding that makes adoption appear developmental. They benefit both from the practice itself (employment, status) and from their role in administering the scaffolding infrastructure (as educators, ceremonial leaders, state functionaries). They have arbitrage options: they can code-switch between traditional and imposed practices, and they can exit to international cultural spaces if the mandate becomes politically unstable. Their situational advantage is that adoption confers visible status and opens pathways unavailable to those who don't adopt.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, agenda_setter).

% Issues the mandate for practice displacement and invests in scaffolding infrastructure (state schools, official dress codes, ceremonial adoption at state events, literacy requirements for civil service). Justifies the mandate publicly as modernization and national unity, privately as administrative consolidation and elimination of rival authorities. Suppresses overt resistance through appointment barriers, ritual prohibition, symbolic marginalization, and (at margins) violence. Benefits from the appearance of voluntary adoption, which generates legitimacy at lower enforcement cost than pure decree would require. Maintains the enforcement apparatus (police, bureaucracy, propaganda) that keeps suppression-requirement from falling to zero. The constraint's success—that partial practice displacement occurs—depends entirely on the state apparatus sustaining the scaffolding and the framing through successive generations.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Face pressure to abandon traditional practices (calendar systems, ritual events, dress norms) in their home regions but lack the institutional infrastructure that makes adoption visible and rewarded in urban areas. They have no employment pathways that require or reward adoption of the imposed practice. They lack access to state schools (geographically distant, transport costs, curriculum not matched to local needs) and are marginalized from state ceremonies. The traditional authorities who formerly validated local practice are suppressed by the state. They experience the mandate as loss of cultural legitimacy and social standing without corresponding gain. They have constrained exit: geographical mobility is limited (subsistence farming, land-based resources), and the mandate applies throughout the territory. The suppression they face is both structural (lack of infrastructure for advantageous adoption) and direct (marginalization, appointment exclusion if they seek state roles, ritual prohibition).
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, biographical, constrained, regional).

% Religious leaders, community elders, guilds, and knowledge-keepers whose authority and social status rested on transmission and validation of traditional practices. The state mandate displaces their institutional role—the state claims the authority to validate what practices are legitimate (through schools, official ceremonies, state-endorsed literacy). They lose social standing as younger cohorts adopt the imposed practice through state schools rather than through traditional apprenticeship. They face suppression through appointment barriers (excluded from state positions regardless of qualification), ritual prohibition (the state forbids traditional ceremonies or marginalizes them as backward), and symbolic displacement (official narratives frame traditional practice as obsolete). They are identity-locked because their very identity and authority are constituted through the traditional practice itself—abandoning it means losing the social basis of their standing. They cannot exit to other roles because their qualifications and status are tied to the traditional system.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_practice_holders, payer,
    moderate, biographical, identity_locked, regional).

% Institutional infrastructure that scaffolds the imposed practice. Schools teach the new calendar, dress norms, official language, and literacy standards to successive cohorts. Their operation creates the appearance that adoption is developmental (natural progress through education) rather than coerced displacement. Schools concentrate in urban areas, making rural participation difficult and therefore creating unequal access to the infrastructure that makes adoption advantageous. Schools benefit from state funding, legitimacy, and expansion. Their operation depends on the mandate's persistence—if practice displacement ceased being a state priority, school curriculum would change and their institutional position would shift.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_schools, beneficiary,
    institutional, generational, analytical, national).

% Religious institutions, guilds, and customary councils that previously held authority over practice standards and transmission. Are structurally excluded from the scaffolding infrastructure—they are not permitted to teach in state schools, approve modifications to dress codes, validate literacy standards in their traditional systems, or conduct public ceremonies that validate traditional practice. Their exclusion is what the enforcement machinery exists to maintain. They would testify that the mandate is experienced as coercive displacement rather than development or coordination, but they are marginalized from public discourse and policy conversation. Their voices are present in grass-roots resistance but absent from state-mediated forums.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, excluded_rival_authorities, excluded,
    moderate, biographical, trapped, regional).

% Historians, anthropologists, and political analysts who examine whether partial displacement of traditional practice reflects genuine adoption, strategic compliance under coercion, or internalized suppression. They can observe adoption patterns across urban-rural divides, examine the correlation between scaffolding infrastructure (school access, ceremony participation, ideological exposure) and adoption rates, analyze the timing of adoption shifts relative to enforcement intensity changes, and test whether the ideological framing (modernization, development, national unity) matches the actual beneficiary distribution and enforcement pattern. They occupy an analytical seat—they neither benefit nor pay directly, but they can see the structure that situated seats cannot.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, comparative_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified practice standard (calendar, dress, literacy, language) across a diverse territory with disparate local traditions. Solves the administrative problem of coordinating taxation, conscription, legal adjudication, and inter-regional trade when populations operate under different calendars and norms. Makes state administration more efficient by requiring a single set of standards rather than managing local variants.
% TRANSFER_FUNCTION: Transfers cultural legitimacy and social authority from traditional practice holders and rural populations to those who adopt the imposed practice. Transfers employment opportunity and access to state apparatus from rural to urban centers. Transfers the prestige and status associated with 'modernity' from the state to those visibly adopting the imposed practice. Transfers the economic value of local traditions (knowledge, practices, cultural products) into the state's symbolic capital (the practice becomes 'the national culture' rather than a local tradition).
% ABSENT_VOICES: Traditional practice holders and rural populations are structurally excluded from public discourse about the mandate's legitimacy and necessity. Rival authorities (religious institutions, guilds, customary councils) who would contest the framing would testify that the mandate is coercive displacement masked as development. These excluded parties would argue that the founding problem (administrative coordination) could be solved without displacing traditional practices, or that the scaffolding infrastructure's unequal distribution (concentrated in urban centers) is the core extraction mechanism rather than a necessary coordination cost.
% DISAPPEARANCE_RATIONALE: If the mandate and its scaffolding infrastructure vanished overnight, rural populations would revert to traditional practices relatively quickly (the generational lag suggests internalization is partial). Traditional authorities would regain social standing and institutional recognition. Urban elites would lose the competitive advantage their visible adoption confers. The state apparatus would lose the appearance of legitimate authority that the scaffolding manufactures. Inter-regional coordination would be more difficult, but alternative standards (negotiated, federal, market-based) would eventually emerge. The disappearance would be substantially disruptive because the constraint manufactures the appearance of voluntary adoption—without that appearance, the underlying coercion becomes visible and its political stability collapses.
% FOUNDING_PROBLEM: Territorial expansion and administrative consolidation require coordinating populations with different local practices, calendars, and authorities. Without unified standards, state taxation, military conscription, legal adjudication, and inter-regional trade are inefficient. Traditional local authorities (religious leaders, guild masters, customary councils) command loyalty independently of the state and resist centralized control. Unifying practice standards would eliminate these rival power centers and create a single framework the state can administer.
% FOUNDING_PROBLEM_CORROBORATION: State administrators testify that the founding problem remains live—administrative efficiency and national integration require unified practice standards, and rival authorities continue to resist centralization. Comparative historians and anthropologists testify that state consolidation does require some level of practice standardization, but contest whether the HYBRID scaffolding (combining mandate with infrastructure that makes adoption selectively advantageous and selectively suppressed) is necessary to solve the coordination problem. They note that pure decree fails and pure endogenous climb is slow, but argue that gentler scaffolding (incentives without suppression, infrastructure without propaganda) might achieve similar coordination with lower extraction. Rural populations and traditional authorities testify that the founding problem is not experienced as a problem from their position—the traditional authorities maintain functional local coordination—and that the mandate is experienced as displacement of their institutional authority, not as solving a coordination gap they experienced.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate-high (0.58 by interval end, rising from 0.35 at t0) because the constraint transfers cultural legitimacy and economic opportunity from traditional to imposed practice, with the transfer concentrated on the beneficiary seats (urban elites, state apparatus). The rise reflects the constraint's maturation—as the scaffolding infrastructure develops and ideological framing becomes institutionalized, the extraction becomes more efficient. Theater ratio rises sharply (0.20 → 0.48) because the scaffolding infrastructure (schools, official ceremonies, elite modeling) manufactures the appearance of voluntary adoption, masking the underlying coercion. Suppression requirement DECLINES (0.85 → 0.71) as the ideological framing takes hold and adoption appears self-motivated, reducing the direct enforcement load. This is the signature pattern of hybrid scaffolding: initial suppression is high because the mandate is naked decree; as infrastructure and messaging mature, suppression requirement falls while theater rises—the extraction is then carried by the appearance of voluntariness rather than by visible force. Accessibility collapse (0.62) reflects that alternatives (returning to traditional practice, refusing adoption) become progressively unavailable as the scaffolding infrastructure monopolizes employment, education, and social status. Resistance (0.69) is substantial because rural populations and traditional authorities actively resist, even though the resistance is increasingly channeled into hidden, identity-level suppression rather than overt conflict. The measurement series uses one shared time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus and urban elites' position, this is genuine coordination scaffolding—they see a unified practice standard as enabling commerce and administration, and they see the schools and ceremonies as developmental infrastructure. The ideological framing (modernization, progress, national unity) appears legitimate from this seat. From the rural population seat, the same structure operates as coercive cultural displacement masked by an ideology that benefits the imposers. From the traditional authority seat, the mandate is experienced as the state weaponizing the appearance of voluntariness to erode their institutional basis. The engine computes this divergence from the structural data: the same extraction metric and the same suppression-decline pattern will produce different type classifications across seats because directionality differs. The rural payer seat will classify the constraint as snare (extraction, suppression, target position); the urban beneficiary seat will classify it as rope or tangled rope (coordination with efficiency gains). This divergence is precisely what the hybrid scaffolding reading predicts—the constraint is designed to LOOK like rope from the beneficiary seat while operating as snare from the victim seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites sit near the full-beneficiary end of directionality (d ≈ 0.1–0.2): they collect employment advantage, social status, and arbitrage opportunity (they can code-switch between traditional and imposed practices). They also hold agenda-setter power, giving them control over the scaffolding. State apparatus sits between beneficiary and symmetric (d ≈ 0.3–0.4): it benefits from the coordination and the appearance of legitimacy, but must invest in enforcement infrastructure and faces the cost of maintaining the suppression apparatus. Rural populations sit near the full-target end (d ≈ 0.85–0.95): they pay the cost of cultural displacement, lack the infrastructure for advantageous adoption, and face suppression if they resist. They are constrained-exit (cannot escape the mandate's scope) with no arbitrage options. Traditional practice holders are identity-locked targets (d ≈ 0.9): their very identity is constituted through the traditional practice, so exit means existential loss, not just economic cost. The directionality asymmetry is the core structural fact: the same mandate creates different d values across seats because the scaffolding infrastructure concentrates benefits in urban centers and excludes rural areas, and because the ideological framing works differently depending on whether adoption confers status (elites) or imposes displacement (rural populations).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative coordination across diverse populations) is live but the relationship between the founding problem and the imposed practice is contested. Pure decree would fail because populations with strong traditional practices would not comply without visible pressure. Pure endogenous climb would be too slow—the state cannot wait for organic adoption. Hybrid scaffolding SUCCEEDS at partial displacement by combining mandate authority with infrastructure that makes adoption selectively advantageous (for elites) and selectively suppressed (for resisters). The mandatrophy question is: does the scaffolding component (schools, ceremonies, elite modeling, ideological messaging) represent a necessary cost of solving the coordination problem, or does it represent extraction layered onto coordination? The tangled rope classification asserts both: the scaffolding solves a real coordination problem AND extracts rents through the unequal distribution of its benefits. The measurement series shows this: extractiveness rises as the scaffolding matures (the coordination function alone would not generate the extraction level observed), and theater rises as the ideology becomes institutionalized (the appearance of voluntariness is manufactured by the infrastructure, not inherent to the practice itself).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization,
    'Is the measured suppression_requirement decline reflecting genuine normalization of the imposed practice, or is suppression increasingly internalized (felt shame, internalized inferiority of traditional practice, loss of self-efficacy for resistance)?',
    'Post-withdrawal suppression trajectory: if populations continue to adopt the imposed practice and reject traditional practices even after state enforcement apparatus is removed or substantially weakened, suppression has become internalized. If adoption rates drop significantly post-withdrawal, suppression was primarily structural and enforcement-dependent.',
    'If internalized, the constraint''s effective suppression is HIGHER than the structural measure suggests—the target carries the suppression with them after exit. The constraint is more extractive than measured because the psychological cost of resistance persists. If structural, the decline in suppression_requirement reflects genuine normalization, and the constraint may be transitioning from snare to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether measured suppression decline reflects internalization or genuine normalization').

omega_variable(
    scaffolding_necessity_boundary,
    'Is the scaffolding infrastructure (schools, ceremonies, elite modeling, ideological framing) structurally necessary to achieve practice coordination, or does it represent extraction layered onto coordination?',
    'Comparative case analysis: examine instances where practice coordination was achieved without scaffolding infrastructure (peer polities that adopted similar practices through treaty, trade exposure, or inter-elite networks without state schools or ceremonial imposition). If coordination succeeded without scaffolding, the infrastructure is extraction, not necessity.',
    'If scaffolding is extractive overlay, the constraint is snare with a coordination-framing cover story. If scaffolding is necessary, the constraint is genuinely tangled rope—coordination with asymmetric benefit distribution. This determination pivots on whether the extracted benefits (status, employment access, legitimacy) follow from the practice itself or from the SCAFFOLDING that makes adoption selective and unequal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffolding_necessity_boundary, empirical, 'Whether scaffolding infrastructure is coordination-necessary or extraction-enabling').

omega_variable(
    hybrid_mechanism_exhaustiveness,
    'Does the hybrid mechanism (mandate + scaffolding + ideology) actually generate ''quasi-endogenous pull,'' or is the appearance of voluntary adoption a theatrical accomplishment that masks continued exogenous coercion?',
    'Adoption pattern analysis: does adoption correlate with infrastructure access and ideological exposure (scaffolding mechanism predicts steep urban-rural divides, correlation with school attendance, timing alignment with ceremony participation), or does adoption show signs of endogenous climb independent of scaffolding (adoption spreads to areas without school infrastructure, adoption correlates with bottom-up organization rather than state ceremony participation)?',
    'If adoption correlates entirely with scaffolding exposure, the mechanism manufactures appearance of endogeneity without creating genuine endogenous commitment. Quasi-endogenous pull is theatrical. If adoption shows independent spread, endogenous internalization is genuine. The distinction affects the reading''s framing: is this hybrid scaffolding reading describing a successful mechanism for legitimizing coercion, or a partially successful attempt at creating genuine adoption that falls short?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_mechanism_exhaustiveness, empirical, 'Whether quasi-endogenous pull is genuine adoption or theatrical appearance').

omega_variable(
    kernel_reading_frame_dependency,
    'Does the ''legitimacy_of_imposed_practice'' kernel admit multiple framings of what legitimacy MEANS, such that the three readings are incommensurable rather than contending?',
    'Philosophical analysis of what the three readings hold legitimacy to be: exogenous reading = legal authority + compliance suffices; endogenous reading = internalized commitment required; hybrid reading = appearance of internalized commitment. If these are different CONCEPTS of legitimacy (authority vs. internalization vs. appearance), the readings are conceptually incommensurable and the kernel itself may require decomposition.',
    'If readings are incommensurable, the three constraints describe three different things (legality, internalization, theatrical legitimacy) and should be analyzed as a constraint family with distinct ε values rather than competing readings of the same constraint. If readings are commensurate (all contending for what legitimacy actually IS), the family link is structural contradiction and the engine should compute which reading''s framing survives empirical test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_dependency, conceptual, 'Whether the three readings are contending framings or incommensurable concepts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.18).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% The 'legitimacy_of_imposed_practice' kernel admits three structurally distinct readings. This constraint models the hybrid_scaffolding_reading: practice displacement succeeds through the combination of top-down mandate, scaffolding infrastructure, and ideological messaging. The sibling exogenous_override_reading models the claim that decree authority alone suffices; the endogenous_climb_reading models the claim that legitimacy requires bottom-up adoption pathways. Each reading instantiates a different ε (this reading: 0.58; siblings predicted to differ by mechanism). The readings are linked by network.affects_constraints and should be analyzed as a family to test which reading's mechanism best explains actual practice displacement patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, powerless, 0.92).
constraint_indexing:directionality_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
