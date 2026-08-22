% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: Dual-Priority AI Safety Umbrella (Both Harm Classes, Non-Competing)
 *   domain: technology governance / AI safety field-structure
 *
 * SUMMARY:
 *   Kernel ai_safety_commitment, reading dual_priority_reading: the
 *   commitment that AI safety must address existential risk and near-term
 *   deployment harms as non-competing priorities. The standing arrangement
 *   under contest is the field-level 'big tent' this reading sustains — the
 *   funding coalitions, convening structures, grant framings, and policy
 *   tracks that hold catastrophic-risk research and deployment-harms work
 *   under a single 'AI safety' identity. The arrangement solves a real
 *   collective-action problem: a rank-ordered split would set the two
 *   research communities against each other for the same funders, talent, and
 *   policy bandwidth, and would likely shrink the field's total capacity. But
 *   under genuine scarcity the two intervention types draw on substantially
 *   overlapping inputs, so every unit allocated to one is unavailable to the
 *   other — and the non-competing norm makes that competition unspeakable,
 *   framing priority-ranking claims from either side as factionalism. The
 *   arrangement's costs therefore fall on both constituent populations (the
 *   union victim set that distinguishes this reading from its single-priority
 *   siblings), while allocation discretion accrues to the umbrella's
 *   administrative center and its largest funders. The epsilon referent is
 *   the standing field arrangement, assessed by this reading's own lights:
 *   the reading itself concedes a coherence challenge on resource allocation
 *   under scarcity, so it cannot honestly rate the arrangement's extraction
 *   as negligible. CONSTRAINT FAMILY: the colloquial label 'AI safety'
 *   decomposes (epsilon-invariance) into three readings — this one,
 *   ai_safety_commitment__existential_risk_reading, and
 *   ai_safety_commitment__near_term_harms_reading — each a separate
 *   constraint story with its own victim set, epsilon, and claimed type,
 *   linked via network.affects_constraints. Claim/metric independence is
 *   preserved: claimed_type is authored from structure (genuine coordination
 *   function, identifiable victims, active enforcement), metrics from
 *   descriptive operation; where they diverge, the divergence is the
 *   measurement.
 *
 * KEY AGENTS:
 *   - ai_safety_field_builders: agenda-setter and receipt seat (institutional/arbitrage) — runs the convening, grant-framing, and agenda machinery; collects the coalition's administrative gains and allocation discretion
 *   - frontier_ai_labs: primary beneficiary (institutional/arbitrage) — collects the broad safety legitimacy that covers whatever internal allocation it chooses
 *   - policy_intermediaries: secondary beneficiary (institutional/mobile) — prefers one unified safety track over two rival regulatory agendas
 *   - existential_risk_researchers: primary payer (moderate/constrained) — agenda diluted; concentration claims suppressed by the non-competing norm
 *   - near_term_harms_advocates: primary payer (organized/constrained) — documented-harms bandwidth taxed by speculative-agenda framing
 *   - affected_communities: payer and structurally excluded voice (powerless/trapped) — bear the deployment harms the umbrella under-addresses; seated only through intermediaries
 *   - ai_safety_meta_researchers: analytical observer — maps the field's funding flows and rhetorical structure from outside the priority dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.67).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.64).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "Dual-Priority AI Safety Umbrella (Both Harm Classes, Non-Competing)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technology governance / AI safety field-structure").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '93f394a7-5ac0-4ee3-833f-cfec8f3e87df').
narrative_ontology:cs_kernel_codification('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', distributed).
narrative_ontology:cs_authority_grounding('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', distributed).
narrative_ontology:cs_reading_relation('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', ai_safety_commitment__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', ai_safety_commitment__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', foundational, both_harm_classes_non_competing).
narrative_ontology:cs_axiom_status(both_harm_classes_non_competing, holdable).
narrative_ontology:cs_axiom_grounding('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', both_harm_classes_non_competing, empirically_contingent).
narrative_ontology:cs_axiom('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', secondary, coalition_viability_requirement).
narrative_ontology:cs_axiom_status(coalition_viability_requirement, holdable).
narrative_ontology:cs_axiom_grounding('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', coalition_viability_requirement, instrumental).
narrative_ontology:cs_reference_frame('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', unified_field_coalition_baseline).
narrative_ontology:cs_drift_state('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', post_scaling_era_field, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('93f394a7-5ac0-4ee3-833f-cfec8f3e87df', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_field_builders).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, policy_intermediaries).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, affected_communities).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, resource_complementarity_thesis).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, coalition_viability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the convening structures, field-building grant programs, and agenda-setting fora through which 'AI safety' is defined as one field spanning both harm classes. They write the both-priorities framing into funding calls, conference themes, and policy agendas, and they administer the allocation discretion the unified frame creates. Their organizations' budgets, staffing, and convening authority scale with the umbrella's breadth; if the frame narrowed to either single agenda, their role as the field's center would shrink to that of one faction among several.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_field_builders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, ai_safety_field_builders, beneficiary).

% Operate under the umbrella's legitimacy: a broad safety commitment lets them point to whichever internal work — alignment research or deployment-harms mitigation — suits current pressures, while the non-competing norm discourages outsiders from demanding a specific allocation between the two. They fund and staff work in both categories at ratios they choose internally. Exit from the umbrella's terms would mean accepting a narrower, externally auditable safety definition.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, frontier_ai_labs, beneficiary,
    institutional, biographical, arbitrage, global).

% Standards bodies, advisory panels, and policy shops that process 'AI safety' as a single regulatory and advisory track. One unified agenda is cheaper to staff and negotiate than two rival ones, and the dual framing lets them convene both research communities without adjudicating between them. Their institutional relevance depends on the track staying unified.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_intermediaries, beneficiary,
    institutional, biographical, mobile, continental).

% Work on misalignment, loss of control, and catastrophic-capability risks. Under the umbrella they compete for funding, talent, and policy attention against deployment-harms work, and the non-competing norm bars them from arguing openly that their agenda warrants concentration. Grant applications and public statements must frame catastrophic-risk work as complementary to harms work. Leaving the coalition for specialized institutes costs access to the mainstream funding channels, convening tables, and policy processes where field-scale decisions are made.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, payer,
    moderate, civilizational, constrained, global).

% Civil-society organizations, accountability researchers, and organizer networks documenting and contesting present-day deployment harms. Under the umbrella their agenda shares bandwidth, funding, and policy slots with long-run speculative work, and the dual framing lets well-resourced actors count minimal harms spending as full safety compliance. They cannot exit the safety discourse — the harms they contest are deployed now — and leaving the coalition forfeits the policy access the umbrella commands.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_advocates, payer,
    organized, immediate, constrained, global).

% Communities that bear biased, exploitative, or misinformation-bearing deployments — the people the near-term agenda advocates for. They are represented at umbrella fora only through advocate intermediaries and are rarely seated where allocation between the two priorities is actually negotiated. They cannot exit AI systems deployed into hiring, housing, policing, and information environments; the umbrella's allocation choices land on them without their voice.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, affected_communities, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, affected_communities, excluded).

% Science-of-science and critical-AI-studies scholars who map the field's funding flows, agenda-setting, and rhetorical structure. They publish allocation analyses and field histories, take no side in the priority dispute, and their work is the main outside check on the umbrella's self-descriptions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_meta_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, ai_safety_field_builders).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds 'AI safety' together as a single field: one funding coalition, one convening table, one policy track spanning both harm classes. It prevents the internecine resource conflict a rank-ordered split would produce, keeps engaged the funders whose portfolios require covering both categories, and preserves shared technical infrastructure and policy bandwidth across the two research communities.
% TRANSFER_FUNCTION: Moves funder dollars, researcher attention, and policy bandwidth between existential-risk work and near-term-harms work under a single umbrella — with the direction and ratio of movement decided by the umbrella's administrative center and its largest funders rather than by either research community. Both populations contribute agenda autonomy and priority-claim rights to the center; the center returns coalition membership, legitimacy, and access.
% ABSENT_VOICES: Affected communities bear the deployment harms at stake but appear only through advocate intermediaries and are rarely seated where allocation is negotiated. Both research populations are present but structurally muffled: the non-competing norm makes the explicit priority-ranking each would demand — and the scarcity admission it rests on — unspeakable at the convening tables where the frame is maintained. They are outside the allocation room, in their own institutes, movements, and communities.
% DISAPPEARANCE_RATIONALE: If the non-competing norm and its umbrella vanished overnight, the field would split along the priority fault line: funders would pick sides or demand explicit ranking, catastrophic-risk and harms institutions would compete openly for the same talent and policy slots, labs would lose the broad legitimacy that covers whatever they internally underfund, and policy processes would face two rival 'safety' agendas instead of one. The coalition's administrative center would dissolve into factional infrastructure.
% FOUNDING_PROBLEM: Early field-builders faced a fragmented safety landscape: catastrophic-risk work and deployment-harms work ran on separate funding, separate venues, and mutually suspicious framings, and the field as a whole was too small and politically fragile for either agenda to carry funders, policy access, and talent alone. The both-priorities umbrella was built to keep the coalition whole and the field fundable while both problem classes were real and neither commanded a majority.
% FOUNDING_PROBLEM_CORROBORATION: Both payer populations attest the underlying scarcity is real, each from its own seat: catastrophic-risk researchers attest it when arguing their agenda is under-concentrated, and near-term-harms advocates attest it when arguing documented harms are underfunded relative to speculative work. Independent funding-landscape studies and the field's own meta-research corroborate that the two agendas draw on substantially overlapping pools. No attestation comes from a fully disinterested seat — every corroborator has a stake in the allocation — which is itself signal about how the founding problem is now used.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.67, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.67: both constituent populations pay real opportunity costs under the umbrella — diluted agendas, bandwidth taxed by the other priority, enforced silence about ranking — while coalition benefits (funding scale, policy access, shared infrastructure) flow partially back to both. The reading's own framework concedes scarcity, so a negligible-epsilon reading is unavailable to it; but the umbrella is not primarily a rent-collection device, which keeps extraction below snare range. Suppression 0.64 is authored as the RAW structural property (unscaled by power or scope — the engine scales only extractiveness, by directionality and spatial scope): enforcement is grant gatekeeping, convening-access control, and career sanction against ranking claims, not legal coercion. Theater 0.50: as funding boomed, performative balancing (both-priorities statements, balanced panels, framework documents) grew faster than functional allocation; half the umbrella's observable activity now asserts the frame rather than operating it. Accessibility_collapse 0.30: the alternatives — the two single-priority sibling readings — remain fully live; the umbrella competes with them rather than collapsing them, which is why resistance 0.60 is high (both payer populations actively contest the frame; the siblings ARE the resistance). Measurements run on ONE shared 7-point grid (every tracked metric authored at every point, T=0..12; points 8-12 projected). All three series rise: base_extractiveness accumulates as the norm hardened over the funding boom (an extraction-accumulation signature), theater_ratio climbs toward the Goodhart threshold, and suppression_requirement rises because enforcement machinery (gatekeeping and convening norms) visibly matured over the interval — that machinery-building is the dynamic this story traces, which is why suppression_requirement is authored at all. FNL gaming note: identity_coordination's 'this is just who we are' framing is a classic extraction cover story; the coupling check should be watched here — extraction concentrates through institutional agenda-setters at global scope while the powerless seat (affected_communities) is excluded rather than coordinated, a Power x Scope shape the identity offset must not excuse.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the field-builder seat, the umbrella is a coalition it built and administers: the non-competing frame is the field's constitution, and ranking claims are attacks on the field's viability. From the two payer seats, the same frame is the mechanism that taxes them: the catastrophic-risk researcher experiences dilution and enforced silence about concentration; the harms advocate experiences speculative spending crowding documented harm under a shared banner. From the lab seat, the umbrella is flexibility — legitimacy that covers whichever internal ratio it chooses. Same nominal field membership, three different constraints experienced. The engine computes this per-seat divergence from the structural data (power, exit, role); the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: field_builders (agenda-setter plus collector, arbitrage exit) and frontier_labs (arbitrage — they can re-weight internal allocation at will, and the umbrella legitimizes whichever half they underfund) sit nearest the beneficiary end; policy_intermediaries sit low-moderate (they collect convening relevance but bear no agenda costs). Victim declarations drive high d: both research populations are constrained-exit — leaving the umbrella costs access to the mainstream funding channels and policy tables where field-scale allocation happens — so they sit near the full-target end. Affected_communities are trapped (cannot exit deployed systems) and excluded from allocation; their costs arrive through the near-term agenda's dilution rather than through a seat at the table, which is why they are authored payer-with-excluded-secondary rather than as a pure advocacy seat. Scope is global for most seats, which amplifies effective extraction for targets (harder verification of 'both priorities served' claims across a global field) — the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric errors. Reading the umbrella as pure rope would erase what both populations pay: the dilution of each agenda and the enforced unspeakability of ranking — 'who could oppose addressing both harms?' is precisely the cover a scarcity-taxing arrangement needs. Reading it as pure snare would erase the genuine coordination function: a rank-ordered split would likely cost both agendas more than the umbrella taxes them, and coalition viability is attested by the precariousness of single-agenda institutions outside it (see omega coalition_necessity_ambiguity). The founding problem — coalition fragmentation under scarcity — is still live, so no mandatrophy declaration is made. The piton trajectory to watch: if the field ever stabilizes into separate catastrophic-risk and harms institutions with durable separate funders, the umbrella's function dies while its statements persist, and theater_ratio (already 0.50 and rising) becomes the leading indicator of a both-priorities frame maintained performatively over allocations that no longer pretend to serve both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the dual-priority framing a genuine reconciliation of the ai_safety_commitment kernel, or one more contested reading whose ''non-competing'' premise cannot survive contact with the scarcity its own framework concedes — and do the sibling readings'' victim sets and allocations differ irreconcilably?',
    'Track whether the field''s actual allocation decisions can be described without ranking (no trade-off language in major funding and policy documents) or whether ranking is ubiquitous in practice while denied in framing; compare against the sibling constraints'' independently authored victim sets.',
    'If allocation is irreducibly rank-ordered in practice, this reading collapses toward whichever sibling captures the actual ranking, and its extraction profile re-bases on that sibling''s victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether this reading is a stable reconciliation or an unstable composite of the sibling readings.').

omega_variable(
    resource_complementarity_ambiguity,
    'Are existential-risk interventions and near-term-harms interventions actually non-competing — drawing on disjoint talent, funding, and policy bandwidth — or do they compete for the same scarce inputs?',
    'Funding-landscape analysis tracing whether the same funders, researcher pools, and policy processes allocate across both intervention types (shared-pool evidence) or whether the pools are genuinely disjoint (complementarity evidence); mobility studies of researchers switching between agendas.',
    'If inputs are shared-pool, the non-competing premise fails empirically, the foundational axiom is overridden, and the arrangement''s extraction rises (every unit to one type taxes the other); if disjoint, the umbrella is closer to pure coordination and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_complementarity_ambiguity, empirical, 'The empirical core of the non-competing premise: shared versus disjoint resource pools.').

omega_variable(
    umbrella_capture_ambiguity,
    'Has the umbrella''s allocation discretion been captured by lab-friendly funders such that the big tent functions partly as a legitimacy device — letting well-resourced actors count minimal spending in one category as ''safety'' while starving the other?',
    'Compare funders'' stated dual-priority commitments against disbursement records by intervention type; audit lab safety claims against internal allocation ratios.',
    'If captured, gains consolidate further on the field-builder and lab seats and the arrangement drifts toward pure extraction; if not, the umbrella''s coordination claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(umbrella_capture_ambiguity, empirical, 'Whether the umbrella''s discretion has been captured by its largest funders.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the enforcement of the non-competing norm structural (grant gatekeeping, convening-access control, career sanctions against priority-ranking claims) or internalized (field members fused the both-priorities frame with professional identity, so ranking claims feel like heresy even absent gatekeeping)?',
    'Post-exit trajectory of researchers who leave the coalition for specialized institutes: if they immediately rank priorities without distress, enforcement was structural; if they carry the both-priorities frame with them, part is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists after any funding reform; if structural, funder-level remedies could relax it quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized enforcement of the non-competing norm.').

omega_variable(
    coalition_necessity_ambiguity,
    'Would the field actually fragment and lose capacity without the non-competing norm, or would a stable division of labor (separate institutions, disjoint funders) emerge that serves both agendas better than the umbrella does?',
    'Natural experiment: specialized catastrophic-risk institutes and harms-focused civil-society coalitions already operating outside the umbrella — compare their funding trajectories, talent retention, and policy access against umbrella-affiliated counterparts.',
    'If outside-track institutions thrive, the coordination function is weaker than claimed and the umbrella is closer to pure extraction; if they wither, coalition viability is real and the rope component is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_necessity_ambiguity, empirical, 'Whether the coalition function is necessary or the field would division-of-labor without it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_safety_dual_priority_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t0, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2, ai_safety_commitment__dual_priority_reading, theater_ratio, 2, 0.26).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t4, ai_safety_commitment__dual_priority_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t4, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t6, ai_safety_commitment__dual_priority_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t6, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t8, ai_safety_commitment__dual_priority_reading, theater_ratio, 8, 0.43).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t8, projected).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t10, projected).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t12, ai_safety_commitment__dual_priority_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(ai_safety_dual_priority_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t0, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t4, ai_safety_commitment__dual_priority_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t4, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t6, ai_safety_commitment__dual_priority_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t6, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t8, ai_safety_commitment__dual_priority_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t8, projected).
narrative_ontology:measurement(ai_safety_dual_priority_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t10, projected).
narrative_ontology:measurement(ai_safety_dual_priority_be_t12, ai_safety_commitment__dual_priority_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_safety_dual_priority_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t0, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2, 0.46).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t4, ai_safety_commitment__dual_priority_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t4, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t6, ai_safety_commitment__dual_priority_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t6, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t8, ai_safety_commitment__dual_priority_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t8, projected).
narrative_ontology:measurement(ai_safety_dual_priority_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t10, projected).
narrative_ontology:measurement(ai_safety_dual_priority_su_t12, ai_safety_commitment__dual_priority_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI safety' conflates three structurally distinct commitments (epsilon-invariance decomposition). This story authors the dual-priority reading only: victim set = union of both populations, resources distributed across both intervention types, coherence challenge under scarcity. The sibling stories — ai_safety_commitment__existential_risk_reading (concentrated resources on catastrophic risk; its victim set is the constituencies whose present-harm concerns are displaced) and ai_safety_commitment__near_term_harms_reading (concentrated resources on documented harms; its victim set is the constituencies whose long-run concerns are dismissed) — carry their own epsilon, stakeholders, and claimed types. This reading influences both siblings: the umbrella absorbs their funding channels and reframes their exclusivity claims as factionalism, changing their resource and legitimacy conditions without foreclosing them. Each family member links to the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
