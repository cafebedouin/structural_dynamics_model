% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Maintenance as Distributed Cosmic Responsibility
 *   domain: religious/political
 *
 * SUMMARY:
 *   In the distributed Ma'at maintenance reading, cosmic order is sustained
 *   through the proper conduct of all social stations acting within their
 *   prescribed roles. The Pharaoh does not monopolize responsibility; the
 *   priesthood interprets cosmic requirements independently; officials,
 *   artisans, and laborers each bear genuine accountability for maintaining
 *   Ma'at through their work and conduct. This reading inverts the common
 *   framing of ancient Egypt as purely hierarchical despotism—instead, it
 *   models a distributed, if strictly ranked, system of moral agency. The
 *   constraint coordinates all actors around the shared project of cosmic
 *   stability while preserving the rank structure; extraction is minimal
 *   because all benefit equally from a stable cosmos and bear real cost from
 *   its disruption.
 *
 * KEY AGENTS:
 *   - Pharaoh: Primary exemplar and enforcer of Ma'at, identity-locked to the role, fully accountable but also the highest-status beneficiary
 *   - Priesthood: Independent interpreters of cosmic requirements, organized power base, authority grounded in demonstrated competence rather than subordination to royal will
 *   - Officials and administrators: Distributed agents of justice and order, replaceable if they fail Ma'at maintenance, accountable to both Pharaoh and priesthood
 *   - Artisans and merchants: Moderate-power participants whose conduct in fair dealing directly affects cosmic order
 *   - Common laborers: Lowest-station participants whose obedience and diligence are treated as essential contributions to cosmic maintenance, though their voice in disputes is minimal
 *   - Scribal tradition: Custodian of precedent, enabling accountability by recording past removals and corrections
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.28).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.15).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Maintenance as Distributed Cosmic Responsibility").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "religious/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, 'a3723666-500b-4a0b-90b8-543e4f8a10eb').
narrative_ontology:cs_kernel_codification('a3723666-500b-4a0b-90b8-543e4f8a10eb', distributed).
narrative_ontology:cs_authority_grounding('a3723666-500b-4a0b-90b8-543e4f8a10eb', practice).
narrative_ontology:cs_interpretation_layer_present('a3723666-500b-4a0b-90b8-543e4f8a10eb').
narrative_ontology:cs_reading_relation('a3723666-500b-4a0b-90b8-543e4f8a10eb', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3723666-500b-4a0b-90b8-543e4f8a10eb', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('a3723666-500b-4a0b-90b8-543e4f8a10eb', foundational, maintenance_accountability_distributed).
narrative_ontology:cs_axiom_status(maintenance_accountability_distributed, holdable).
narrative_ontology:cs_axiom_grounding('a3723666-500b-4a0b-90b8-543e4f8a10eb', maintenance_accountability_distributed, deontological).
narrative_ontology:cs_axiom('a3723666-500b-4a0b-90b8-543e4f8a10eb', foundational, authority_grounded_in_performance).
narrative_ontology:cs_axiom_status(authority_grounded_in_performance, holdable).
narrative_ontology:cs_axiom_grounding('a3723666-500b-4a0b-90b8-543e4f8a10eb', authority_grounded_in_performance, conventional).
narrative_ontology:cs_reference_frame('a3723666-500b-4a0b-90b8-543e4f8a10eb', distributed_station_accountability).
narrative_ontology:cs_drift_state('a3723666-500b-4a0b-90b8-543e4f8a10eb', late_period_egypt, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3723666-500b-4a0b-90b8-543e4f8a10eb', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, all_station_occupants).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, cosmic_order_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, administrative_officials).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, artisans_merchants).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, common_laborers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains Ma'at through governance, ritual performance, and exemplary conduct in the highest station. Under this reading, the Pharaoh's role is not sole arbiter but primary exemplar whose conduct sets the template for all other stations. Failure in ritual or justice jeopardizes cosmic order and triggers accountability from other maintenance actors. The Pharaoh collects deference and ritual priority but must demonstrate Ma'at maintenance continuously.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh, beneficiary).

% Maintains Ma'at through ritual performance, cosmological knowledge, and interpretation of divine will. Positioned as independent assessors of whether cosmic order is sustained; their authority derives from demonstrated competence in maintenance practices, not from inherent status. Can dispute Pharaoh's conduct as disruptive to Ma'at; their role is active interpretation of maintenance requirements, not mere execution of royal will.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priesthood, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, priesthood, beneficiary).

% Maintain Ma'at through just administration, record-keeping, dispute resolution, and proper taxation. Accountable for their station's conduct; Ma'at failure (corruption, injustice, negligence) can trigger removal or replacement. Authority grounded in functional maintenance, not inherited right.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, administrative_officials, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, administrative_officials, beneficiary).

% Maintain Ma'at through honest work, fair exchange, and proper respect for hierarchical relationships. Their conduct in trade, craft, and social relations directly affects cosmic order. Can appeal to priesthood or officials if higher-station actors disrupt Ma'at.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, artisans_merchants, beneficiary,
    moderate, biographical, constrained, regional).

% Maintain Ma'at through diligent work, obedience to legitimate authority, and fulfillment of reciprocal obligations within their station. Their participation in cosmic order is real and binding; collective failure (widespread laziness, vice, disorder) is believed to ripple into cosmological imbalance. Have minimal recourse if higher stations abuse them, but are not excluded from accountability for their own conduct.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, common_laborers, beneficiary,
    powerless, biographical, trapped, local).

% Records and transmits the principle of distributed Ma'at maintenance across generations. Authority as custodian of precedent and written practice. Can reference recorded instances where pharaohs or officials were removed or corrected for Ma'at violation, establishing accountability as binding, not merely aspirational.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, scribal_tradition, observer,
    organized, civilizational, constrained, national).

% Have no voice in defining or maintaining Egyptian Ma'at; their own conduct is outside the cosmos the principle organizes. Portrayed as chaotic (isfet), not Ma'at-participating. Excluded from the moral framework of distributed responsibility; treated as objects of domination rather than stations within a shared order.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_powers, excluded,
    powerful, biographical, trapped, global).

% The meta-beneficiary of the constraint. Under the distributed reading, cosmic order is maintained only through the continuous proper conduct of all stations; no single actor can maintain it alone. The order itself legitimizes the hierarchical arrangement and the distributed accountability.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(maat_order_principle__distributed_maintenance_reading, cosmic_order).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of cosmic order maintenance across a hierarchical society: without a principle that distributes accountability across all stations, high-station actors could neglect their role and blame chaos on lower stations, or lower stations could claim they bear no responsibility for order. The distributed reading coordinates the entire population around continuous maintenance efforts proportionate to their station.
% TRANSFER_FUNCTION: Moves deference, labor, and obedience upward from lower to higher stations, and moves legitimacy-to-rule and cosmic responsibility downward from higher to lower stations. No material extraction per se; the 'transfer' is of relational positioning and accountability claims.
% ABSENT_VOICES: Foreigners and those outside Egyptian civilization are excluded from the moral framework entirely—they would argue that cosmic order is not tied to hierarchical Egyptian conduct, but this reading does not admit external validation. Women's voices are attested in some sources but are not systematically integrated into the surviving administrative records of Ma'at maintenance; female heads of household and priestesses exist but their role in distributed maintenance is unevenly documented.
% DISAPPEARANCE_RATIONALE: If the principle of distributed Ma'at maintenance vanished—if Pharaoh and priesthood claimed cosmic order depended on them alone and masses were excluded from moral agency—the entire legitimacy structure for daily compliance would shift. Subjects would lose the narrative link between their personal conduct and cosmic stability; authority would rest on force alone rather than participatory order-maintenance. The social organization of ritual, justice, and economic life would require reconstruction without the distributed accountability frame.
% FOUNDING_PROBLEM: Early dynastic Egypt faced chronic coordination failures: local officials acted with impunity, the Pharaoh could not monitor all conduct, and natural disasters (low Nile, famine) required explanation that was not merely 'the Pharaoh failed.' The distributed Ma'at principle solved this by making every actor a stakeholder in cosmic order and every breach—from the highest to the lowest—a potential rupture requiring repair.
% FOUNDING_PROBLEM_CORROBORATION: Temple inscriptions and administrative records from the New Kingdom attest that Ma'at breaches by officials and commoners alike triggered investigation, replacement, or punishment, supporting the distributed accountability reading. Some sources present the Pharaoh as sole guarantor (divine_mandate_reading); others, particularly Wisdom Literature and instructions to officials, emphasize that each station has non-negotiable Ma'at obligations. The scribal tradition records cases of officials removed for corruption, reinforcing that maintenance accountability was not nominal. However, no source explicitly states the doctrine in the distributed form—this reading synthesizes from practice rather than from a single authoritative text.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 average across the interval) because the constraint is genuinely coordinative: all stations benefit from a stable cosmos and would suffer equally if Ma'at dissolved. There is no concentration of gain in a single actor; the Pharaoh's authority is real but so is the priesthood's independent voice and officials' accountable tenure. Suppression is very low (0.15) because the constraint does not rely on coercion to maintain participation—participation flows from genuine belief that personal conduct affects cosmic order and from the distributed accountability that makes compliance self-reinforcing (if I neglect my station's duties, others will point it out and the cosmos might destabilize, harming me). Theater ratio is modest (0.22) because while ritual performance is central and some of it is ceremonial, the underlying principle (distributed maintenance) drives real behavioral change across all stations. The measurement series show slight downward drift in extractiveness (ending at 0.28) and slight upward drift in theater (ending at 0.25), suggesting that as the principle becomes more established and internalized, the performative aspects slightly increase while the purely extractive elements (e.g., claims to sole Pharaonic responsibility) weaken.
 *
 * PERSPECTIVAL GAP:
 *   The Pharaoh and priesthood sit at the opposite ends of a control spectrum under this reading. The Pharaoh experiences the constraint as empowering (cosmic order flows through the Pharaoh's conduct and is reinforced by it) but also as a burden (any cosmological catastrophe is attributed to Pharaonic failure). The priesthood experiences it as autonomy (their independent assessment of cosmic state gives them interpretive power and can challenge the Pharaoh's claims). Officials experience it as mixed: high responsibility but also high security (they can be removed if they fail, but they have the status and material benefit of office). Common laborers experience it as moral inclusion but also as a no-exit trap: they are accountable for cosmic order but have no means to contest higher-station actors' claims about what the cosmos requires. The engine should compute these per-seat types differently: the Pharaoh's directionality might be near 0.5 (beneficiary of authority but burdened by accountability); the priesthood's near 0.2 (beneficiary of interpretive autonomy, moderate exit via replacement); officials' near 0.4 (mixed burden and benefit); common laborers' near 0.7 (trapped, accountable, minimal voice). The constraint appears as Rope from most seats but as Snare from the powerless perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the distributed reading, all stations are beneficiaries of cosmic order (high directionality toward beneficiary end for all). But directionality also tracks power and exit: the Pharaoh is identity-locked to the role (exit impossible without losing all status and meaning) and institutional power (directionality ~0.35–0.45, mixed beneficiary/burdened). The priesthood is organized power with some ability to challenge and some exit via replacement (directionality ~0.25–0.35, leaning beneficiary). Officials have biographical time horizons and constrained exit—they can be removed, so their accountable position is more binding than the priesthood's (directionality ~0.40–0.50). Artisans and merchants have moderate power and constrained exit; they benefit from cosmic stability and just administration, but they cannot exit the system (directionality ~0.35–0.45). Common laborers are powerless, trapped, and bear accountability without voice (directionality ~0.65–0.75, substantially toward target end). The victims array is empty because no actor is nominally victimized; all are described as participants. However, the powerless seats in this constraint are de facto targets—they bear the burden of accountability with minimal exit—so the per-seat engine computation should flag Snare-type extraction for the lowest seats despite the constraint's rope-like structure from the Pharaoh's seat. Directionality overrides might be needed to reflect that priesthood seats, despite being organized, are substantially beneficiaries of the distributed principle (they gain interpretive power), so d_value ~0.25 for organized power in priestly roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating cosmic order maintenance across a rigid hierarchy without collapse) is LIVE throughout the interval—no regime in ancient Egypt abandoned the Ma'at principle as the basis for legitimacy. However, the founding problem's SOLUTION shifted: in early dynastic Egypt, the Pharaoh alone was the guarantor; by the Middle Kingdom, distributed accountability was explicit (officials were removed, priesthood was independent, records show public acknowledgment of these facts). By the late period, the principle persists but is increasingly theatrical—the actual accountability mechanisms weaken (officials become more hereditary, priesthood becomes more autonomous from state control, common people have less recourse), yet the language of distributed Ma'at maintenance remains the legitimacy frame. This is not mandatrophy in the strict sense (the founding problem is not dead) but it is an approach to it: the functional distribution of maintenance erodes while the principle is maintained ceremonially. The measurement series should capture this: extractiveness is low early (genuine distributed accountability) and slightly rises later (more hereditary and less replaceable); theater is constant-to-rising (the principle is performed even as actual accountability erodes). No mandate-obsolescence flag would fire here, but the trend is toward piton-like degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_vs_concentrated_interpretation,
    'Was Ma''at maintenance genuinely distributed across all stations as the sources suggest, or was the distributed language a cover story for concentrated Pharaonic power that benefited from all other stations'' compliance?',
    'Archaeological and administrative evidence of official removals, priestly independence, and accountability: if high-ranking officials were demonstrably removed for Ma''at violations and the decision involved priesthood input or public acknowledgment, distributed accountability was real; if removals were rare or purely secretive, the language was cover.',
    'If distributed accountability was genuine practice, the constraint is a true Rope. If the distributed language was purely instrumental (allowing the Pharaoh to delegate blame for failures while preserving authority), it is a Tangled Rope or Snare with extracted legitimacy and distributed account-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_vs_concentrated_interpretation, empirical, 'Whether the distributed Ma''at principle reflected actual institutional practice or was a performative overlay on concentrated power.').

omega_variable(
    powerless_station_moral_agency,
    'Were common laborers and enslaved persons genuinely treated as moral agents whose conduct affected cosmic order, or was their accountability a rhetorical device to enforce compliance without consulting their voice?',
    'Evidence from wisdom literature, instruction texts, and legal records: if laborers'' conduct is addressed as morally significant and remedies (compensation, removal of exploiters) acknowledge their standing, agency was real; if accountability flows only downward and laborers have no recourse, agency was nominal.',
    'If powerless stations were genuine moral agents in practice, the constraint is Rope for all seats. If their agency was rhetorical while their exit was trapped, the constraint is Snare for powerless seats despite appearing as Rope from higher seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(powerless_station_moral_agency, conceptual, 'Whether the principle extended genuine moral agency to powerless stations or was a one-directional accountability structure.').

omega_variable(
    priesthood_independence_vs_subordination,
    'Did the priesthood maintain genuine interpretive independence in assessing cosmic state and Pharaonic conduct, or was priesthood authority ultimately delegated from the Pharaoh and revocable at will?',
    'Recorded instances of priesthood challenging Pharaonic policy or judgment; evidence of priesthood property, resources, and organizational continuity across Pharaonic reigns; whether priesthood authority persisted when individual Pharaohs were weak or absent.',
    'Real priesthood independence strengthens the distributed reading and makes priesthood seats genuine Rope beneficiaries. Subordination makes priesthood authority purely delegated, converting them to conditional payers rather than beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priesthood_independence_vs_subordination, empirical, 'Whether the priesthood''s role in distributed Ma''at maintenance was genuinely independent or ultimately subordinate to the Pharaoh.').

omega_variable(
    reading_committer_identity,
    'Is this ''distributed maintenance'' reading a genuine doctrine held by ancient Egyptian theologians and scribes, or is it a modern analytical imposition that reads historical evidence through a contemporary egalitarian lens?',
    'Examination of Middle and New Kingdom wisdom literature, scribal instruction texts, and temple inscriptions for explicit statements of distributed maintenance as doctrine; assessment of whether ancient sources use language consistent with distributed agency or whether modern scholarship is extrapolating from practice.',
    'If ancient sources explicitly formulate the doctrine, this reading is a faithful constraint story. If the doctrine is a modern synthesis from scattered practices, this story is a committer-side frame that may not have been a lived constraint among historical actors—the constraint would be better modeled as what actors actually claimed (divine mandate or reciprocity) rather than what modern analysis infers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_identity, conceptual, 'Whether the distributed reading is an authentic ancient Egyptian doctrine or a modern analytical reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(maat_tr_t5, maat_order_principle__distributed_maintenance_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__distributed_maintenance_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(maat_tr_t15, maat_order_principle__distributed_maintenance_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__distributed_maintenance_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maat_be_t5, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(maat_be_t15, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(maat_su_t5, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(maat_su_t15, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 30, 0.17).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.12).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% The Ma'at order principle decomposes into three structurally distinct constraint stories, each instantiating a different reading of the contested kernel 'maat_order_principle'. The distributed_maintenance_reading (this story) grounds authority in demonstrated maintenance performance and distributes accountability across all stations. The divine_mandate_reading (sibling) grounds authority in the Pharaoh's inherent cosmic identity and concentrates accountability upward. The reciprocity_reading (sibling) frames maintenance as a contractual exchange (Pharaoh provides justice, subjects provide obedience) and derives authority from bilateral obligation rather than either performance or status. All three readings coexist in the ancient sources and have been held by different communities of interpretation. Each reading instantiates different beneficiary/victim structures, different types, and different extraction metrics: the distributed reading is lowest-extraction Rope; the divine_mandate reading is high-extraction Mountain (claims naturality) or false-summit candidate (if divine mandate benefits identifiable parties); the reciprocity reading is Tangled Rope (coordination of mutual obligation plus asymmetric enforcement). Network edges link all three; each affects the others' plausibility and institutional position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
