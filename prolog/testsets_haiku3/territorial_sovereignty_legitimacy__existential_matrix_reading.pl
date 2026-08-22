% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Territorial Sovereignty as Existential Zero-Sum (Existential Matrix Reading)
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the existential_matrix_reading of the
 *   territorial_sovereignty_legitimacy kernel. Under this reading,
 *   sovereignty legitimacy is NOT grounded in legal recognition, historical
 *   continuity, or demographic self-determination, but in each population's
 *   existential requirement for territorial control as the precondition of
 *   collective survival and identity expression. The reading frames the
 *   territorial conflict as zero-sum: partition frameworks (two-state,
 *   power-sharing, international guarantees) are structurally unstable
 *   because both populations perceive territorial compromise as existential
 *   threat, and legal/diplomatic instruments are epiphenomenal to the
 *   underlying existential competition. Legitimacy accrues to whichever
 *   faction achieves and maintains demographic/military dominance. This
 *   reading is one of three siblings that contest the kernel; the constraint
 *   family (territorial_sovereignty_legitimacy) includes
 *   covenant_continuity_reading and self_determination_reading, each
 *   grounding legitimacy differently and producing different classifications.
 *
 * KEY AGENTS:
 *   - demographic_dominant_faction: The population faction that commands numerical and/or military superiority; sets terms of territorial control, benefits from the existential logic, enforces exclusion.
 *   - dispossessed_or_minority_population: Lacks demographic dominance, faces territorial displacement/restriction, trapped by the existential frame (legal claims rendered ineffective).
 *   - international_legal_frameworks: Theater seat; claim to adjudicate legitimacy via legal/historical argument but operate on epiphenomenal causal layer.
 *   - compromise_negotiators: Excluded by the existential reading's logic (cannot solve zero-sum conflict via territorial partition).
 *   - external_power_stakeholders: Benefit from conflict persistence; have no incentive to resolve.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.92).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.88).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.87).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Territorial Sovereignty as Existential Zero-Sum (Existential Matrix Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e81e86cb-9e9d-4dab-a9b1-d81d4859215c').
narrative_ontology:cs_kernel_codification('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', fixed_text).
narrative_ontology:cs_authority_grounding('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', extraction).
narrative_ontology:cs_interpretation_layer_present('e81e86cb-9e9d-4dab-a9b1-d81d4859215c').
narrative_ontology:cs_reading_relation('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', foundational, existential_security_prior_to_juridical_legitimacy).
narrative_ontology:cs_axiom_status(existential_security_prior_to_juridical_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', existential_security_prior_to_juridical_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', foundational, territorial_compromise_structurally_unstable).
narrative_ontology:cs_axiom_status(territorial_compromise_structurally_unstable, holdable).
narrative_ontology:cs_axiom_grounding('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', territorial_compromise_structurally_unstable, instrumental).
narrative_ontology:cs_reference_frame('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', existential_security_competition_preeminent).
narrative_ontology:cs_drift_state('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', contemporary_statehood_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e81e86cb-9e9d-4dab-a9b1-d81d4859215c', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_dominant_faction).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, dispossessed_or_minority_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, external_power_stakeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The population faction that commands numerical and/or military superiority within the contested territory. Under this reading, legitimacy accrues to whichever group can achieve and maintain demographic/military dominance. They set the terms of territorial control and enforce exclusion through security apparatus and settlement/colonization policies. Their existential narrative frames territorial control as non-negotiable precondition for collective survival; any territorial concession reads as existential threat.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_dominant_faction, agenda_setter,
    organized, civilizational, trapped, national).

% The population faction that lacks demographic dominance and faces territorial displacement or restriction. Under this reading, their legitimacy claims (self-determination, historical presence, legal rights) are epiphenomenal; the existential frame renders these claims powerless against the dominant faction's existential fear and military capacity. They are trapped: territorial exit means dispersal/refugee status; political incorporation under minority status preserves existential vulnerability; armed resistance meets superior force. Their only leverage is international pressure, which the existential reading renders ineffective (legal/diplomatic instruments do not override existential survival logic).
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dispossessed_or_minority_population, payer,
    powerless, civilizational, trapped, national).

% UN partition plans, international law on self-determination and territorial integrity, Geneva Conventions, peace agreements. Under this reading these frameworks are theater: they claim to adjudicate sovereignty legitimacy via legal and historical argument, but the underlying driver (existential fear) renders legal settlement structurally unstable. The frameworks observe and document but cannot resolve because they operate on a different causal layer (juridical) than the actual mechanism (existential competition for territorial security).
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_legal_frameworks, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(territorial_sovereignty_legitimacy__existential_matrix_reading, international_legal_frameworks).

% International mediators, NGOs, peace process architects proposing two-state or power-sharing solutions. Under this reading they are structurally excluded from resolution because their framework (territorial partition, shared governance, legal guarantees) presumes both sides can accept vulnerability and trust institutional frameworks. The existential reading denies this: both sides perceive territorial compromise as existential threat, rendering all negotiated settlements unstable until one faction achieves undisputed dominance.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, compromise_negotiators, excluded,
    institutional, generational, constrained, national).

% Great powers and regional hegemons that leverage territorial conflict for geopolitical positioning, military base access, arms sales, and alliance-building. Under this reading they benefit from the conflict's persistence (leverage over client states) and from the existential framing (which renders compromise frameworks unstable and keeps the conflict hot). They have no incentive to resolve it permanently.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, external_power_stakeholders, beneficiary,
    institutional, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_dominant_faction).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading does NOT frame the constraint as solving a coordination problem. Instead, it frames the constraint as expressing an irreducible competitive logic: each population faction seeks territorial control as an existential precondition, making the relationship inherently zero-sum. What appears as 'coordination' (partition plans, autonomy arrangements, power-sharing) is theater masking the underlying existential competition.
% TRANSFER_FUNCTION: Transfers territorial control (and the security/identity benefits that accrue from control) from one faction to another through military dominance, demographic shifts, and administrative exclusion. The transfer is coercive and unidirectional — the dominant faction consolidates control while the minority faction loses access to territory and the existential security it provides.
% ABSENT_VOICES: Voices advocating the possibility of mutual security guarantees, shared sovereignty, or territorial compromise are structurally excluded by the existential reading itself — the reading redefines these voices as analytically incoherent (you cannot guarantee the other side's vulnerability away). Alternative readings (covenant_continuity_reading, self_determination_reading) would argue for juridical and historical legitimacy respectively; they remain live as competing framings but under this reading are diagnosed as epiphenomenal to the existential driver.
% DISAPPEARANCE_RATIONALE: The existential reading asserts that if this constraint disappeared (i.e., if populations accepted shared territory without existential security guarantee), the world would NOT rearrange — instead, the underlying existential competition would intensify toward violent resolution until one faction achieved dominance. The disappearance would be immediately reversed by the logic it names. Sibling readings would argue the world WOULD rearrange (legal frameworks could stabilize it, or self-determination could be honored). The contest is whether the underlying driver is juridical/historical or existential.
% FOUNDING_PROBLEM: Territorial partition of a shared land between two populations with historical presence, conflicting collective narratives, and incommensurable existential security requirements. The founding problem under this reading is NOT 'how do we adjudicate historical claims or legal rights' but 'how do we partition territory so that neither population faces existential threat.' The existential reading asserts this problem is unsolvable via territorial partition because both populations read minority status within shared territory as existential threat.
% FOUNDING_PROBLEM_CORROBORATION: Military analysts, security studies scholars, and conflict resolution researchers who adopt the existential-security framing attest the problem remains live — the founding problem (how to partition for mutual security) persists because both populations continue to experience territorial insecurity and frame concessions as existential vulnerability. Sibling readings (covenant_continuity, self_determination) would corroborate different founding problems. International legal frameworks and peace mediators attest to the problem's persistence but frame it as juridical/historical (thus diagnosing different causal drivers). No external non-partisan source neutral to all three readings exists by construction — the readings name different causal layers.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.68→0.92 over interval) because the existential frame generates asymmetric outcomes: the dominant faction consolidates territorial control and identity security while the minority population faces territorial loss and existential vulnerability. Suppression is high (0.88) because the dominant faction must actively enforce exclusion and suppress alternatives (partition, power-sharing) that would reduce its existential security. Theater_ratio is moderate (0.48) because legal/diplomatic activity is substantial but functionally decoupled from actual territorial outcomes — treaty negotiations, peace proposals, and international pressure constitute a significant performance economy, but the underlying driver (existential competition) renders them structurally inert. Accessibility_collapse (0.91) is very high: once the existential frame is accepted, alternatives (compromise, legal settlement, shared sovereignty) collapse as strategically incoherent — they read as existential suicide. The measurement series tracks rising extractiveness and theater_ratio over time, consistent with the constraint's deepening: as the dominant faction consolidates territorial control and demographic advantage, the extractive asymmetry increases and the performance economy (UN resolutions, peace agreements, humanitarian frameworks) expands as theater masking the underlying existential outcome. All metrics are authored on one shared time grid (0, 10, 25, 40, 55, 75) so temporal analysis has consistent data.
 *
 * PERSPECTIVAL GAP:
 *   The existential_matrix_reading predicts maximum seat divergence: the dominant faction reads the constraint as legitimate sovereignty (a snare from their seat, extractive but justified by existential necessity); the minority faction reads it as illegitimate dispossession (a snare from their seat, extractive and condemned). International observers and mediators read the constraint as a legal/historical dispute (tangled_rope from their seat: coordination problem + enforced extraction, but solvable via negotiation). The engine computes these per-seat classifications from the structural data (power, exit, beneficiary/victim declarations); the authored claim does not adjudicate the divergence. The existential reading's structural prediction is that all three seat classifications are correct according to their own frames, but the underlying driver (existential fear) ensures the dominant seat's frame prevails through military/demographic outcome, not through juridical legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   The demographic_dominant_faction is the structural beneficiary (d near 0.0): they collect territorial control, security, and identity expression from the constraint's operation. Their power is organized, their time horizon civilizational, their exit options trapped (exit means abandoning the territorial claim entirely). The dispossessed_or_minority_population is the structural target (d near 1.0): they bear territorial loss, exclusion, and existential vulnerability. Their power is powerless, their exit options trapped (territorial exit = refugee status; political incorporation = minority vulnerability). External power stakeholders have moderate beneficiary directionality (d ~0.2-0.3): they benefit from conflict persistence and geopolitical leverage, but their power is less aligned with the conflict's local dynamics. Compromise negotiators have observer/analytical directionality (d = 0.5) — they are symmetrically invested in the outcome but positioned outside the existential competition. The directionality derivation chain runs unoverridden for all seats: beneficiary/victim declarations + power + exit → d values that reflect the asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (territorial partition for mutual security) is diagnosed as LIVE under this reading, but the existential frame renders it unsolvable via legal/diplomatic instruments. The constraint persists not because the founding problem is resolved but because it is irreducible: partition cannot satisfy both factions' existential security requirements simultaneously. The theater_ratio (0.48, rising toward 0.5) indicates that legal/peace-process activity is substantial but decoupled from actual territorial outcomes — the constraint would be classified as piton (atrophied function maintained theatrically) IF the founding problem had died AND external forces were maintaining the performance. Instead, the founding problem is live (both factions actively seek territorial dominance) and the theater is instrumentally functional for the dominant faction (international legitimacy theater reduces external pressure and sanctions). The constraint is snare, not piton: the dominant faction actively extracts and enforces; the performance serves extraction, not inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_juridical_causal_layer,
    'Is territorial competition driven by existential security requirements (the existential_matrix_reading assertion) or by legal/historical claims interpreted through juridical frameworks (the covenant_continuity_reading and self_determination_reading assertion)?',
    'Comparison of negotiating positions over time: if populations accept territorial partition that respects their juridical/historical claims (resolving the legal question), the existential reading is falsified. If populations reject partition regardless of legal settlement (or immediately resume conflict), the existential reading gains support. Longitudinal analysis of peace agreement collapse and renegotiation.',
    'If juridical, legal frameworks and historical settlement can resolve the constraint; if existential, legal frameworks are theater and resolution requires military/demographic outcome favoring one faction. This directly determines whether the constraint is snare (irreducible extractive) or tangled_rope (coordinate + extract).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_vs_juridical_causal_layer, conceptual, 'Whether the constraint''s driver is existential security or juridical legitimacy claim.').

omega_variable(
    existential_security_sufficiency,
    'Can territorial partition ever satisfy both populations'' existential security requirements, or is the existential requirement inherently asymmetrical (one faction''s security requires the other''s insecurity)?',
    'Examination of security requirements stated by each faction: if both accept a partition where both achieve defensible borders and demographic stability, existential security is mutually satisfiable and the reading''s zero-sum claim is challenged. If one faction consistently frames the other''s security as incompatible with its own, the asymmetry is confirmed.',
    'If existential security is mutually satisfiable, partition frameworks become structurally possible and the snare classification weakens (the constraint becomes tangled_rope with a solvable coordination problem). If asymmetrical, the zero-sum claim holds and snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_security_sufficiency, empirical, 'Whether existential security requirements are mutually satisfiable through territorial partition.').

omega_variable(
    sibling_reading_committer_frame,
    'Is this reading (existential_matrix_reading) coherent as a single framework grounded in the same kernel as the sibling readings (covenant_continuity_reading, self_determination_reading), or does it redraw the kernel itself (claiming the legitimacy question is epiphenomenal rather than central)?',
    'Examination of whether all three readings claim to answer the same question (''What grounds legitimate sovereignty over this territory?'') or whether existential_matrix_reading claims the question itself is misdirected. If the latter, the kernel may not be unitary.',
    'If the readings answer the same question from different framings, the constraint family structure holds (three readings of one kernel). If existential_matrix_reading reframes the question entirely, it may be a different kernel (legitimacy_derivation vs. conflict_persistence), which would reorganize the network structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_committer_frame, conceptual, 'Whether the existential_matrix_reading shares a kernel with sibling readings or redefines the legitimacy question.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the dispossessed population''s suppression (0.88 at interval end) primarily structural (lack of military capacity, territorial barriers, legal exclusion) or internalized (identity fusion with the territorial claim, psychological habituation to subordination, generational transmission of trauma)?',
    'Post-exit observation: if the dispossessed population exits the territory and suppression persists (displacement trauma, refugee status internalization), the suppression is partly internalized. If suppression declines sharply upon exit and reconstitution elsewhere, it is primarily structural.',
    'If primarily internalized, the constraint''s effective suppression is higher than the structural measure suggests — the population carries the constraint with them and its persistence is harder to reverse. If structural, remedies targeting territorial redistribution or mobility have higher efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether dispossessed population''s suppression is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(terr_tr_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(terr_tr_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 55, 0.46).
narrative_ontology:measurement(terr_tr_t75, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(terr_be_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(terr_be_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 55, 0.89).
narrative_ontology:measurement(terr_be_t75, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 75, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(terr_su_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(terr_su_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 55, 0.87).
narrative_ontology:measurement(terr_su_t75, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 75, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_sovereignty_legitimacy kernel. The covenant_continuity_reading grounds legitimacy in ancient covenant + continuous presence + modern recognition; the self_determination_reading grounds it in modern self-determination + demographic majority; this reading (existential_matrix_reading) claims legitimacy is epiphenomenal to existential security competition. All three readings share the referent (the territorial conflict) but author different ε values (legitimacy efficacy). The three stories form a constraint family linked via network.affects_constraints. The existential_matrix_reading influences both sibling readings structurally: if existential security is the actual driver, legal/historical arguments (the substance of both siblings) become performative. However, the existential reading does not foreclose the siblings — each remains a live position held by different parties and different analytical traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
