% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Christology: Christ as Created Subordinate
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   In the early fourth century, Arius and a network of Eastern bishops
 *   taught that Christ was created in time, subordinate to the Father, and
 *   did not share identical divine substance (ousia) with the Father. This
 *   reading appeals to scriptural texts emphasizing the Father's supremacy
 *   and the Logos as God's instrument of creation. The Council of Nicaea (325
 *   CE) condemned this teaching and imposed the homoousios (consubstantial)
 *   formula, backed by imperial enforcement. The Arian reading persisted,
 *   however, among distributed Eastern episcopal networks, particularly in
 *   Egypt and Syria, resisting imperial suppression and maintaining
 *   alternative theological authority. The constraint operates as a
 *   tangled_rope: it coordinates a genuine theological position rooted in
 *   scriptural interpretation, episcopal autonomy, and church tradition;
 *   simultaneously, it requires active enforcement (exile, suppression,
 *   property seizure) to suppress the rival pro-Nicene reading and maintain
 *   uniformity. The beneficiaries are the Arian episcopal networks (who
 *   maintain doctrinal authority and distributed power) and the principle of
 *   distributed episcopal authority itself (which the Arian position
 *   defends). The victims are the pro-Nicene bishops (subjected to pressure
 *   to enforce Nicene doctrine) and the imperial enforcement apparatus (which
 *   must continuously suppress Arian networks). This story instantiates ONLY
 *   the Arian reading of the homoousios kernel; sibling readings (pro-Nicene
 *   and semi-Arian) are separate constraint stories with their own ε values,
 *   beneficiary structures, and classification.
 *
 * KEY AGENTS:
 *   - arian_bishops_eastern_network: Distributed organized power holding alternative doctrinal authority; resist imperial enforcement
 *   - pro_nicene_imperial_faction: Institutional power wielding imperial machinery to impose homoousios formula
 *   - pro_nicene_bishops: Powerful but constrained; tasked with enforcing the formula against resistant Eastern networks
 *   - christian_laity_eastern_regions: Powerless but trapped; in Arian-dominant regions, subject to suppression campaigns
 *   - semi_arian_bishops: Excluded from the Arian-Nicene binary; attempt mediation but lack institutional standing
 *   - alexandria_school_theological_tradition: Organized intellectual lineage providing theological alternative to Nicene framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.68).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.72).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Christology: Christ as Created Subordinate").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, '774b843d-9ad8-4908-adad-2e27253ccbab').
narrative_ontology:cs_kernel_codification('774b843d-9ad8-4908-adad-2e27253ccbab', fixed_text).
narrative_ontology:cs_authority_grounding('774b843d-9ad8-4908-adad-2e27253ccbab', distributed).
narrative_ontology:cs_reading_relation('774b843d-9ad8-4908-adad-2e27253ccbab', homoousios_christology__pro_nicene_reading, coexists_with).
narrative_ontology:cs_reading_relation('774b843d-9ad8-4908-adad-2e27253ccbab', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('774b843d-9ad8-4908-adad-2e27253ccbab', foundational, christ_created_subordinate).
narrative_ontology:cs_axiom_status(christ_created_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('774b843d-9ad8-4908-adad-2e27253ccbab', christ_created_subordinate, empirically_contingent).
narrative_ontology:cs_axiom('774b843d-9ad8-4908-adad-2e27253ccbab', foundational, episcopal_authority_distributed).
narrative_ontology:cs_axiom_status(episcopal_authority_distributed, holdable).
narrative_ontology:cs_axiom_grounding('774b843d-9ad8-4908-adad-2e27253ccbab', episcopal_authority_distributed, conventional).
narrative_ontology:cs_reference_frame('774b843d-9ad8-4908-adad-2e27253ccbab', distributed_episcopal_subordinationism).
narrative_ontology:cs_drift_state('774b843d-9ad8-4908-adad-2e27253ccbab', post_nicene_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('774b843d-9ad8-4908-adad-2e27253ccbab', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, eastern_bishops_arian_faction).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, distributed_episcopal_authority).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_bishops).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, imperial_enforcement_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, christian_laity_eastern_regions).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_imperial_faction).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, christian_laity_eastern_regions).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, imperial_enforcement_apparatus).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, scriptural_subordinationism).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, divine_monarchy_principle).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, christ_created_ex_nihilo).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinate theological teaching that Christ is created, begotten in time, and subordinate to the Father — not sharing identical divine substance. They control major episcopal sees in Egypt, Syria, and Asia Minor, and maintain doctrinal authority through councils, letters, and catechesis. They resist imperial pressure to accept the Nicene formula and organize alternative synods to defend the subordinationist reading.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_bishops_eastern_network, agenda_setter,
    organized, generational, constrained, continental).

% The emperor and pro-Nicene bishops leverage imperial enforcement machinery (exile, suppression of ordinations, property seizure) to impose the homoousios formula and erase Arian teaching from official recognition. They collect political legitimacy and doctrinal uniformity as the constraint's benefit. They bear the cost of maintaining constant enforcement against a distributed, non-hierarchical opposition.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_imperial_faction, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, pro_nicene_imperial_faction, payer).

% Accept the Nicene homoousios formula but live under pressure from imperial authorities and rival bishops to enforce it, suppress Arian ordinations, and erase rival teaching. They occupy prominent sees but depend on imperial backing for their authority. Their dissent costs them influence and sees; their compliance costs them ongoing conflict with Arian-leaning populations and clergy.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_bishops, payer,
    powerful, generational, constrained, continental).

% In Egypt and the East, receive Arian teaching as the local norm; many bishops, clergy, and congregations understand Christ as created and subordinate. They benefit from coherent, locally-rooted theology that aligns their experience with their leadership. They pay by being subject to imperial suppression campaigns, exiled clergy, and the instability of contested episcopal succession. Exit from Arian belief means abandoning their religious identity and local community.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, christian_laity_eastern_regions, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, christian_laity_eastern_regions, beneficiary).

% Bears the cost of maintaining enforcement against distributed Arian networks: exile orders, property confiscation, military suppression of synods, surveillance of ordinations. The machinery persists because the constraint requires constant active defense; without it, Arian teaching would re-establish itself in Eastern sees within years.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, imperial_enforcement_apparatus, payer,
    institutional, generational, arbitrage, global).

% Advocate a middle position (homoiousios — Christ is of similar substance) but are excluded from both Nicene and Arian camps. They attempt to broker peace but find themselves trapped between imperial pressure for doctrinal uniformity and substantive theological disagreement with both poles.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, semi_arian_bishops, excluded,
    organized, generational, constrained, continental).

% The Alexandrian theological tradition (Origen, Lucian school lineage) supports subordinationism as internally coherent with Scripture and Logos doctrine. It benefits from having distributed, intellectually independent local leadership and alternative catechetical authority. It pays through suppression campaigns, exile of its major teachers, and institutional pressure to conform.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, alexandria_school_theological_tradition, beneficiary,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_non_agent(homoousios_christology__arian_reading, alexandria_school_theological_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, pro_nicene_imperial_faction).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a distributed, locally-rooted theological teaching that respects the divine monarchy and subordinationism of the Logos — coordinating Eastern episcopal practice, catechesis, and liturgy around a coherent scriptural reading without requiring centralized dogmatic authority.
% TRANSFER_FUNCTION: Moves doctrinal authority from centralized imperial/conciliar formulation to distributed episcopal networks; moves teaching content away from Nicene homoousios toward created-Logos subordinationism; extracts enforcement cost from imperial institutions maintaining suppression.
% ABSENT_VOICES: Semi-Arian bishops are structurally excluded from the Arian-Nicene binary; they would advocate for middle positions but find themselves without institutional standing. Provincial laity, particularly in Egypt and Syria, have theological preferences but no voice in high councils. Origen and other patriotic-era theologians are reinterpreted rather than consulted.
% DISAPPEARANCE_RATIONALE: If Arian teaching and its enforcement machinery vanished overnight, Eastern episcopal sees would reorganize: some would adopt Nicene doctrine (in regions where pro-Nicene bishops already held power), others would revert to subordinationist teaching (in Egypt and Syria where Arian positions dominated). The empire would lose a major source of doctrinal conflict but face fragmentation along existing theological fault lines.
% FOUNDING_PROBLEM: How to express the relationship between the Father and the Logos in a way consistent with Scripture's accounts of the Son's subordination, the Father's absolute monarchy, and the Logos's role as God's instrument of creation.
% FOUNDING_PROBLEM_CORROBORATION: Arian bishops and Eastern theological schools attest the problem is live and their reading solves it faithfully. Pro-Nicene bishops and imperial authorities attest the founding problem is subordinationism itself — a false problem created by misreading Scripture — and the Nicene homoousios formula is the only correct solution. Semi-Arian bishops attest the problem requires compromise. Independent patristic scholarship (Augustine, Jerome, later church historians) and comparative theological analysis show all three readings claim scriptural warrant but differ on hermeneutical method and authority structure.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, contested).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Arian reading shows moderate-to-high extraction (0.68 endpoint) because it requires constant active enforcement to suppress and represents a minority position in its own theological tradition by century's end. Suppression is high and growing (0.42 → 0.72) because imperial enforcement escalates from exile and property seizure to military suppression of synods and ordination controls — the constraint's persistence depends entirely on enforcement. Theater ratio is moderate (0.41 endpoint) and rising (0.28 → 0.41): the constraint begins as genuine theological advocacy but increasingly operates as performance of doctrinal conformity, with enforcement machinery defending suppression more than teaching. Accessibility collapse is moderate (0.62) because alternatives (semi-Arian compromise, local subordinationist practice) remain available in Eastern regions despite suppression; the constraint does not achieve complete foreclosure. Resistance is high (0.71) because distributed episcopal networks mount sustained intellectual and organizational opposition to the homoousios formula, sustaining alternative ordinations and catechesis. The measurements show extraction rising steeply in the first 10-15 years (as imperial enforcement machinery is built up) then stabilizing at 0.68, suggesting the constraint reaches a steady-state enforcement ceiling — suppression cannot increase further without destroying the regional churches it purports to govern. The measurement series are authored on one shared time grid (t ∈ {0, 5, 10, 15, 20, 25, 30}) to enable legitimate temporal analysis. All three metrics are measured at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the Arian bishop's seat, the constraint is a defense of scriptural truth and distributed episcopal authority against imperial doctrinal overreach — they experience it as a genuine coordination problem (how to preserve subordinationist theology against enforced uniformity). From the pro-Nicene imperial seat, the constraint is heresy suppression and doctrinal unity — they experience it as a coordination problem (how to maintain universal Christian doctrine against fragmentation). From the enforced laity's seat, the constraint is a contested succession of authority and suppression — they experience extraction (exile of their clergy, instability of their sees). These perspectival gaps are structural: they arise from the asymmetric power and exit positions, not from disagreement about facts. The engine computes per-seat classification from power + exit + directionality, capturing the gap automatically.
 *
 * DIRECTIONALITY LOGIC:
 *   The Arian bishops hold distributed, organized power and maintain doctrinal authority — their directionality is near the beneficiary end (d ≈ 0.25-0.35): they extract enforcement cost from the imperial machinery while retaining alternative legitimacy. The pro-Nicene imperial faction and bishops hold institutional power but bear the cost of constant enforcement — their directionality is near the target end (d ≈ 0.65-0.75): they must extract conformity through suppression. Eastern laity are identity-locked (their local theology, clergy lineage, and catechetical formation tie them to subordinationism) and trapped (no exit to regions with Arian dominance; exit to pro-Nicene belief costs them their local religious identity) — their directionality is near full target (d ≈ 0.80-0.85). The semi-Arian bishops are excluded and constrained (d ≈ 0.55-0.65). The directionality structure is asymmetric because the stakeholders occupy genuinely different structural positions: Arian bishops maintain alternative authority (beneficiary position); pro-Nicene bishops execute enforcement (target position); laity are identity-locked targets. No override is needed because the structural derivation from beneficiary/victim + exit + identity_locked mapping produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The Arian reading satisfies mandatrophy criteria: it was built to solve a live theological problem (how to express Christ's relationship to the Father in a way consistent with Scripture and divine monarchy), the problem remains contested (different traditions and exegetical schools continue to interpret the same texts differently), yet the arrangement is enforced and suppressed. The constraint avoids pure snare classification because genuine theological coordination exists (the Arian network does solve a real problem for Eastern bishops — how to maintain distributed episcopal authority and scriptural-based teaching against imperial universalism). It avoids pure rope classification because active enforcement is required to maintain the constraint against rival authority — the suppression is not theatrical overhead for coordination but essential for persistence. The tangled_rope classification captures this: real coordination (theological teaching, distributed episcopal practice) coupled with asymmetric extraction (enforcement cost on pro-Nicene seats, identity-lock cost on Eastern laity). Mandatrophy is resolved by recognizing the constraint as an ecclesiastical extension conflict: the founding theological problem (Christ's metaphysical relationship to the Father) is live, but the constraint's persistence depends on suppressing a competing reading rather than solving the problem itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_warrant_interpretation,
    'Which hermeneutical framework (subordinationist, consubstantial, or homoiousios) most faithfully represents the scriptural testimony on the Father-Son relationship?',
    'Comparative exegesis of disputed texts (John 14:28 ''the Father is greater than I'', 1 Corinthians 15:28 ''the Son also himself shall be subject'', Colossians 1:15 ''the image of the invisible God'' vs. John 1:1 ''the Word was God'') conducted by scholars independent of doctrinal allegiance and imperial faction.',
    'Arian exegesis claims scriptural supremacy; if independent analysis supports their reading, the constraint shifts from imperial imposition to legitimate theological interpretation. If pro-Nicene reading is shown more faithful, Arian teaching becomes doctrinally unsustainable and the constraint collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_warrant_interpretation, conceptual, 'Competing hermeneutical claims on scriptural fidelity across the kernel readings.').

omega_variable(
    divine_nature_metaphysics_boundary,
    'Is the nature of divine substance (ousia) a coherent metaphysical category that can be measured and predicated (identical vs. similar vs. created), or is the entire ousia framework a philosophical imposition on mysteries that exceed rational categories?',
    'Systematic comparison of the metaphysical commitments of each reading with their theological consequences; examination of whether the ousia framework itself (borrowed from Greek philosophy) is compatible with Christian theological principles, or whether all three readings are corruptions of a non-metaphysical apostolic tradition.',
    'If ousia-thinking is itself the error, the entire constraint (which turns on ousia definitions) could be delegitimized as a false problem. If ousia is legitimate, the dispute becomes which ousia-claim is true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_nature_metaphysics_boundary, conceptual, 'Whether the metaphysical framework (ousia) that grounds all three readings is itself theologically coherent or a philosophical distortion.').

omega_variable(
    imperial_coercion_impact_on_consensus,
    'To what extent does imperial enforcement machinery (exile, suppression, property seizure) constitute false consensus, vs. legitimately enforcing true doctrine against heresy?',
    'Comparative analysis of voluntary adherence rates in pro-Nicene and Arian regions (controlling for enforcement intensity); examination of whether doctrinal shifts correlate with theological persuasion or enforcement escalation; post-suppression stability analysis (if enforcement were withdrawn, which reading would persist).',
    'High coercion + low voluntary adherence suggests the constraint is enforcement-sustained extraction, not genuine coordination. Low coercion + high voluntary adherence suggests the formula reflects real consensus. This directly affects classification: an enforcement-sustained minority reading is a snare or tangled_rope; a genuinely held majority reading is a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_coercion_impact_on_consensus, empirical, 'Whether the Arian reading''s persistence or decline is driven by theology or coercion.').

omega_variable(
    reading_kernel_ambiguity,
    'Is this constraint a single kernel with three competing readings (kernel_id: homoousios_christology) or three distinct theological commitments (three separate kernels)?',
    'Structural analysis: if the three readings all claim to interpret the same scriptural and patristic tradition, using the same source texts (though differently), they are readings of one kernel. If they rest on fundamentally incommensurable theological principles (e.g., one denies the metaphysical coherence of the problem itself), they are separate kernels.',
    'If three readings: the constraint models one contested kernel. If three kernels: each reading should be a separate constraint story, and the ''sibling readings'' architecture is incoherent. The decision cascades to network linkage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the homoousios dispute is one kernel with multiple readings or three distinct theological commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__arian_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(homo_tr_t0, observed).
narrative_ontology:measurement(homo_tr_t5, homoousios_christology__arian_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(homo_tr_t5, observed).
narrative_ontology:measurement(homo_tr_t10, homoousios_christology__arian_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(homo_tr_t10, observed).
narrative_ontology:measurement(homo_tr_t15, homoousios_christology__arian_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(homo_tr_t15, observed).
narrative_ontology:measurement(homo_tr_t20, homoousios_christology__arian_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(homo_tr_t20, observed).
narrative_ontology:measurement(homo_tr_t25, homoousios_christology__arian_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(homo_tr_t25, observed).
narrative_ontology:measurement(homo_tr_t30, homoousios_christology__arian_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(homo_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__arian_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(homo_be_t0, observed).
narrative_ontology:measurement(homo_be_t5, homoousios_christology__arian_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(homo_be_t5, observed).
narrative_ontology:measurement(homo_be_t10, homoousios_christology__arian_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(homo_be_t10, observed).
narrative_ontology:measurement(homo_be_t15, homoousios_christology__arian_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(homo_be_t15, observed).
narrative_ontology:measurement(homo_be_t20, homoousios_christology__arian_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(homo_be_t20, observed).
narrative_ontology:measurement(homo_be_t25, homoousios_christology__arian_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(homo_be_t25, observed).
narrative_ontology:measurement(homo_be_t30, homoousios_christology__arian_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(homo_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__arian_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(homo_su_t0, observed).
narrative_ontology:measurement(homo_su_t5, homoousios_christology__arian_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(homo_su_t5, observed).
narrative_ontology:measurement(homo_su_t10, homoousios_christology__arian_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(homo_su_t10, observed).
narrative_ontology:measurement(homo_su_t15, homoousios_christology__arian_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(homo_su_t15, observed).
narrative_ontology:measurement(homo_su_t20, homoousios_christology__arian_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(homo_su_t20, observed).
narrative_ontology:measurement(homo_su_t25, homoousios_christology__arian_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(homo_su_t25, observed).
narrative_ontology:measurement(homo_su_t30, homoousios_christology__arian_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(homo_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__arian_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__semi_arian_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three constraint stories, one per reading. Each reading has a distinct ε, beneficiary/victim structure, and authorization mechanism. They are linked by network.affects_constraints to enable contamination analysis. The kernel contest is over the metaphysical and scriptural interpretation of Christ's nature; the ε-invariance principle requires separate stories because each reading produces a different constraint classification (Arian: tangled_rope; pro-Nicene: rope with false-summit FSM risk; semi-Arian: tangled_rope with lower extractiveness). The sibling readings are separate constraints, not alternate observables of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
