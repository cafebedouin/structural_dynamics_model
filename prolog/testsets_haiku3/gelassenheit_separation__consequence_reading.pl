% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation (Consequence Reading): Community Practice Preservation
 *   domain: religious/cultural/technological
 *
 * SUMMARY:
 *   The consequence reading of gelashenheit separation evaluates
 *   technological adoption by its functional impact on core community
 *   practices: in-person visiting (spiritual and social fabric), mutual aid
 *   (economic resilience, relational care), and geographic rootedness
 *   (place-based identity and commitment). Under this reading, a telephone is
 *   permitted in a barn because it enables emergency communication without
 *   eroding daily visiting norms; forbidden in a home because family members
 *   remain indoors rather than visiting neighbors. A tractor is acceptable
 *   for belt-driven power (preserves labor community) but not as road
 *   transport (enables isolation and exit). The constraint is substantially
 *   low-extraction (0.28) because it coordinates around shared practice goals
 *   rather than restricting for restriction's sake; members recognize the
 *   functional logic and enforce it communally rather than through external
 *   coercion. Theater ratio is modest (0.22) because the rules change with
 *   context (barn yes, home no) in ways that require ongoing discernment
 *   rather than mechanical compliance.
 *
 * KEY AGENTS:
 *   - anabaptist_community: Core beneficiary and coordinator — collectively maintains visiting norms and evaluates technology by practice consequence
 *   - community_elders: Governance seat — interprets consequence doctrine and adjudicates boundary cases (does this tool help or harm rootedness)
 *   - younger_generation: Tension seat — experiences the rules as both protective and constraining; pressure point for doctrine evolution
 *   - external_society: Analytical observer — evaluates whether separation is functional practice preservation or identity-performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.28).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.15).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation (Consequence Reading): Community Practice Preservation").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/cultural/technological").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, 'd455a420-cfad-4199-aefd-3b6431700781').
narrative_ontology:cs_kernel_codification('d455a420-cfad-4199-aefd-3b6431700781', fixed_text).
narrative_ontology:cs_authority_grounding('d455a420-cfad-4199-aefd-3b6431700781', lineage).
narrative_ontology:cs_interpretation_layer_present('d455a420-cfad-4199-aefd-3b6431700781').
narrative_ontology:cs_reading_relation('d455a420-cfad-4199-aefd-3b6431700781', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('d455a420-cfad-4199-aefd-3b6431700781', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_axiom('d455a420-cfad-4199-aefd-3b6431700781', foundational, technology_evaluated_by_practice_consequence).
narrative_ontology:cs_axiom_status(technology_evaluated_by_practice_consequence, holdable).
narrative_ontology:cs_axiom_grounding('d455a420-cfad-4199-aefd-3b6431700781', technology_evaluated_by_practice_consequence, instrumental).
narrative_ontology:cs_axiom('d455a420-cfad-4199-aefd-3b6431700781', foundational, community_visiting_essential_to_discipleship).
narrative_ontology:cs_axiom_status(community_visiting_essential_to_discipleship, holdable).
narrative_ontology:cs_axiom_grounding('d455a420-cfad-4199-aefd-3b6431700781', community_visiting_essential_to_discipleship, deontological).
narrative_ontology:cs_reference_frame('d455a420-cfad-4199-aefd-3b6431700781', practice_preservation_through_contextual_discernment).
narrative_ontology:cs_drift_state('d455a420-cfad-4199-aefd-3b6431700781', contemporary_digital_acceleration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d455a420-cfad-4199-aefd-3b6431700781', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, anabaptist_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, younger_generation).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, younger_generation).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, community_visiting_as_spiritual_practice).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, geographic_rootedness_as_discipleship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community collectively maintains the consequence reading of separation: evaluating technology by its effect on visiting, mutual aid, and geographic rootedness. Elders interpret doctrine; members enforce it through social expectation and occasional discipline. They benefit from the constraint because it stabilizes the practices they believe are essential to discipleship and community survival. Exit is identity-locked because leaving the community means leaving an entire worldview and relational matrix; the cost is existential as much as economic.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, anabaptist_community, beneficiary,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, anabaptist_community, agenda_setter).

% Born into the community; experience the separation rules as both protective (rootedness, visiting bonds) and constraining (limited career options, communication lag, perceived backwardness relative to secular peers). They pay costs in foregone technology adoption and reduced mobility. They benefit from community resilience and clear ethical framework. Exit requires leaving the community, which is experienced as losing family and identity.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, younger_generation, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, younger_generation, beneficiary).

% Responsible for interpreting and adjudicating the separation doctrine. They decide boundary cases: does this tool help visiting (barn telephone, permitted) or harm it (home smartphone, forbidden)? They enforce through teaching, example, and community discipline. They perceive the constraint as essential to community survival and cultural continuity. Exit is not available; they carry the doctrine across generations.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, community_elders, agenda_setter,
    powerful, generational, trapped, local).

% Observes the community as an artifact or curiosity; designs technology without regard for its community-practice consequences. From their standpoint, the separation rules are irrational obstacles to development. They have no voice in the community's doctrine but their technological acceleration creates increasing pressure on the younger generation. They are excluded by the community's boundary maintenance and by their fundamental disagreement about technology's purpose.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, external_society, excluded,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, anabaptist_community).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of technology-driven community erosion: coordinates members around shared norms that preserve visiting, mutual aid, and geographic rootedness by contextually evaluating which technologies threaten these practices (home telephone/smartphone yes, barn telephone no; road tractor threatens, belt-drive tractor permits). Without coordination, each member would optimize individually for mobility and convenience, and the community's social cohesion would degrade.
% TRANSFER_FUNCTION: Moves authority over technology evaluation from individual preference to collective discernment. Individual members transfer decision-making power to the community (elders, collective interpretation) in exchange for shared practice stability and collective identity. The constraint also moves status: those who adhere receive community respect and spiritual legitimacy; those who resist or leave experience social cost.
% ABSENT_VOICES: Younger members who wish to adopt secular technology are structurally present but have limited voice in doctrine interpretation — the constraint's legitimacy rests on elder authority, not consensus. Secular professionals (engineers, educators, entrepreneurs) who could demonstrate technology's neutral or positive impact on community practice are excluded by the community's boundary maintenance. Technology designers and manufacturers have no seat at all — the constraint evaluates their products as threats by default.
% DISAPPEARANCE_RATIONALE: If the consequence reading of separation disappeared overnight, the community's technology practices would reorganize toward individual consumption (smartphones in homes, electric vehicles for personal transport, internet-connected businesses). Visiting frequency would shift (members would communicate remotely rather than face-to-face); mutual aid networks would thin as members become geographically mobile; the community's identity would dissolve into American individualism. The arrangement is load-bearing for community cohesion.
% FOUNDING_PROBLEM: Technology's trajectory toward individual convenience and distance communication threatens the Anabaptist community's core practices: mutual aid depends on proximity and relational interdependence; visiting (spiritual practice, social maintenance) requires in-person presence; geographic rootedness is the marker of discipleship commitment. Early telephone adoption showed this threat (farmers using phones to check markets rather than visiting neighbors); mid-20th century car culture accelerated it (members leaving for jobs, commuting across distances). The doctrine developed to preserve practices against this erosion.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary community studies (ethnographic fieldwork by anthropologists studying Anabaptist communities, published in academic journals outside the community) document that communities with stricter technology boundaries show higher visiting frequency and stronger mutual-aid networks than communities with looser boundaries. Secular researchers (Cynthia Brouwer, David Kline, others without community membership) have published data showing the correlation. Community members themselves attest to the pressure: younger members express tension between technology access and community participation; elders report seeing visiting frequency decline in communities that relax technology rules.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint solves a genuine coordination problem: without norms against distance-enabling technology, community members disperse and mutual aid networks degrade. Suppression is low (0.15) because the constraint operates through shared understanding (consequences for visiting) rather than punishment — members internalize the logic. Theater is modest (0.22) because while the rules are genuinely motivated, some enforcement activity is ceremonial: the barn-telephone rule creates visible markers (technology OUT of home space) that may serve identity-signaling as much as functional preservation. The measurement series shows slow drift upward, indicating gradual rule clarification and tightening as technology boundaries become more contested and the community develops case law around smartphones, electric vehicles, and internet access. The drift is gentle (extractiveness +0.08 over 40 years) because the core functional logic remains stable; sharper drift would suggest axiom_overriding (practice preservation ceasing to be achievable through technology rules).
 *
 * PERSPECTIVAL GAP:
 *   The community (beneficiary seat) perceives the constraint as enabling visiting, mutual aid, and rootedness — the rules exist to preserve what the community values. External observers and younger members perceive it as restricting individual autonomy and modernization. Neither perception is false; they stem from different reference frames. The engine computes this divergence from the structural data (beneficiary role for anabaptist_community, moderate power, identity_locked exit for most members) — the consequence reading is authored from the community's framing (the constraint preserves practices they value) and the metrics reflect low extraction (the constraint is not parasitic on the community for external rents), but the accessibility_collapse (0.62) indicates that exit is genuinely costly once inside the interpretive frame.
 *
 * DIRECTIONALITY LOGIC:
 *   The anabaptist community is the sole beneficiary: they articulate the problem (visiting is essential; technology threatens it), propose the solution (contextual technology governance), and enforce it communally. Directionality for the community is near-beneficiary (d ≈ 0.2): they pay modest costs in mobility and communication lag but gain coordination benefits they endorse. No named victims — the constraint is not extractive from external parties. Younger members and those with different technology preferences experience it as costly (identity_locked exit), which would make d ≈ 0.6–0.7 for them if they were separately named; the schema treatment bundles them as part of the community, so the community's net d reflects the internal tension.
 *
 * MANDATROPHY ANALYSIS:
 *   The consequence reading avoids mandatrophy (constraint persisting after its founding problem dissolves) through its functional framing: the founding problem is 'erosion of community visiting and mutual aid networks through distance-enabling technology.' If that problem remains live (visiting continues to erode in communities that permit smartphones), the constraint's mandate persists. Mandatrophy would arise if communities that enforced separation rules rigorously STILL experienced collapse of visiting (suggesting the rules no longer solve the original problem) — at that point the constraint becomes ceremonial. The measurement series does not yet show this; the gentle upward drift suggests increasing rule clarification as new technologies test the principle, not mandate atrophy. Monitoring point: if visiting frequency DECLINES despite rule enforcement, axiom_overriding has occurred (practice_preservation axiom has lost empirical grounding) and the reading should be reclassified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_consequence_vs_principle,
    'Is separation''s core rationale the CONSEQUENCE for community practices (visiting, mutual aid, rootedness), or the PRINCIPLE of structural non-entanglement?',
    'Genealogical analysis of gelashenheit doctrine (16th-century Anabaptist texts, contemporary community leadership interviews): does the tradition justify rules by their functional impact on community cohesion, or by logical structure of entanglement avoidance?',
    'If consequence-centered, this reading holds; the principle reading forecloses it only if principles override outcomes in the same interpretive tradition. If principle-centered, this reading is demoted to a secondary justification and the principle reading gains primacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_consequence_vs_principle, empirical, 'Whether gelashenheit doctrine is grounded in community outcomes or logical purity.').

omega_variable(
    artifact_vs_consequence_visibility,
    'Do the sibling readings (artifact and principle) produce observable rule divergences from this one, or do they converge?',
    'Comparative rule audit across the three readings: specific case analysis of telephone placement, tractor function, electricity wiring, internet access. Map which rules each reading permits/forbids and whether the sets overlap or split cleanly.',
    'If artifact, principle, and consequence readings converge on the same rules, the distinction is interpretive framing only (coexists_with relation confirmed). If they diverge substantially (e.g., consequence permits telephones for emergency aid, artifact forbids them as worldly mimicry), the readings are structurally distinct constraints (influences or forecloses relation confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(artifact_vs_consequence_visibility, empirical, 'Whether sibling readings instantiate materially different rule sets.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.15) externally enforced (community discipline of rule-breakers) or internalized (members have fused their identity with separation practice)?',
    'Post-departure trajectory study: interview former community members 2–5 years after leaving. Do they maintain separation practices voluntarily (internalized), or do they immediately adopt prohibited technologies (externally suppressed)?',
    'If internalized, effective suppression is higher than the scalar suggests and the constraint operates as deep commitment; if external, suppression is structural coercion and reflects power imbalance (community members experiencing exit as costly identity loss). Affects theta-locked/identity-locked directionality for members.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    consequence_measurement_ambiguity,
    'How is ''preservation of visiting, mutual aid, geographic rootedness'' measured and verified? What counts as attestation?',
    'Community-based measurement: time-use diaries, visiting frequency records, mutual-aid transaction logs, property-ownership stability. Compare consequence reading communities (telephone-in-barn permitted) to principle reading communities (telephone-only-structural-isolation) on these metrics.',
    'If metrics show visiting/mutual aid unharmed by barn telephones, the consequence reading''s core justification is weakened. If metrics show sharp declines where phones enter homes, the reading gains empirical grounding. The measurement becomes the validation surface for the axiom_overriding dynamic (if practices decline despite rule adherence, the practice-preservation axiom is becoming incoherent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequence_measurement_ambiguity, empirical, 'Whether the consequence reading''s functional claim is empirically observable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gela_tr_t0, observed).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__consequence_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(gela_tr_t8, observed).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__consequence_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(gela_tr_t16, observed).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__consequence_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(gela_tr_t24, observed).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__consequence_reading, theater_ratio, 32, 0.24).
narrative_ontology:measurement_basis(gela_tr_t32, projected).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(gela_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(gela_be_t0, observed).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__consequence_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(gela_be_t8, observed).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__consequence_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(gela_be_t16, observed).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__consequence_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement_basis(gela_be_t24, observed).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__consequence_reading, base_extractiveness, 32, 0.29).
narrative_ontology:measurement_basis(gela_be_t32, projected).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement_basis(gela_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(gela_su_t0, observed).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__consequence_reading, suppression_requirement, 8, 0.13).
narrative_ontology:measurement_basis(gela_su_t8, observed).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__consequence_reading, suppression_requirement, 16, 0.14).
narrative_ontology:measurement_basis(gela_su_t16, observed).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__consequence_reading, suppression_requirement, 24, 0.15).
narrative_ontology:measurement_basis(gela_su_t24, observed).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__consequence_reading, suppression_requirement, 32, 0.16).
narrative_ontology:measurement_basis(gela_su_t32, projected).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.17).
narrative_ontology:measurement_basis(gela_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gelashenheit separation kernel (3-constraint family). The principle reading (structural non-entanglement) and artifact reading (visible distinction) are sibling constraints with different ε values, different beneficiary structures, and different rule sets. All three readings share the kernel (the historical Anabaptist doctrine of Gelassenheit) but instantiate different constraints because they evaluate technology by different criteria. Network edges link all three; each story documents its sibling relations in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
