% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Artifact-Reading Gelassenheit Separation (Visible Distinction)
 *   domain: religious_studies/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the 'artifact reading' of Gelassenheit
 *   separation — the interpretation that separation from the world is
 *   constituted by visible distinction in artifacts (clothing, tools,
 *   technology, architecture). Under this reading, a technology is forbidden
 *   if it resembles English artifacts, regardless of its function or
 *   isolation from worldly systems. Solar panels are forbidden because they
 *   look like modern roofs; polyester is forbidden because it looks like
 *   English clothing; battery-powered tools are forbidden because they
 *   resemble power tools. The constraint operates through the Ordnung
 *   (unwritten and written church rules) enforced by the ministry via
 *   twice-yearly council and the sanction of Meidung. The claimed type is
 *   tangled_rope: there is a genuine coordination function (visible boundary
 *   maintenance) but it extracts heavily from members who bear the cost of
 *   forgoing functionally useful, isolated technologies, and it requires
 *   active enforcement. The high extractiveness (0.88) and suppression (0.92)
 *   reflect the maximalist nature of this reading — it leaves almost no room
 *   for functional exceptions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.88).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.92).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Artifact-Reading Gelassenheit Separation (Visible Distinction)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious_studies/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, 'c86aab92-bcb5-42b0-bbe4-3a6717eb9e16').
narrative_ontology:cs_kernel_codification('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', formalized).
narrative_ontology:cs_authority_grounding('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', lineage).
narrative_ontology:cs_interpretation_layer_present('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16').
narrative_ontology:cs_reading_relation('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', gelassenheit_separation__principle_reading, forecloses).
narrative_ontology:cs_reading_relation('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', foundational, visible_distinction_is_separation_itself).
narrative_ontology:cs_axiom_status(visible_distinction_is_separation_itself, holdable).
narrative_ontology:cs_axiom_grounding('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', visible_distinction_is_separation_itself, theological).
narrative_ontology:cs_axiom('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', foundational, artifactual_resemblance_trumps_functional_isolation).
narrative_ontology:cs_axiom_status(artifactual_resemblance_trumps_functional_isolation, holdable).
narrative_ontology:cs_axiom_grounding('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', artifactual_resemblance_trumps_functional_isolation, theological).
narrative_ontology:cs_reference_frame('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', founding_ordnung_visible_markers).
narrative_ontology:cs_drift_state('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', contemporary_technology_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c86aab92-bcb5-42b0-bbe4-3a6717eb9e16', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, amish_ministry).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, conservative_bishops).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, amish_lay_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, amish_youth_seeking_modern_conveniences).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, amish_farmers_needing_offgrid_energy).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, visible_separation_is_gelassenheit_itself).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, artifactual_resemblance_equals_worldly_entanglement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ministry (bishops, ministers, deacons) interprets and enforces the Ordnung. They rule on technology petitions, conduct twice-yearly council meetings, and impose Meidung (shunning) on members who violate artifact prohibitions. Their authority derives from ordination lineage and the claim to preserve the founding vision. They do not personally profit materially but their institutional position depends on maintaining the visible boundary.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, amish_ministry, agenda_setter,
    institutional, generational, analytical, regional).

% Bishops of the most conservative affiliations (Swartzentruber, Nebraska Amish) gain status and authority by holding the strictest visible-distinction line. Their congregations look to them as the standard of faithfulness. A bishop who permits solar panels or polyester loses standing among peers and may face pressure from more conservative districts. Their identity is fused with the artifact boundary.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, conservative_bishops, beneficiary,
    powerful, biographical, identity_locked, local).

% Adult baptized members bear the daily cost of artifact prohibitions: no solar panels for off-grid electricity (relying on diesel generators or wind), no modern fabrics (wearing only plain cotton/wool), no power tools that resemble worldly models. They accept this as the cost of church membership and salvation. Exit means losing family, community, economic network, and (in their framework) eternal security. The identity lock is religious-existential, not merely social.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, amish_lay_members, payer,
    organized, biographical, identity_locked, local).

% Youth during Rumspringa and young adults before baptism experience the artifact prohibitions as arbitrary and costly. They see peers in other affiliations using solar, phones, or modern fabrics without apparent spiritual harm. Their voice is not heard in ministry councils; they must either submit at baptism or leave. Leaving means severing all primary relationships. Some delay baptism indefinitely, living in a liminal state.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, amish_youth_seeking_modern_conveniences, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, amish_youth_seeking_modern_conveniences, excluded).

% Farmers who need reliable electricity for milk cooling, water pumping, or shop tools are forbidden solar panels because panels 'look modern' and 'resemble English roofs.' They must use diesel generators (costly, noisy, fuel-dependent) or hydraulic/pneumatic systems. The functional equivalence of off-grid solar is dismissed; only the visual marker matters. Petitions for exception are routinely denied by conservative ministries.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, amish_farmers_needing_offgrid_energy, payer,
    moderate, biographical, constrained, local).

% Non-Amish neighbors, zoning boards, and agricultural regulators observe the artifact prohibitions from outside. They sometimes pressure for exceptions (building codes requiring electric pumps, milk regulations requiring cooling). Their power is external and coercive; the ministry negotiates accommodations only when forced, framing them as 'not a change in principle.'
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, english_neighbors_and_regulators, observer,
    powerful, biographical, arbitrage, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a visibly distinct communal boundary that signals separation from 'English' (non-Amish) society, enabling the community to recognize its own members, regulate interaction with outsiders, and preserve a shared identity rooted in 19th-century rural life. The visible distinction IS the coordination mechanism — it solves the problem of 'who is us' without requiring constant doctrinal examination.
% TRANSFER_FUNCTION: Transfers technological convenience, economic efficiency, and individual autonomy from lay members (especially farmers and youth) to the ministry's authority structure. The ministry retains the power to define what counts as 'worldly appearance'; members surrender the ability to adopt functionally isolated modern tools (solar, synthetic fabrics, battery tools) because those tools visually resemble English artifacts.
% ABSENT_VOICES: Amish members in more progressive affiliations (New Order, Beachy) who use solar panels, phones, and modern fabrics while maintaining community cohesion — they would argue visible distinction is not necessary for separation. Also absent: former members who left primarily over artifact prohibitions; their testimony is dismissed as 'bitterness' or 'lack of faith.'
% DISAPPEARANCE_RATIONALE: If the artifact-reading prohibition vanished overnight, conservative districts would fracture: some congregations would adopt solar and modern fabrics immediately (following progressive affiliations), others would split, and the ministry's authority to define the boundary would collapse. The visible marker system is the primary coordination mechanism for these affiliations; its loss would reorganize Amish society along principle- or consequence-reading lines.
% FOUNDING_PROBLEM: How to maintain a distinct, visible communal identity in a rapidly industrializing America (mid-19th century) when 'English' society was adopting new technologies, clothing, and social forms that blurred the line between church and world. The artifact reading answered: forbid anything that looks like what the English use, regardless of its function, because visual similarity is spiritual contamination.
% FOUNDING_PROBLEM_CORROBORATION: Conservative bishops attest the founding problem is live: 'The world changes faster now; we must hold the line on appearance more strictly.' Progressive Amish leaders (New Order, Beachy) and scholars of Amish studies (Kraybill, Nolt, Hostetler) attest the founding problem is substantially solved or obsolete: visible distinction no longer prevents assimilation; functional isolation and community practice do. Historical records show the artifact prohibitions expanded AFTER the founding generation (e.g., buttons, hooks-and-eyes, fabric types were later additions), suggesting the reading evolved beyond its founding scope.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) because the constraint forbids technologies that would provide genuine functional benefit (off-grid solar for refrigeration, synthetic fabrics for durability, battery tools for efficiency) solely on visual grounds. The cost is borne entirely by lay members, especially farmers and youth. Suppression is maximal (0.92) because the constraint's persistence depends on active enforcement: the ministry must continually rule on new technologies, deny petitions, and impose Meidung on violators. Alternatives (principle reading, consequence reading) are suppressed by framing them as 'compromise' or 'worldliness.' Theater ratio is moderate (0.38) because the visible distinction IS the claimed function — the performance is the substance — but a growing share of enforcement energy goes to policing edge cases (e-bikes, LED lights, solar chargers) where the visual marker is trivial, suggesting some theatricality. Accessibility collapse is near-total (0.89): once a member accepts the artifact reading's premise, no functional argument can reopen a prohibited technology. Resistance is moderate (0.42): there is persistent low-level petitioning and some defection to progressive affiliations, but open resistance is rare due to identity-locked exit.
 *
 * PERSPECTIVAL GAP:
 *   From the ministry's seat, the constraint is a genuine coordination mechanism (Rope-like) that preserves the community's soul; the extraction is the necessary cost of boundary maintenance. From the lay member's seat (especially farmers and youth), the same constraint operates as enforced extraction (Snare-like): they see progressive affiliations thriving with solar and modern fabrics, proving the visual prohibition is not necessary for separation. The engine computes this divergence from the structural data — the claimed tangled_rope type captures the tension but the per-seat effective extraction will differ dramatically.
 *
 * DIRECTIONALITY LOGIC:
 *   The ministry and conservative bishops are structural beneficiaries (d near 0.0-0.15): they control the boundary definition, their authority depends on it, and they do not bear the daily material costs. Lay members, farmers needing off-grid energy, and youth are structural targets (d near 0.85-0.95): they pay the material and autonomy costs, their exit is identity-locked (leaving means damnation in their framework), and they have no voice in the rule-making. English neighbors/regulators are observers with arbitrage exit — they can pressure but not participate. The identity-locked exit for members is religious-existential: the constraint fuses their salvation identity with compliance, making exit cognitively unavailable, not just costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining visible distinction in an industrializing world) is contested: conservative leaders say it's live; progressive leaders and scholars say it's dead or transformed. The constraint persists despite the original conditions (19th-century rural America) being gone — new technologies (solar, LEDs, synthetics) are banned by analogy to old visual markers. This is mandatrophy: the mandate (visible separation) has outlived its original function (distinguishing from 19th-century English society) and now serves primarily to maintain the ministry's interpretive authority. The artifact reading has become a piton candidate for some affiliations (theater rising, function narrowing), but for the most conservative (Swartzentruber) it remains a high-extraction tangled_rope because the coordination function (visible boundary) is still actively pursued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_reading_vs_principle_reading_foreclosure,
    'Does the artifact reading''s core premise (''visual resemblance equals worldly entanglement'') logically foreclose the principle reading''s core premise (''functional isolation permits'') within any single community''s framework, or can a community hold a hybrid position?',
    'Examine historical splits: when a district adopts solar panels (principle reading), does it necessarily abandon the artifact reading entirely, or do some districts maintain artifact prohibitions in other domains (clothing, buggies) while accepting solar? The Nebraska Amish vs. Swiss Amish divergence on phones vs. tractors may illuminate.',
    'If foreclosure is structural, the two readings cannot coexist in one district — the kernel splits the community. If hybrid positions are stable, the kernel admits a spectrum and the foreclosure claim is overstated. This changes whether the engine should model them as mutually exclusive constraint types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_reading_vs_principle_reading_foreclosure, conceptual, 'Whether artifact and principle readings are logically incompatible in a single framework').

omega_variable(
    beneficiary_structure_ministry_vs_laity,
    'Does the ministry genuinely benefit from the artifact prohibition (institutional authority, status), or do they bear net costs (enforcement burden, conflict, defections) that make them net payers rather than beneficiaries?',
    'Compare defection rates and ministry stress in artifact-reading districts vs. principle-reading districts. If ministry in strict districts spends disproportionate time on technology petitions, faces more youth loss, and gains no material benefit, the beneficiary declaration may be misplaced.',
    'If ministry are net payers, the constraint may be a piton (inertial maintenance) rather than tangled_rope (active extraction by beneficiaries). The directionality derivation would shift ministry d toward target end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_ministry_vs_laity, empirical, 'Whether the ministry are net beneficiaries or net payers of the artifact prohibition').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the high suppression (0.92) primarily structural (Meidung, petition denial, bishop authority) or internalized (members believe solar panels are spiritually contaminating, police their own desires)?',
    'Post-exit trajectory study: members who leave for progressive affiliations — do they immediately adopt solar/modern fabrics, or do they retain aversion? If aversion persists, suppression is partially internalized. Also examine youth during Rumspringa: do they desire forbidden technologies or have they internalized the prohibition?',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit. This would increase measured suppression for identity-locked agents in the engine''s computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in artifact-reading enforcement').

omega_variable(
    kernel_framing_ambiguity,
    'Is the kernel ''gelassenheit_separation'' properly framed as a single commitment with three readings, or are these three distinct commitments (separation-as-boundary, separation-as-purity, separation-as-community) that only appear related through the shared label ''Gelassenheit''?',
    'Trace the historical usage of ''Gelassenheit'' in Amish/Anabaptist literature: does it name one concept with contested application, or has it become a homonym for distinct concepts? Compare with Schleitheim Confession (1527) vs. modern Ordnung usage.',
    'If the kernel is a homonym, the three readings are not sibling constraints of one kernel but independent constraints sharing a label. The network.affects_constraints links would be mis-specified. The cs_structure would need to be restructured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel ''gelassenheit_separation'' is a unified commitment or a homonym for distinct commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 1850, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gelassenheit_artifact_tr_t1850, gelassenheit_separation__artifact_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(gelassenheit_artifact_tr_t1890, gelassenheit_separation__artifact_reading, theater_ratio, 1890, 0.18).
narrative_ontology:measurement(gelassenheit_artifact_tr_t1930, gelassenheit_separation__artifact_reading, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(gelassenheit_artifact_tr_t1960, gelassenheit_separation__artifact_reading, theater_ratio, 1960, 0.31).
narrative_ontology:measurement(gelassenheit_artifact_tr_t1990, gelassenheit_separation__artifact_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(gelassenheit_artifact_tr_t2024, gelassenheit_separation__artifact_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(gelassenheit_artifact_be_t1850, gelassenheit_separation__artifact_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(gelassenheit_artifact_be_t1890, gelassenheit_separation__artifact_reading, base_extractiveness, 1890, 0.48).
narrative_ontology:measurement(gelassenheit_artifact_be_t1930, gelassenheit_separation__artifact_reading, base_extractiveness, 1930, 0.62).
narrative_ontology:measurement(gelassenheit_artifact_be_t1960, gelassenheit_separation__artifact_reading, base_extractiveness, 1960, 0.73).
narrative_ontology:measurement(gelassenheit_artifact_be_t1990, gelassenheit_separation__artifact_reading, base_extractiveness, 1990, 0.81).
narrative_ontology:measurement(gelassenheit_artifact_be_t2024, gelassenheit_separation__artifact_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(gelassenheit_artifact_su_t1850, gelassenheit_separation__artifact_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(gelassenheit_artifact_su_t1890, gelassenheit_separation__artifact_reading, suppression_requirement, 1890, 0.58).
narrative_ontology:measurement(gelassenheit_artifact_su_t1930, gelassenheit_separation__artifact_reading, suppression_requirement, 1930, 0.71).
narrative_ontology:measurement(gelassenheit_artifact_su_t1960, gelassenheit_separation__artifact_reading, suppression_requirement, 1960, 0.82).
narrative_ontology:measurement(gelassenheit_artifact_su_t1990, gelassenheit_separation__artifact_reading, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(gelassenheit_artifact_su_t2024, gelassenheit_separation__artifact_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This constraint (artifact_reading) and its two siblings (principle_reading, consequence_reading) form the gelassenheit_separation constraint family. They share the kernel 'separation from the world' but instantiate structurally distinct constraints with different ε values: artifact_reading ε=0.88 (maximal suppression, visual boundary), principle_reading ε≈0.35 (functional isolation permitted), consequence_reading ε≈0.45 (consequentialist evaluation). The artifact_reading forecloses the principle_reading within a single framework; both coexist with consequence_reading across affiliations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, institutional, 0.1).
constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, organized, 0.9).
constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
