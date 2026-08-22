% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Functional Separation Principle (Technology Doctrine)
 *   domain: religious/commitment_systems
 *
 * SUMMARY:
 *   The Gelassenheit principle-reading interprets separation from worldly
 *   systems as an obligation to avoid structural entanglement with market
 *   integration, insurance systems, and information networks — but permits
 *   technologies that operate functionally isolated from those systems. This
 *   reading distinguishes itself from sibling readings (artifact-based
 *   visibility and consequence-based community impact) by grounding
 *   legitimacy in structural relationship rather than appearance or social
 *   effect. Solar equipment on a truly off-grid homestead is acceptable;
 *   internet access is forbidden regardless of technical isolation
 *   possibility because internet systems are architecturally entangled with
 *   global surveillance and market infrastructure. Insurance is forbidden
 *   even when it would be functionally isolated benefit, because insurance
 *   systems embody structural entanglement with worldly economic logic. The
 *   constraint is authored as CLAIMED tangled_rope (genuine separation
 *   function + asymmetric extraction) with moderate extractiveness (lower
 *   than snare, higher than rope) and measurable suppression (the doctrine
 *   must actively prohibit technologies that pass functional isolation
 *   tests). Theater ratio is moderate-low: the interpretation work is genuine
 *   theological scholarship, but an increasing share of enforcement activity
 *   defends the boundary against technologies that the functional-isolation
 *   criterion itself might permit.
 *
 * KEY AGENTS:
 *   - Theological interpretation authority: maintains and adjudicates the principle-based framework; holds interpretive power over structural entanglement; benefits from deference and community coordination around the doctrine.
 *   - Community adopters: navigate the constraint daily; face the cost of foregone technologies even when isolation-based arguments might permit them; identity-locked into the community and unable to exit without surrendering spiritual belonging.
 *   - Boundary-edge members: craftspeople, medically vulnerable individuals, young adults at community margins; bear prohibition costs asymmetrically because exit is costlier than for institutional authority.
 *   - Rival interpretive traditions: excluded by lineage authority; hold alternative readings (artifact-based and consequence-based) that would reshape technology access and community authority.
 *   - Technology manufacturers: excluded from legitimacy adjudication; could offer isolation-compatible products but have no voice in the doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.38).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.52).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Functional Separation Principle (Technology Doctrine)").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '683ef9ea-de39-4567-aa29-ad93a71cbb58').
narrative_ontology:cs_kernel_codification('683ef9ea-de39-4567-aa29-ad93a71cbb58', distributed).
narrative_ontology:cs_authority_grounding('683ef9ea-de39-4567-aa29-ad93a71cbb58', lineage).
narrative_ontology:cs_interpretation_layer_present('683ef9ea-de39-4567-aa29-ad93a71cbb58').
narrative_ontology:cs_reading_relation('683ef9ea-de39-4567-aa29-ad93a71cbb58', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('683ef9ea-de39-4567-aa29-ad93a71cbb58', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('683ef9ea-de39-4567-aa29-ad93a71cbb58', foundational, structural_entanglement_is_primary_separation_concern).
narrative_ontology:cs_axiom_status(structural_entanglement_is_primary_separation_concern, holdable).
narrative_ontology:cs_axiom_grounding('683ef9ea-de39-4567-aa29-ad93a71cbb58', structural_entanglement_is_primary_separation_concern, deontological).
narrative_ontology:cs_axiom('683ef9ea-de39-4567-aa29-ad93a71cbb58', foundational, functional_isolation_permits_worldly_technology_use).
narrative_ontology:cs_axiom_status(functional_isolation_permits_worldly_technology_use, holdable).
narrative_ontology:cs_axiom_grounding('683ef9ea-de39-4567-aa29-ad93a71cbb58', functional_isolation_permits_worldly_technology_use, empirically_contingent).
narrative_ontology:cs_reference_frame('683ef9ea-de39-4567-aa29-ad93a71cbb58', separation_as_structural_independence).
narrative_ontology:cs_drift_state('683ef9ea-de39-4567-aa29-ad93a71cbb58', contemporary_global_entanglement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('683ef9ea-de39-4567-aa29-ad93a71cbb58', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, theological_interpretation_authority).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, community_adopters).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, boundary_edge_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, boundary_edge_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and interprets the principle-based separation doctrine within Anabaptist theological tradition. Adjudicates which technologies conform to functional isolation and which constitute structural entanglement. Collects interpretive authority and community deference; holds the power to reclassify technologies as acceptable or forbidden based on theological analysis.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, theological_interpretation_authority, agenda_setter,
    institutional, generational, identity_locked, regional).

% Members embedded in the community who must navigate the constraint daily: deciding which tools are functionally isolated enough to use, which create structural entanglement. Bear the cost of foregone technologies (no internet, no insurance, limited mechanization) even when isolation-based arguments might permit them. Their behavior is the material where the principle is tested.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, community_adopters, payer,
    moderate, biographical, identity_locked, regional).

% Individuals whose livelihood or social situation sits at the margin: craftspeople who might use solar equipment to stay off-grid, community members with medical needs that insurance would cover, young adults contemplating entry/exit. They bear the prohibition asymmetrically because exit (leaving community, surrendering identity) is costlier than for institutional authority.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, boundary_edge_members, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__principle_reading, boundary_edge_members, beneficiary).

% Other Anabaptist communities and theological schools that read separation differently (artifact-based or consequence-based readings). They are structurally kept out of the doctrine-adjudication process by lineage authority; their alternative framings would challenge the principle-reading but cannot gain institutional footing without fragmenting community authority.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, rival_interpretive_traditions, excluded,
    moderate, generational, constrained, regional).

% Producers of solar equipment, pneumatic tools, internet infrastructure. They are excluded from the conversation about what counts as functionally isolated, though the principle's classifications affect their market. They could offer products claimed as isolation-compatible but have no voice in legitimacy adjudication.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, technology_manufacturers, excluded,
    powerful, generational, mobile, global).

% External analyst examining how the principle-reading's functional-isolation framework operates: what counts as structural entanglement, how the metaphor is applied, where the boundary between isolation and entanglement is drawn and why.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, theological_scholar_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, theological_interpretation_authority).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the theological claim that separation from worldly systems is a moral obligation: permits coordination of community life around tools and technologies that maintain structural independence from market integration, insurance systems, and information networks, while allowing those that operate functionally isolated.
% TRANSFER_FUNCTION: Moves interpretive authority and boundary-maintenance work from the embedded community to the theological interpretation authority. Community members transfer deference and behavioral conformity to the doctrine-setter; the doctrine-setter transfers back theological legitimacy and community coherence.
% ABSENT_VOICES: Rival interpretive traditions (artifact-based and consequence-based readings) are excluded by lineage authority and cannot present their alternative framings to community adjudication. Technology manufacturers and insurance providers are also absent: they could argue that tools like solar systems are isolation-compatible by design, but have no standing in the theological determination.
% DISAPPEARANCE_RATIONALE: If the principle-based separation doctrine disappeared, the community would rapidly adopt technologies under different classification schemes (artifact-based visibility or consequence-based community impact). The social authority structure would reorganize around a different reading; community members' technology access would expand; theological interpretations of separation would fragment across multiple schemas.
% FOUNDING_PROBLEM: Early Anabaptist separation theology distinguished withdrawal from worldly systems as a path to spiritual purity and community integrity. The principle-reading emerged to preserve that theology while accommodating technological change: separation is about structural relationship to market/information/insurance systems, not about artifact visibility or social consequence.
% FOUNDING_PROBLEM_CORROBORATION: The theological interpretation authority attests the problem is live and the principle-reading is the correct solution. Rival interpretive communities and external theological scholars attest the founding problem is either obsolete (modern technologies have eliminated the worldly entanglement the doctrine warned of) or wrongly framed (consequence and artifact readings better capture the actual concerns communities express). Academic historians of Anabaptism document the doctrinal shift from consequence-based to principle-based frameworks in the mid-20th century, providing external corroboration of the interpretive contest.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).
:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the principle-reading does coordinate genuine separation theology (a coordination function exists) while also concentrating interpretive authority and extracting behavioral conformity from the community. The reading permits some technologies (solar, pneumatic tools) that artifact-based and consequence-based readings would forbid, which lowers extraction relative to more restrictive readings. However, the absolute prohibition on internet and insurance regardless of isolation possibility reveals that the functional-isolation criterion is not always applied consistently — suggesting that some prohibitions serve extraction of interpretive authority rather than pursuit of structural isolation. Suppression is moderate (0.52) because exit from the community is costly (identity-locked), but the principle-reading itself is less suppressive than alternatives: it rests on rational argument (functional isolation) rather than on appearance policing or social pressure. Theater ratio is moderate-low (0.28) because the core interpretation work — discerning structural entanglement — is genuine, but enforcement increasingly focuses on boundary defense (blocking solar applications, denying insurance use cases) rather than on the structural principle itself. The measurement series shows slow creep upward in all three metrics: as technologies advance and edge cases multiply, maintaining the principle requires more suppression and more interpretive theater to defend the doctrine against counterexamples. The trajectory reflects the constraint approaching either mandatrophy (the founding problem dissolves, but the doctrine persists) or compression into a piton (mostly performance, diminishing functional role).
 *
 * PERSPECTIVAL GAP:
 *   The theological interpretation authority perceives this constraint as genuine coordination: separation theology that permits rational technology adoption while preventing worldly entanglement. From the payer seats (community adopters, boundary-edge members), the same constraint operates as a authority-concentration mechanism that prohibits technologies (solar, insurance) the functional-isolation principle itself might permit. The prohibitions not explained by the stated principle suggest extraction of deference and behavioral control. The engine computes this divergence from the stakeholder positions and the structural data: the theological authority sits at low directionality (beneficiary end, holds interpretive power), while community adopters sit at high directionality (payer end, constrained by identity-lock). The audit should flag whether the constraint is tangled_rope (genuine coordination + asymmetric extraction, as claimed) or snare (extraction with a theological cover story).
 *
 * DIRECTIONALITY LOGIC:
 *   The theological interpretation authority is the structural beneficiary (d near 0.0-0.2): it sets the agenda, collects deference and behavioral conformity, controls the boundary between acceptable and forbidden technologies, and holds the power to reclassify technologies as the doctrine faces counterexamples. Its exit options are arbitrage-grade: it can reinterpret the principle, shift the boundary, or adopt rival readings without leaving its institutional seat. Community adopters are structural payers (d near 0.7-0.9): they must conform daily, face costs of foregone technologies, and cannot exit without surrendering identity. Their exit options are identity-locked: leaving the community means losing spiritual belonging, family ties, and social location. Boundary-edge members are target-adjacent (d near 0.8-1.0): they bear prohibition costs asymmetrically and have the fewest exit options. Rival interpretive traditions are excluded but would have high d if admitted — they are kept out structurally, not persuaded out. The suppression is moderate because the constraint rests on theological argument (rational, not purely coercive) but the identity-lock is severe (community membership = spiritual identity = exit cost is unbounded). The engine should compute suppression-adjusted d for identity-locked targets: the constraint is less suppressive than if maintained by pure external force, but more suppressive than voluntary coordination because exit means identity dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would arise if the founding problem (worldly entanglement in early industrial/modern technological systems) has substantially been solved or is no longer the actual concern animating community separation practice. Evidence for mandatrophy: (1) the functional-isolation criterion would permit solar and pneumatic technologies, but the doctrine forbids them anyway, suggesting the principle is no longer the decision rule; (2) the doctrine forbids internet and insurance despite isolation possibilities, suggesting structural entanglement is a post-hoc rationalization for other values (artifact visibility, community consequence); (3) community members report that technological access decisions are made by consequence and artifact criteria, not principle-based structural analysis. If mandatrophy is true, the constraint becomes piton-like: the principle persists through theological authority and identity-lock, but its actual function (guiding technology decisions) has been displaced by the sibling readings (artifact and consequence), which are now doing the work. The theater-ratio creep (increasing interpretive defense and boundary policing) is consistent with mandatrophy: the principle must be defended more theatrically as the functional-isolation criterion fails to match actual community technology decisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_isolation_vs_systemic_entanglement,
    'Is functional isolation sufficient to prevent structural entanglement, or do all technologies participate in global systems of extraction and surveillance regardless of their isolated operation?',
    'Examine community members'' actual technology use and decision-making: do they consistently apply the functional-isolation criterion, or do they prohibit technologies that pass the criterion? Compare stated doctrine with practiced enforcement.',
    'If functional isolation is genuinely the decision rule, the constraint is tangled_rope (real coordination function + extraction of interpretive authority). If actual decisions reflect different criteria (artifact, consequence, narrative fit), the constraint is snare (extraction with a principle cover story) or piton (principle obsolete, theater maintains authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_isolation_vs_systemic_entanglement, empirical, 'Whether the functional-isolation principle is the actual decision rule or a post-hoc rationalization.').

omega_variable(
    foundational_problem_status_contested,
    'Is the founding problem (avoiding structural entanglement in worldly systems) still live, dead, or contested as a motivating concern in contemporary Anabaptist communities?',
    'Ethnographic study: interview community members about their reasons for technology prohibitions; compare stated theological reasoning with lived motivations. Examine historical texts showing the principle-reading''s emergence and evolution.',
    'If foundational problem is dead but the doctrine persists, the constraint exhibits mandatrophy: the principle is maintained by authority and identity-lock, not by genuine coordination need. If contested, different seats hold different assessments of whether worldly entanglement is the true concern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_status_contested, empirical, 'Whether the principle-based separation theology still animates actual technology governance.').

omega_variable(
    reading_commensurability_under_globalization,
    'Does the principle-reading''s distinction between functional isolation and structural entanglement remain coherent in 21st-century contexts where all economic activity is globally entangled?',
    'Analyze boundary cases: can solar equipment truly be functionally isolated when its supply chain, financing, and warranty systems are globally entangled? Can any production activity avoid structural participation in market and surveillance systems?',
    'If isolation becomes incoherent (all systems are entangled), the principle-reading forecloses itself and converges toward artifact-based or consequence-based readings. If isolation remains coherent through local economics and community production, the principle sustains its justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commensurability_under_globalization, conceptual, 'Whether functional isolation is a coherent category in globally-entangled systems.').

omega_variable(
    identity_lock_suppression_mechanisms,
    'Is the measured suppression (0.52) adequate to account for identity-lock effects, or is the constraint''s effective suppression substantially higher because exit means identity dissolution?',
    'Compare suppression between community members with high identity-lock (born into community, family ties) and those with lower identity-lock (recent converts, weaker community bonds). Examine exit patterns and post-exit suppression trajectories.',
    'If identity-lock substantially amplifies effective suppression, the constraint is more extractive than the 0.52 metric suggests. The directionality for identity-locked targets should reflect amplified suppression, pushing them closer to full-target (d near 1.0) than moderate suppression alone would indicate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanisms, empirical, 'Whether identity-lock mechanisms create internalized suppression beyond structural barriers.').

omega_variable(
    alternative_reading_coexistence_viability,
    'Can the principle-reading coexist with artifact-based and consequence-based readings within a single community authority structure, or does lineage-authority require doctrinal monopoly?',
    'Examine communities that have institutionalized multiple interpretations; compare conflict rates and authority stability against single-reading communities. Study doctrinal contests in historical texts.',
    'If coexistence is viable, the constraint is less suppressive than currently measured — alternative readings could be openly debated rather than excluded. If lineage authority requires monopoly, the exclusion of rival readings is structural suppression the current measurement may underestimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_coexistence_viability, conceptual, 'Whether commitment-system authority can sustain multiple legitimate readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__principle_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__principle_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__principle_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(gela_tr_t45, gelassenheit_separation__principle_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__principle_reading, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__principle_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__principle_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__principle_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(gela_be_t45, gelassenheit_separation__principle_reading, base_extractiveness, 45, 0.39).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__principle_reading, base_extractiveness, 60, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__principle_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__principle_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__principle_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(gela_su_t45, gelassenheit_separation__principle_reading, suppression_requirement, 45, 0.54).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__principle_reading, suppression_requirement, 60, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested gelassenheit_separation kernel. The principle-reading interprets separation as structural-relationship-based (avoiding entanglement with market, insurance, surveillance systems). Sibling readings (artifact-based and consequence-based) interpret separation as visible-distinction-based and community-practice-based respectively. The three readings share the kernel (a stabilized commitment to separation) but instantiate different constraint structures with different epsilon values, different suppressed alternatives, and different organizational implications. The principle-reading permits technologies the artifact-reading forbids (solar equipment resembles worldly infrastructure but is accepted under functional isolation); forbids technologies the consequence-reading might permit (insurance if community-impact-neutral); and constrains technologies the artifact-reading might accept (decorated tools are forbidden if structurally entangled, permitted if isolated). The three stories are linked via network.affects_constraints to model the doctrinal contest and the structural pressure each reading exerts on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__principle_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
