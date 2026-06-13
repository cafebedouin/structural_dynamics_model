% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Reading: Authentic Community Through Patient Participatory Labor Under Divine Blessing
 *   domain: theological/political/social
 *
 * SUMMARY:
 *   The Jerusalem reading instantiates authentic human community renewal
 *   through patient, participatory labor under divine blessing. It emerges
 *   from the post-exilic restoration (539 BCE onward) as a theological and
 *   practical alternative to both Babel (unified technological/linguistic
 *   systems imposing uniformity) and technocratic transcendence (human
 *   optimization without vulnerability). The constraint is CLAIMED as rope
 *   (genuine coordination solving a real rupture problem) and the metrics
 *   describe low-to-moderate extractiveness with minimal suppression—a
 *   participatory structure where the primary work is formation and
 *   persuasion, not coercion. The authored measurements model the gradual
 *   expansion of the community and institutional complexity over eighty years
 *   of restoration, with a slight rise in extractiveness in the mid-period
 *   (as institutional authority solidifies) and a modest decline as the
 *   commonwealth stabilizes and the participatory logic becomes culturally
 *   embedded.
 *
 * KEY AGENTS:
 *   - returning_exiles_marginalized: Bearers of memory and disruption; central to the covenant renewal; powerless but non-negotiable to inclusion.
 *   - temple_prophetic_leadership: Institutional agenda-setters; transmit the covenant reading; insist on slow, inclusive work and resist both external pressure and internal efficiency-cutting.
 *   - local_population_neighbors: Bear costs and receive benefits; their participation is constitutive, not auxiliary.
 *   - persian_imperial_authority: Excluded from covenant logic; constrains what is possible; their indifference permits the restoration.
 *   - prophetic_witness_external: Later observers who read this pattern as a model for authentic renewal; analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.35).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.18).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Reading: Authentic Community Through Patient Participatory Labor Under Divine Blessing").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "theological/political/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '71a03d86-7ff7-484a-a7b1-4f3892790b29').
narrative_ontology:cs_kernel_codification('71a03d86-7ff7-484a-a7b1-4f3892790b29', fixed_text).
narrative_ontology:cs_authority_grounding('71a03d86-7ff7-484a-a7b1-4f3892790b29', lineage).
narrative_ontology:cs_interpretation_layer_present('71a03d86-7ff7-484a-a7b1-4f3892790b29').
narrative_ontology:cs_reading_relation('71a03d86-7ff7-484a-a7b1-4f3892790b29', human_transcendence_pathway__babel_reading, forecloses).
narrative_ontology:cs_reading_relation('71a03d86-7ff7-484a-a7b1-4f3892790b29', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_axiom('71a03d86-7ff7-484a-a7b1-4f3892790b29', foundational, human_transcendence_through_vulnerable_participation).
narrative_ontology:cs_axiom_status(human_transcendence_through_vulnerable_participation, holdable).
narrative_ontology:cs_axiom_grounding('71a03d86-7ff7-484a-a7b1-4f3892790b29', human_transcendence_through_vulnerable_participation, theological).
narrative_ontology:cs_axiom('71a03d86-7ff7-484a-a7b1-4f3892790b29', foundational, covenant_extends_to_marginalized_non_negotiable).
narrative_ontology:cs_axiom_status(covenant_extends_to_marginalized_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('71a03d86-7ff7-484a-a7b1-4f3892790b29', covenant_extends_to_marginalized_non_negotiable, deontological).
narrative_ontology:cs_axiom('71a03d86-7ff7-484a-a7b1-4f3892790b29', secondary, divine_blessing_accompanies_participatory_solidarity).
narrative_ontology:cs_axiom_status(divine_blessing_accompanies_participatory_solidarity, holdable).
narrative_ontology:cs_axiom_grounding('71a03d86-7ff7-484a-a7b1-4f3892790b29', divine_blessing_accompanies_participatory_solidarity, empirically_contingent).
narrative_ontology:cs_reference_frame('71a03d86-7ff7-484a-a7b1-4f3892790b29', authentic_covenant_renewed_through_participatory_restoration).
narrative_ontology:cs_drift_state('71a03d86-7ff7-484a-a7b1-4f3892790b29', institutionalization_of_commonwealth, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('71a03d86-7ff7-484a-a7b1-4f3892790b29', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles_marginalized).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, whole_community_commonwealth).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, future_generations_covenant).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, local_population_neighbors).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, local_population_neighbors).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, human_dignity_through_participation).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, divine_blessing_accompanies_solidarity).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, plurality_as_resource_not_obstacle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The exiles returning from Babylon are the primary bearers of memory and the template for reconstitution. They arrive with nothing, dependent on collective work and restoration. Their inclusion in the rebuilding is non-negotiable theologically—the covenant extends to them first. They carry the knowledge that efficiency schemes (like Babel's unified system) failed them; the alternative they embody is patient solidarity.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles_marginalized, beneficiary,
    powerless, generational, identity_locked, local).

% The priesthood and prophetic voices (Haggai, Zechariah, the Ezra circle) set the theological and practical frame: rebuilding the temple as the physical locus of covenant renewal, insisting on inclusive participation and slow, careful work. They resist both external pressure (Persian authorities, neighboring peoples) and internal pressure to cut corners. Their authority derives from the covenant reading they transmit.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, temple_prophetic_leadership, agenda_setter,
    institutional, generational, identity_locked, regional).

% Those who remained in the land during exile or arrived afterward. They bear the cost of the slow rebuilding: labor shared, resources contributed, land negotiations, vulnerability to economic pressure from neighboring states. They also benefit from the renewed commonwealth and the restoration of a stable, morally grounded social order, though that benefit is delayed and diffuse.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, local_population_neighbors, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, local_population_neighbors, beneficiary).

% The imperial order permits but does not actively fund the restoration. Their interest is minimal disruption and tribute-paying stability. They are excluded from the covenant logic and the participatory labor frame, but their power constrains what is possible—the rebuilding happens only within the empire's tolerance.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, persian_imperial_authority, excluded,
    institutional, biographical, trapped, continental).

% Later theological communities, reformist movements, and contemporary observers who read this pattern as a model for authentic renewal: the witness to how divine blessing accompanies patient, participatory, plural work. They have no direct stake but measure their own societies against the Jerusalem pattern.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, prophetic_witness_external, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rebuild a commonwealth and restore the temple through participatory labor, incorporating the returned exiles and the resident population into shared responsibility for a morally renewed social order. The coordination problem solved: how to restore legitimacy, social trust, and institutional function after catastrophic rupture (exile), without either abandoning the marginalized (the exiles) or imposing top-down efficiency (which Babel exemplifies as failed model).
% TRANSFER_FUNCTION: Moves labor, resources, and decision-making authority from centralized, rapid-execution schemes toward distributed, slow, inclusive processes. What transfers: from efficiency-prioritized hierarchy to participation-prioritized solidarity. To whom: the benefits accrue to the community as whole, with special weight on the exiles' restored dignity and the covenant's extension. From whom: the costs are borne by all participants through the sacrifice of speed and efficiency for the sake of inclusive deliberation.
% ABSENT_VOICES: The Samaritan population (excluded from the rebuilding, though resident) would argue for inclusion in the temple reconstruction and the covenant framework, but are kept out by the Jerusalem-centered reading of authenticity. Pragmatists within the community would argue for faster execution and lighter theological weight, but the prophetic frame marginalizes them. Economic beneficiaries of the pre-exile order (now disrupted) would resist the leveling implications of the participatory model.
% DISAPPEARANCE_RATIONALE: If the Jerusalem reading vanished—if the restored community reverted to centralized, rapid-efficiency models (like the Persian imperial logic or the pre-exilic monarchy)—the exiles would lose the framework of restored dignity, the community would fragment along class/origin lines, and the theological claim that divine blessing follows participation in solidarity would dissolve. A world without this constraint would reorganize around transactional authority and efficiency, not covenantal participation.
% FOUNDING_PROBLEM: How to rebuild authentic human community and restore covenant relationship after exile (cultural death, diaspora, loss of shared institutions and place). The specific theological problem: can a people return from exile not as subjects of a dominating system but as participants in a divinely blessed commonwealth that includes the most marginalized among them?
% FOUNDING_PROBLEM_CORROBORATION: The books of Haggai, Zechariah, Nehemiah, and Ezra document the prophetic insistence that the problem (exile, broken covenant, marginalization of returning exiles) is live and requires the participatory model as the only authentic solution. Later Jewish tradition, Christian theology (especially post-Christendom renewal movements), and modern political theology cite this pattern as corroboration. Opposition to the corroboration comes from pragmatists who argue the founding problem has been solved by other means (imperial stability, hybrid structures, efficiency-first governance).
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.35 at end) because the constraint operates through formation and shared conviction, not coercion. The beneficiaries are the community as whole (especially the exiles) and the future generations under renewed covenant; there is no concentrated class extracting surplus. Suppression is minimal (0.18) because the constraint's persistence depends on theological persuasion and participatory commitment, not on external enforcement. The slow rise in extractiveness (0.18→0.35) reflects institutional consolidation and the gradual professionalization of prophetic authority—the more the restoration becomes a stable commonwealth, the more institutional gatekeeping emerges. Theater is very low (0.12) because the participatory labor and temple reconstruction are functionally necessary, not performative. The slight rise and recovery (0.08→0.13→0.12) reflects moments when prophetic authority leans more heavily on rhetorical legitimacy than functional necessity (the gap between 'this is essential work' and 'this work proves the covenant's validity'). The measurements are authored on one shared time grid aligned to the interval [0,80] representing the post-exilic century of restoration.
 *
 * PERSPECTIVAL GAP:
 *   The returning exiles and the local population should compute the constraint differently from the prophetic leadership. For the exiles, the constraint is salvation and restoration (high beneficiary position, low extractiveness felt). For the local population, it is a call to solidarity with costs (moderate payer position, moderate extraction felt). For the prophetic leadership, it is a responsibility to transmit and protect the covenant frame (agenda-setter position, minimal extraction but high burden of maintaining persuasion). The engine computes these divergences from the structural data; the authored claim (rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are named as returning_exiles_marginalized, whole_community_commonwealth, and future_generations_covenant. These are not individual seats but structural positions in the covenant. No directionality overrides are needed because the derivation from beneficiary/victim + exit options produces accurate directionality values: exiles benefit + identity-locked = near-beneficiary; community as whole benefits but distributed = near-symmetric; no declared victims means no high-d targets (this is structurally a rope, not a snare or tangled_rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to rebuild authentic community after exile, including the marginalized) is live and contested. The constraint persists because the theological claim of divine blessing accompanies participation in solidarity is held as true by the covenant community. The risk of mandatrophy: if the commonwealth becomes stable and prosperous through other means (imperial support, economic optimization, assimilation), the founding problem fades from view and the participatory discipline becomes vestigial—the constraint transforms into a piton maintained for theological nostalgia rather than functional necessity. The early rise in extractiveness (0.18→0.35) models the gradual institutional professionalization that carries this risk. The constraint does not resolve mandatrophy in the data; it remains contested whether the participatory labor is constitutively necessary or increasingly ceremonial as the commonwealth stabilizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    participatory_efficiency_tradeoff,
    'Is slow, participatory rebuilding structurally necessary for authentic community, or is it a contingent choice that trades efficiency for solidarity?',
    'Historical counterfactuals (what if the Persian authorities had required rapid reconstruction?) and comparative analysis of communities that rebuilt after rupture through efficiency-first vs. participation-first models.',
    'If slow participation is contingent, the constraint''s extractiveness would rise—it would become a chosen sacrifice of efficiency, not a functional necessity. If it is necessary, the low extractiveness is accurate because the participation solves a real coordination problem (restoring trust after exile).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participatory_efficiency_tradeoff, empirical, 'Whether participatory labor is functionally necessary or an aesthetic choice.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the human_transcendence_pathway kernel is structurally true: Babel (unified systems), Jerusalem (participatory covenant), or Technocratic-Incarnational (optimization vs. grace)?',
    'Empirical assessment of outcomes: which reading''s predictions about authentic renewal, social stability, and long-term community cohesion are borne out in historical and contemporary cases? Which reading correctly identifies the founding problem?',
    'The type classification, beneficiary structure, and extracted epsilon depend on which reading is accepted. Under Babel, the Jerusalem approach is inefficient extraction. Under Jerusalem, Babel is false transcendence. Under Technocratic-Incarnational, both Babel and Jerusalem are misunderstandings of human transcendence itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The core committer-axis contest: which kernel reading captures authentic human transcendence.').

omega_variable(
    divine_blessing_claim_empiricism,
    'Is the claim that ''divine blessing accompanies participatory solidarity'' empirically testable, or is it a non-falsifiable framework claim?',
    'Compare outcomes (social stability, community durability, member flourishing, covenant compliance) of communities organized around the Jerusalem reading vs. other renewal models, controlling for confounders. If outcomes match predictions, the claim gains credibility; if not, it must be reframed as framework-dependent rather than empirical.',
    'If empirical, the constraint''s natural-law properties strengthen (the divine blessing is a genuine feature of reality, not a constructed narrative). If framework-dependent, the constraint remains a coordination choice, and the type classification shifts from rope toward tangled_rope (the framework is persuasive work, not discovery).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_blessing_claim_empiricism, empirical, 'Whether divine blessing is empirically detectable or framework-constituted.').

omega_variable(
    marginalization_inclusion_asymmetry,
    'Does the Jerusalem reading''s insistence on including the marginalized (returning exiles) extract a cost from the non-marginal (local population), creating hidden victims?',
    'Detailed historical and sociological analysis of the post-exilic restoration: did the inclusion of exiles require sacrifice from the resident population beyond fair share of coordination costs? Were there distributional asymmetries hidden by the collective covenant framing?',
    'If marginal inclusion does extract from the non-marginal, the constraint transitions from rope (all benefit from renewed commonwealth) to tangled_rope (coordination + asymmetric extraction). If no hidden extraction, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalization_inclusion_asymmetry, empirical, 'Whether participatory inclusion of exiles contains hidden extraction from resident population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t10, human_transcendence_pathway__jerusalem_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(huma_tr_t10, observed).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement_basis(huma_tr_t40, observed).
narrative_ontology:measurement(huma_tr_t60, human_transcendence_pathway__jerusalem_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement_basis(huma_tr_t60, observed).
narrative_ontology:measurement(huma_tr_t80, human_transcendence_pathway__jerusalem_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement_basis(huma_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(huma_be_t10, observed).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(huma_be_t40, observed).
narrative_ontology:measurement(huma_be_t60, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(huma_be_t60, observed).
narrative_ontology:measurement(huma_be_t80, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement_basis(huma_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t10, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(huma_su_t10, observed).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.17).
narrative_ontology:measurement_basis(huma_su_t40, observed).
narrative_ontology:measurement(huma_su_t60, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 60, 0.19).
narrative_ontology:measurement_basis(huma_su_t60, observed).
narrative_ontology:measurement(huma_su_t80, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement_basis(huma_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__jerusalem_reading, 0.12).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the human_transcendence_pathway kernel. The kernel itself is contested; different readings instantiate structurally distinct constraints with different epsilon values, different beneficiary/victim structures, and different types. The Jerusalem reading (this file) emphasizes participatory solidarity and divine blessing; the Babel reading emphasizes unified technological systems and self-sufficiency; the Technocratic-vs-Incarnational reading emphasizes the optimization-vs-grace axis as the fundamental distinction. Each reading is authored as a clean, ε-invariant constraint. The three constraints form a family linked by network.affects_constraints; the kernel_context and cs_structure fields document the committer structure (readings, axioms, relations) for each.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
