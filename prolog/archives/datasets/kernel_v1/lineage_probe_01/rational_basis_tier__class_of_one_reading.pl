% ============================================================================
% CONSTRAINT STORY: rational_basis_tier__class_of_one_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rational_basis_class_of_one, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rational_basis_tier__class_of_one_reading
 *   human_readable: Equal Protection as Class-of-One Vendetta Suppression (Rational Basis Tier)
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   United States v. Olech (2000) established that equal protection's
 *   rational basis tier extends to arbitrary singling-out of a single
 *   individual—a 'class of one.' The constraint models the doctrinal
 *   structure that emerges: equal protection as a guarantee against vendetta,
 *   not merely against caste- or group-based discrimination. This reading
 *   locates the suppression mechanism in individualized arbitrary
 *   administrative action and the extractive benefit in the state's ability
 *   to target specific property owners, permit applicants, or benefit seekers
 *   without articulating rational purpose. The Olech reading holds that
 *   rational basis review, properly understood, prevents this—the state
 *   cannot single out one person unless the differentiation is rationally
 *   related to a legitimate governmental purpose. The constraint exhibits
 *   mixed coordination and extraction: rational basis tier coordinates state
 *   legitimacy (preserving wide discretion), but the class-of-one requirement
 *   extracts through asymmetric burden (individual must negate all
 *   conceivable purposes; state articulates none). Extractiveness has
 *   declined since Olech's adoption as courts have become more willing to
 *   examine actual effects and require minimal articulable purpose, reducing
 *   the pure-spite extraction window. Theater remains moderate because
 *   rational basis still permits post-hoc rationalization; true intent-based
 *   review would lower theater further.
 *
 * KEY AGENTS:
 *   - Singled-Out Individual Claimant: Primary victim (powerless/trapped) — faces unexplained adverse action with burden to prove vendetta; local geographic fixedness traps them; carries maximum extraction experience
 *   - Property Owners and Permit Applicants: Secondary victim class (moderate/constrained) — face potential differential treatment; can relocate or abandon but at significant cost; benefit from class-of-one doctrine but remain extraction-constrained
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — retains rational basis tier's broad discretion; benefits from ability to act without articulating purpose; sees class-of-one as minimal constraint on coordinating legitimate action
 *   - Courts Applying Rational Basis: Secondary institutional actor (institutional/constrained) — must apply tier without clear standards for when rational basis is satisfied; class-of-one provides intermediate guidance but creates doctrinal friction
 *   - Rational Basis Tier Doctrine: Authority structure (institutional/arbitrage) — coordinates state legitimacy; the tier itself benefits from preservation of wide discretion while requiring vendetta suppression
 *   - Civil Rights Advocates: Organized agents (organized/constrained) — use class-of-one doctrine as scaffold for longer-term doctrinal pressures (toward animus doctrine or heightened review); constrained by existing tier structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rational_basis_tier__class_of_one_reading, 0.38).
domain_priors:suppression_score(rational_basis_tier__class_of_one_reading, 0.52).
domain_priors:theater_ratio(rational_basis_tier__class_of_one_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rational_basis_tier__class_of_one_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(rational_basis_tier__class_of_one_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rational_basis_tier__class_of_one_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rational_basis_tier__class_of_one_reading, tangled_rope).
narrative_ontology:human_readable(rational_basis_tier__class_of_one_reading, "Equal Protection as Class-of-One Vendetta Suppression (Rational Basis Tier)").
narrative_ontology:topic_domain(rational_basis_tier__class_of_one_reading, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(rational_basis_tier__class_of_one_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rational_basis_tier__class_of_one_reading, 'd03858e1-6a38-4de0-91b0-bfc1bc3d23e4').
narrative_ontology:cs_kernel_codification('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', fixed_text).
narrative_ontology:cs_authority_grounding('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', lineage).
narrative_ontology:cs_interpretation_layer_present('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4').
narrative_ontology:cs_reading_relation('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', rational_basis_tier__animus_with_bite_reading, coexists_with).
narrative_ontology:cs_reading_relation('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', rational_basis_tier__pure_deference_reading, forecloses).
narrative_ontology:cs_axiom('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', foundational, rationality_prevents_individual_vendetta).
narrative_ontology:cs_axiom_status(rationality_prevents_individual_vendetta, holdable).
narrative_ontology:cs_axiom_grounding('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', rationality_prevents_individual_vendetta, deontological).
narrative_ontology:cs_axiom('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', foundational, individual_arbitrariness_actionable_as_equal_protection_violation).
narrative_ontology:cs_axiom_status(individual_arbitrariness_actionable_as_equal_protection_violation, holdable).
narrative_ontology:cs_axiom_grounding('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', individual_arbitrariness_actionable_as_equal_protection_violation, conventional).
narrative_ontology:cs_reference_frame('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', rational_basis_minimal_deference).
narrative_ontology:cs_drift_state('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', post_olech_application_phase, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d03858e1-6a38-4de0-91b0-bfc1bc3d23e4', '2026-02-26T15:32:00Z').
narrative_ontology:cs_kernel_id(rational_basis_tier__class_of_one_reading, rational_basis_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rational_basis_tier__class_of_one_reading, singled_out_individual_claimant).
narrative_ontology:constraint_victim(rational_basis_tier__class_of_one_reading, arbitrary_administrative_action).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SINGLED-OUT INDIVIDUAL (SNARE) — The targeted citizen faces unexplained adverse action (permit denial, benefit withholding, tax treatment, occupancy restriction) with no avenue to contest except by proving negative: that the state acted out of personal spite, not conceivable rational purpose. The burden of proof structure traps the individual. They cannot exit—the harm is site-specific (local land, local permit). Extraction operates through the impossibility of demonstrating motive.
constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CLASS-OF-ONE CLAIMANT COMMUNITY (TANGLED_ROPE) — Property owners, permit applicants, and benefit seekers who suspect animus but lack proof face mixed coordination and extraction. The rational basis tier coordinates legitimate governmental action (no blanket suspect class protections required). But the structure extracts through asymmetric burden: the individual must negate every conceivable purpose while the state bears no burden to articulate actual purpose. Exit is costly but possible—relocation, abandonment, legal challenge—but carries significant loss.
constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RATIONAL BASIS TIER (INSTITUTIONAL ROPE) — The tier itself coordinates governmental legitimacy. States benefit from rational basis: minimal judicial interference, wide policy discretion, enforcement capacity preserved. The tier enables coordination—states can act on any rational purpose without proving burden or necessity. Olech's class-of-one reading preserves this coordination while requiring that individualized action meet a threshold of rationality that excludes pure vendetta. The institution sees this as consistent—rational basis still permits enormous latitude.
constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CLASS-OF-ONE DOCTRINE AS INTERIM REMEDY (SCAFFOLD) — The Olech reading functions as a temporary doctrinal patch: it allows courts to intervene when arbitrariness is flagrant without requiring fundamental tier restructuring or adoption of animus-with-bite across all contexts. It has an implicit sunset: either rational basis evolves to require actual articulation of purpose (the doctrine sunsets into stricter review), or animus doctrine coalesces into a separate tier with its own tools (the remedy becomes specialized). Organized advocates (civil rights groups, state attorneys general demanding clarity) use the scaffold as leverage for longer-term doctrinal change.
constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RATIONAL BASIS AS DEGRADED FORM (PITON) — From a civilizational institutional view, rational basis is a partially broken doctrinal tool. The tier purports to require a rational relationship between means and ends but in practice accepts post-hoc rationalization and hypothesized purposes never articulated by the state. Class-of-one doctrine is theater: a performative check that rational basis still has some force, when in fact the real work (blocking pure vendetta) could be done explicitly through animus doctrine. The piton classification reflects that rational basis survives institutional inertia, not functional necessity. Courts maintain it because coherent alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical frame, the Olech reading might appear as recognition of a natural-law principle: that arbitrariness, taken to its extreme (singling out one person with no rational differentiation), is inherently irrational. This perspective views the class-of-one doctrine as discovering an immutable boundary: no system that claims rationality can protect naked vendetta against one person. However, the structural data contradicts this—the reading is contingent on a specific doctrinal interpretation (that rational basis permits no vendetta), not an inevitable logical truth. This is a false summit: the engine will reclassify through FSM detection.
constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_basis_tier__class_of_one_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_basis_tier__class_of_one_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rational_basis_tier__class_of_one_reading, TR),
    TR >= 0.70.

:- end_tests(rational_basis_tier__class_of_one_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, declining. The class-of-one reading allows courts to intervene when a single person is singled out without rational differentiation. This suppresses the state's ability to extract through pure spite directed at individuals. However, rational basis still permits extraction through post-hoc rationalization—the state can articulate a conceivable (not actual) rational purpose, and the individual has already borne the cost of litigation. The extractiveness value reflects that Olech reduced but did not eliminate spite-based extraction. The declining trajectory (0.48 → 0.38) reflects growing judicial willingness to require articulable purpose, not just conceivable purpose. Suppression (0.52): Moderate-high, stable. Suppression operates through the burden-of-proof asymmetry: the individual must prove the state acted from vendetta; the state articulates purpose (actual or hypothesized) and sees the burden satisfied. Procedural barriers (discovery limits, qualified immunity, administrative deference) suppress individual ability to access state motive and intent evidence. This suppression is structural and not declining—it reflects the rational basis framework, which the class-of-one reading leaves intact. Theater ratio (0.35): Moderate, declining. Rational basis tier still permits performative purpose-articulation, but class-of-one doctrine has shifted practice toward requiring stated purpose rather than purely hypothesized purpose. The declining trajectory reflects increasing judicial skepticism of transparent post-hoc rationalization in individual cases, though rational basis in general remains permissive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a crucial gap between beneficiary and victim perspectives. The state administrative apparatus (institutional/arbitrage) sees the class-of-one reading as compatible with rational basis—it adds a vendetta-suppression requirement but preserves the tier's broad-discretion coordination function. The singled-out individual (powerless/trapped) experiences the same rule as extractive: the burden-of-proof structure ensures that most spite-based actions survive judicial review if the state articulates any conceivable purpose. The organized advocates (organized/constrained) see the doctrine as an interim remedy with real but limited force—useful as leverage but not a solution. The rational basis tier itself (viewed as institutional actor) sees the reading as either compatible or parasitic depending on how animus doctrine develops. If animus becomes a separate tier, class-of-one becomes redundant (piton logic). The false-summit perspective (analytical/universal) risks naturalizing the vendetta-suppression principle as inherent to rationality itself, when in fact it is a contingent doctrinal commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: the singled-out individual as victim with trapped exit receives high d (close to 1.0), experiencing maximum extraction. The state apparatus as beneficiary with arbitrage exit receives low d (close to 0.0), experiencing negative chi (benefit). The moderate claimant class as constrained victim receives mid-to-high d (0.60–0.75), experiencing moderate extraction. The institutional rational basis tier sees its own structure: as a coordination mechanism, it generates low d for institutional beneficiaries; as a target of class-of-one constraints, it experiences d around 0.30–0.40 (the constraint operates on the tier, not against it directly). The engine derives d automatically from beneficiary/victim declarations; the commentary verifies that the resulting chi values align with classification. The snare perspective (singled-out individual) produces high chi (high extraction experienced); the rope perspective (state apparatus) produces low chi (low extraction, high coordination benefit); the tangled-rope perspective (claimant community) produces moderate chi (mixed experience).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by distinguishing rational basis review (tier coordination) from the vendetta-suppression requirement (extraction suppression). Rational basis tier coordinates state legitimacy—this is genuine coordination, not extraction. The class-of-one reading adds a suppression condition: the state cannot single out one person for no reason. This is not a higher tier; it is a refinement of rational basis's rationality requirement. The ambiguity is whether the refinement creates a second de facto tier (heightened review for individual cases) or remains within rational basis. The reading resolves this by claiming that individual-animus scrutiny is rational-basis-proper, not tier-elevation. This sidesteps mandatrophy by maintaining that rational basis does the work—it simply requires that the rational relationship hold for individual actions too, not just group-level policies. The false-summit threat is real: if the reading is actually a covert heightened standard (as the piton perspective suggests), then rational basis is being degraded doctrinal theater. The engine's FSM detection will flag this if beneficiary declarations create proof of functional tier-shifting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animus_distinguishability_from_rational_basis,
    'Is ''naked animus/vendetta'' distinguishable from rational basis review, or does it require a separate doctrinal tier?',
    'Doctrinal genealogy: does animus-with-bite (Moreno, Cleburne, Romer) constitute application of rational basis or invocation of an implicit heightened standard? Post-Olech case law patterns: are courts explicitly invoking class-of-one without animus language, or collapsing the two?',
    'If distinguishable and contained within rational basis: class-of-one reading is stable and coordinates rational basis tier without undermining it. If animus requires separate tier: class-of-one reading forecloses pure-deference reading and influences animus-with-bite reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animus_distinguishability_from_rational_basis, conceptual, 'Whether animus doctrine is within rational basis or requires separate tier').

omega_variable(
    intent_vs_effect_asymmetry,
    'Does the class-of-one reading require proof of actual discriminatory intent, or can it proceed from documented disparate effect without motive inference?',
    'Analysis of Olech''s requirement language and post-Olech case application: how many circuits require explicit proof of spite vs. accepting systematic differentiation as sufficient? Comparison with Washington v. Davis intent requirement.',
    'If intent-required (high proof burden): class-of-one reading is nearly identical to animus-with-bite reading, and the distinction becomes nominal. If effect-sufficient (lower burden): class-of-one is a genuine alternative pathway with lower suppression and higher individual recourse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_effect_asymmetry, empirical, 'Whether class-of-one requires intent proof or disparate effect suffices').

omega_variable(
    procedural_cost_of_negation,
    'How do discovery and procedural rules affect the burden of proving vendetta? Does the individual''s cost of access to state motives (FOIA, discovery rights, administrative records) correlate with success rates in class-of-one litigation?',
    'Empirical analysis: litigation success rates by jurisdiction and discovery regime; comparison of states with expansive public records disclosure vs. limited discovery in administrative law contexts.',
    'If procedural barriers are systematic and correlate with failure rates: extractiveness should be upward-revised and suppression increased (the structure is designed to prevent vindication). If procedural access is robust: extractiveness and suppression estimates are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_cost_of_negation, empirical, 'Procedural barriers to proving individual animus').

omega_variable(
    kernel_contest_framing,
    'Is this constraint one stable reading of the rational basis tier kernel, or is the kernel itself under deconstruction such that these readings are competing proposals rather than interpretations?',
    'Doctrinal genealogy: does the Supreme Court treat rational basis as a fixed standard that Olech clarifies, or as a malleable framework under contestation? Is the tier''s authority grounded in precedent or in ongoing institutional negotiation?',
    'If stable kernel: this reading coexists with others and influences doctrinal margins. If deconstructed kernel: this reading forecloses pure-deference reading within any coherent equal protection framework, and the contest is properly framed as authority erosion, not interpretation variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether rational basis is stable kernel or deconstructed framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rational_basis_tier__class_of_one_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbco_theater_t0, rational_basis_tier__class_of_one_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rbco_theater_t7, rational_basis_tier__class_of_one_reading, theater_ratio, 7, 0.37).
narrative_ontology:measurement(rbco_theater_t14, rational_basis_tier__class_of_one_reading, theater_ratio, 14, 0.35).

% Extraction over time
narrative_ontology:measurement(rbco_extract_t0, rational_basis_tier__class_of_one_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(rbco_extract_t7, rational_basis_tier__class_of_one_reading, base_extractiveness, 7, 0.41).
narrative_ontology:measurement(rbco_extract_t14, rational_basis_tier__class_of_one_reading, base_extractiveness, 14, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rational_basis_tier__class_of_one_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rational_basis_tier__class_of_one_reading, rational_basis_tier__animus_with_bite_reading).
narrative_ontology:affects_constraint(rational_basis_tier__class_of_one_reading, rational_basis_tier__pure_deference_reading).
narrative_ontology:affects_constraint(rational_basis_tier__class_of_one_reading, equal_protection_suspect_classification).
narrative_ontology:affects_constraint(rational_basis_tier__class_of_one_reading, administrative_arbitrariness_doctrine).

% DUAL FORMULATION NOTE:
% The class-of-one reading is one interpretation of the rational basis tier kernel. It coexists with the animus-with-bite reading (which uses irrationality finding as tool) and the pure-deference reading (which treats rational basis as near-total deference). The three readings share a single kernel (rational basis as doctrine) but diverge on what suppression the tier requires. Each reading has its own constraint story with distinct ε values and perspectival patterns. The family is linked through network edges: class-of-one influences both siblings by proposing a specific suppression mechanism (vendetta prohibition); animus-with-bite influences class-of-one by offering an alternative tool (explicit animus finding); pure-deference forecloses both by claiming the tier permits post-hoc rationalization even for vendetta.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rational_basis_tier__class_of_one_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
