% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Endogenously-Driven Practice Standardization
 *   domain: political_history/institutional_change/modernization_studies
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous-displacement reading of the
 *   legitimacy-of-practice-standardization kernel. The reading asserts that
 *   practice change (calendar systems, administrative procedures, measurement
 *   standards, dress norms) is legitimate when it emerges from voluntary
 *   adoption driven by perceived utility or cultural evolution, rather than
 *   from explicit state decree or institutional mandate. The referent is the
 *   standing arrangement under contest: the old practice and the new practice
 *   coexisting during a transition period, evaluated from the endogenous
 *   reading's own frame (what makes voluntary adoption legitimate). The
 *   reading's endorsed alternative—a world where practice change is
 *   universally available through voluntary mechanisms without late-resistor
 *   friction—is NOT the referent; ε measures the standing arrangement's
 *   extraction from the reading's epistemic standpoint. The constraint
 *   describes the adoption curve, the diffusion dynamics, and the
 *   institutional standardization that follows, not the counterfactual world
 *   without friction.
 *
 * KEY AGENTS:
 *   - early_adopters: perceive utility and shift first (moderate power, mobile exit) — benefit from coordination efficiency
 *   - utility_perceiving_cohorts: follow when adoption becomes peer-dominant (organized power, constrained exit) — consolidated adopters
 *   - tradition_maintaining_authority: holds legitimacy through continuity but loses it through irrelevance (institutional power, trapped exit) — pays the cost of institutional illegitimacy
 *   - late_resistors: identity-locked to old practice, face social exclusion and friction (powerless, identity-locked exit) — bear the highest friction costs
 *   - institutional_standardizers: monitor adoption curves and ratify them legally (institutional, analytical) — agenda-setters who complete the displacement
 *   - comparative_authority_observers: external witnesses who document whether change is endogenous or exogenous (institutional, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.38).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Endogenously-Driven Practice Standardization").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/institutional_change/modernization_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '21e2fd00-a49d-4e7b-890f-ee64a4c59d7a').
narrative_ontology:cs_kernel_codification('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', distributed).
narrative_ontology:cs_authority_grounding('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', lineage).
narrative_ontology:cs_interpretation_layer_present('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a').
narrative_ontology:cs_reading_relation('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', foundational, voluntary_adoption_confers_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_adoption_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', voluntary_adoption_confers_legitimacy, instrumental).
narrative_ontology:cs_axiom('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', foundational, utility_perception_drives_sustainable_practice_change).
narrative_ontology:cs_axiom_status(utility_perception_drives_sustainable_practice_change, holdable).
narrative_ontology:cs_axiom_grounding('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', utility_perception_drives_sustainable_practice_change, empirically_contingent).
narrative_ontology:cs_reference_frame('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', dual_practice_transition_phase).
narrative_ontology:cs_drift_state('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', post_institutional_standardization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21e2fd00-a49d-4e7b-890f-ee64a4c59d7a', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_perceiving_cohorts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tradition_maintaining_authority).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, late_resistors).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_adoption_principle).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perceive utility in the new practice (calendar system, dress code, administrative form) and adopt it voluntarily because it solves a coordination problem or offers practical advantage. They demonstrate the new practice works and carries social prestige; their adoption creates diffusion incentives. They bear the cost of nonconformity during transition but benefit from coordination efficiency and social capital once adoption spreads.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, regional).

% Observe gradual adoption curves and decide to switch when the new practice becomes dominant in their peer network or when its utility exceeds the switching cost. They follow early adopters rather than leading; their adoption consolidates the change and makes reversal costly. They benefit from reduced friction once the new practice becomes standard.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_perceiving_cohorts, beneficiary,
    organized, generational, constrained, national).

% Holds legitimacy through continuity with established practice and doctrine. As voluntary adoption spreads, their authority erodes through observed irrelevance rather than explicit prohibition. They bear the cost of institutional illegitimacy when their endorsed practices fall into disuse, and the cost of enforcement if they attempt to compel the old practice against apparent utility and cultural momentum.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, tradition_maintaining_authority, payer,
    institutional, civilizational, trapped, national).

% Cannot exit the identity-fusion with traditional practice without experiencing profound identity dislocation (a priest who abandons liturgical calendar, a merchant refusing new commercial dating systems). They are compelled to maintain the old practice within their domain but face progressive social irrelevance, economic friction, and exclusion from domains where the new practice is mandatory. Their resistance is real but increasingly ineffective as adoption spreads.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, late_resistors, payer,
    powerless, biographical, identity_locked, local).

% Monitor adoption curves, coordinate the timing of administrative/legal recognition of the new practice, and eventually declare the transition complete by recognizing only the new practice in official domains (tax records, legal documentation, educational curriculum). They do not decree the change initially but ratify it once voluntary adoption becomes dominant, thereby locking out the old practice and completing the displacement.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, institutional_standardizers, agenda_setter,
    institutional, generational, analytical, national).

% Are positioned outside the jurisdiction to observe adoption patterns, measure diffusion speed, and document whether change appears endogenous (gradual, adoption-curve-shaped, driven by perceived utility) or exogenous (rapid, decree-driven, imposed without prior adoption). Their testimony disambiguates readings when state authorities later claim the change was 'popular demand' versus when activists claim it was coercive standardization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, comparative_authority_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of dual-practice friction and measurement incompatibility during institutional modernization. When old and new practices coexist (calendar systems, measurement standards, administrative procedures, dress codes), transaction costs rise: merchants must track two date systems, scribes must maintain parallel records, communities fragment across generations on what counts as 'legitimate' practice. Voluntary adoption of a unified practice reduces these costs for agents who perceive utility in the transition and creates diffusion incentives as early adopters demonstrate the new practice works.
% TRANSFER_FUNCTION: Transfers cultural authority from tradition-maintaining institutions (church hierarchies, customary law courts, ritual authorities) to early adopters and utility-perceiving cohorts, accomplished through observed irrelevance and adoption-curve momentum rather than through legal prohibition. The transfer becomes institutionally locked when institutional standardizers legally recognize only the new practice, foreclosing the old practice in domains where institutional status matters (tax records, legal documentation, educational curricula). Late resistors bear the cost of this displacement through social invisibility in shared domains and economic friction in commercially-integrated domains.
% ABSENT_VOICES: Late resistors and identity-locked practitioners are excluded from early-adoption decisions; their voice appears only as 'resistance to be managed' rather than as a party to a negotiation about practice change. Exogenous authorities (state decree advocates) are absent from this reading's initial framing; they would argue that voluntary adoption is too slow for collective fiscal/diplomatic benefit. Dual-practice equilibrium advocates are absent; they would argue for permanent domain-partitioned coexistence rather than displacement.
% DISAPPEARANCE_RATIONALE: If this constraint—the legitimacy of endogenous practice change—were negated, the institutional legitimacy structure would shift radically. Without the principle that voluntary adoption confers legitimacy, either practices would persist in permanent dual-use equilibrium (dual-practice reading's frame) or change would require explicit state decree (exogenous-override reading's frame). The world rearranges because adoption curves cease to be a source of legitimacy, institutional standardization loses its moral ground in 'popular preference,' and authority reverts to either institutional continuity (old practice remains legitimate because it is established) or explicit coercive mandate (new practice is legitimate because the state decrees it for collective benefit).
% FOUNDING_PROBLEM: The founding problem is the coordination friction and measurement incompatibility that emerges when societies modernize without displacing old practices—merchants tracking two calendars, administrative systems running in parallel, educational transmission fractured across generations. This problem is real and documented in comparative historical records of calendar reforms (Gregorian adoption in Catholic Europe, Julian-to-Gregorian conversion in Orthodox and Western traditions, French Revolutionary calendar adoption), metric system transitions, and administrative procedure standardization. The constraint solves this by treating voluntary adoption curves as evidence of genuine utility and therefore as a legitimate ground for institutional standardization.
% FOUNDING_PROBLEM_CORROBORATION: The endogenous reading attests the coordination friction is real; comparative historians (David Landes on measurement standards, Anthony Aveni on calendar systems, Witold Kula on metrology) document adoption curves that follow S-shaped diffusion patterns consistent with utility-driven adoption. The exogenous-override reading attests that voluntary adoption is too slow and coordination-insufficient for collective benefit (fiscal systems require unified dating, international trade requires unified measures); state actors document cases where voluntary adoption stalled and decree was necessary. The dual-practice equilibrium reading documents communities (Jewish diaspora, traditional agriculture, ritual communities) where dual practices persisted in stable equilibrium for centuries without displacement. No reading is self-attesting; all rest on external comparative historical documentation of actual transitions.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply in the early-to-mid transition (0.15→0.42 across t=0 to t=60) because late resistors are progressively forced into compliance through social irrelevance and economic friction, not through legal prohibition. The constraint's enforcement operates through adoption curves and institutional standardization: as the new practice becomes dominant, the old practice becomes dysfunctional in shared domains, effectively suppressing it without formal decree. The grid models this level-resolved: structural accessibility collapse rises steeply as alternative-practice domains close off; organizational stakes inflate as firms and institutions must choose; class-level and individual resistance begin high (65% and 55%) because the old practice is still culturally available, but decline sharply (15% and 22%) as adoption consolidates and reversal becomes unthinkable. Theater ratio remains modest (0.08→0.19) throughout the transition because the constraint's operation is genuinely coordination-driven (adoption curves are real, utility is perceived) rather than performatively maintained; once consolidation completes (t≈80+), both extractiveness and theater decline (0.42→0.32, 0.19→0.15) because the constraint shifts from an active mechanism enforcing displacement to an inert social fact (nobody chooses the old practice anymore, not because they are suppressed but because it has become objectively obsolete in shared domains).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (institutional standardizers) and early adopters experience the constraint as beneficial coordination—voluntary adoption demonstrating genuine utility, institutional ratification completing an inevitable transition. Late resistors and identity-locked practitioners experience the same mechanism as coercive standardization—their exit options collapse not through legal prohibition but through social irrelevance, and their identity is progressively delegitimized. The exogenous-override reading (state decree) would see the same adoption curve but attribute it to hidden state pressure or coordinated institutional capture rather than genuine utility diffusion. This gap is structural, not perceptual: from different positions in the adoption process, the same constraint appears as fundamentally different mechanisms. The engine computes this divergence from directionality (early adopters → low d; late resistors → high d) and power/exit combinations (moderate+mobile vs. powerless+identity-locked).
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and utility-perceiving cohorts sit near d=0.2 (beneficiaries with leverage): they choose to adopt, benefit from coordination efficiency, and retain the option to revert (mobile exit) during the early phase. Tradition-maintaining authority sits near d=0.65 (target): loses institutional legitimacy through observed irrelevance without explicit legal defeat; the suppression of their endorsed practice is accomplished through social preference for utility, not through coercive enforcement. Late resistors sit near d=0.85 (full target): identity-locked to the old practice, face friction costs they cannot escape without identity dislocation, and become progressively invisible in shared domains as the new practice becomes mandatory. Institutional standardizers sit near d=0.5 (symmetric): they administrate the transition but do not initiate it; their enforcement is ratification, not prescription. The grid models how this directionality concentrates suppression on the powerless (individual and class levels show 12%+15% suppression at t=100) while structural-level suppression remains modest (18%) because the constraint operates through adoption preference, not formal coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—dual-practice friction during modernization—is live and real at t=0 (calendar/measurement systems genuinely create transaction costs when coexisting). By t≈60–80, the problem is functionally dead: the new practice has achieved near-universal adoption in shared administrative/commercial domains, and the coordination friction has disappeared. Yet the constraint persists because institutional standardizers have legally recognized only the new practice, and late resistors continue to bear friction costs in domains where old practice was relegated to private/ritual use. This is a genuine mandatrophy: the founding problem is solved (coordination friction eliminated) but the constraint remains (late resistors still suppressed, dual-practice exit options closed off). The constraint remains legitimate under the endogenous reading only as long as adoption appears spontaneous and utility-driven; once institutionally mandatory recognition occurs (t≈60–80), the ratification step transitions the constraint from coordination mechanism to institutional lock-in. The reading's own frame becomes ambiguous at t>80: is the constraint still a coordination mechanism (adoption so universal that alternatives are unthinkable)? Or has it shifted to a snare (new practice now enforced institutionally, old practice suppressed by legal recognition rather than voluntary preference)? The endogenous reading asserts the former; the exogenous-override reading would document evidence of the latter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_induced_adoption,
    'Are adoption curves genuinely driven by perceived utility and voluntary choice, or are they induced by coordinated institutional pressure and elite modeling that creates the appearance of voluntary adoption?',
    'Temporal sequence analysis: if institutional standardization precedes adoption curves, adoption is induced; if adoption curves precede official recognition, adoption is endogenous. Comparative case studies of transitions where adoption stalled despite institutional support (evidence for exogenous limitation) versus transitions where adoption accelerated despite institutional resistance (evidence for endogenous drive).',
    'If adoption is induced, the endogenous reading''s legitimacy claim collapses and the constraint reclassifies as a snare (coercive standardization wearing the mask of voluntary preference). If adoption is genuinely endogenous, the reading''s coordination frame holds and the constraint remains a rope or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_induced_adoption, empirical, 'Whether measured adoption curves are driven by utility perception or institutional pressure.').

omega_variable(
    late_resistor_suppression_mechanism,
    'Is the suppression of late resistors structural (economic disadvantage in domains where the new practice is mandatory) or internalized (the late resistor''s own identity-fusion preventing exit)?',
    'Post-exit trajectory analysis: if late resistors who emigrate to dual-practice jurisdictions or live in communities where the old practice persists continue to experience suppression, it is partially internalized. If suppression lifts completely after exit, it is purely structural.',
    'If suppression is structural, the constraint''s effective extraction is anchored to power/scope and can be reduced by creating domains where the old practice remains viable. If suppression is internalized, the constraint extracts even from agents who have exited; identity-lock makes the suppression travel with the late resistor and the constraint reclassifies toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(late_resistor_suppression_mechanism, empirical, 'Whether suppression of tradition-maintaining authority and late resistors is structural or internalized.').

omega_variable(
    utility_perception_heterogeneity,
    'What proportion of adoption is driven by genuine perceived utility (the new practice objectively reduces coordination costs in the adopter''s domain) versus social-proof utility (the adopter perceives utility because others have adopted)?',
    'Controlled adoption studies isolating technical utility (e.g., metric system''s computational efficiency) from social utility (e.g., adoption because peers adopted). Geographic/professional domain analysis: do adopters in domains where the new practice offers no technical advantage still adopt at the same rate as domains where it does?',
    'If adoption is primarily social-proof driven, the endogenous reading''s utility-legitimacy claim is weakened; the constraint may be a coordination mechanism on false premises. If genuine technical utility drives a significant portion, the reading''s frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_perception_heterogeneity, empirical, 'Proportion of adoption driven by genuine utility versus social-proof.').

omega_variable(
    institutional_standardization_as_enforcement,
    'Does institutional standardization (legal recognition of only the new practice) complete a voluntary displacement, or does it transition the constraint from coordination to coercion?',
    'Behavioral shift analysis: do agents'' adoption patterns change after institutional standardization? Does compliance expand into domains where adoption had not yet occurred? Do late resistors experience increased enforcement pressure once the new practice gains legal status?',
    'If institutional standardization completes a voluntary displacement (adoption already dominant, legal change merely formalizes it), the constraint remains a rope or tangled rope. If standardization initiates coercive compliance in previously-voluntary domains, the constraint transitions to snare and the endogenous reading''s frame becomes post-hoc rationalization of exogenous imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_standardization_as_enforcement, empirical, 'Whether institutional standardization completes voluntary displacement or initiates coercive expansion.').

omega_variable(
    alternative_framing_possibility,
    'Is the endogenous reading''s core premise—that voluntary adoption from utility perception legitimates practice change—compatible with the exogenous reading''s claim that state coordination for collective benefit is legitimate, or do these premises foreclose each other in a single framework?',
    'Logical analysis: a framework that treats both voluntary adoption and state-mandated coordination as legitimate sources of practice legitimacy is coherent if the two mechanisms coexist without conflicting (voluntary adoption in some domains, state standardization in others). Such a framework would make the readings coexist rather than foreclose.',
    'If the readings coexist in a hybrid framework, the constraint permits both endogenous and exogenous legitimacy sources. If they genuinely foreclose, the constraint''s legitimacy depends on which reading''s premises are true in the actual case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framing_possibility, conceptual, 'Whether endogenous and exogenous legitimacy premises are logically compatible in a single institutional framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(legi_tr_t0, projected).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(legi_tr_t40, observed).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(legi_tr_t60, observed).
narrative_ontology:measurement(legi_tr_t80, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(legi_tr_t80, observed).
narrative_ontology:measurement(legi_tr_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(legi_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(legi_be_t0, projected).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(legi_be_t40, observed).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(legi_be_t60, observed).
narrative_ontology:measurement(legi_be_t80, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(legi_be_t80, observed).
narrative_ontology:measurement(legi_be_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 100, 0.32).
narrative_ontology:measurement_basis(legi_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(legi_su_t0, projected).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(legi_su_t40, observed).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 60, 0.24).
narrative_ontology:measurement_basis(legi_su_t60, observed).
narrative_ontology:measurement(legi_su_t80, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 80, 0.21).
narrative_ontology:measurement_basis(legi_su_t80, observed).
narrative_ontology:measurement(legi_su_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement_basis(legi_su_t100, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=100
narrative_ontology:measurement(legi_grid_01, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(class), 0, 0.4).
narrative_ontology:measurement(legi_grid_02, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(class), 100, 0.65).
narrative_ontology:measurement(legi_grid_03, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(individual), 0, 0.3).
narrative_ontology:measurement(legi_grid_04, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(individual), 100, 0.55).
narrative_ontology:measurement(legi_grid_05, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(organizational), 0, 0.35).
narrative_ontology:measurement(legi_grid_06, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(organizational), 100, 0.72).
narrative_ontology:measurement(legi_grid_07, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(structural), 0, 0.25).
narrative_ontology:measurement(legi_grid_08, legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse(structural), 100, 0.68).
narrative_ontology:measurement(legi_grid_09, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(legi_grid_10, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(class), 100, 0.18).
narrative_ontology:measurement(legi_grid_11, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(individual), 0, 0.55).
narrative_ontology:measurement(legi_grid_12, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(individual), 100, 0.22).
narrative_ontology:measurement(legi_grid_13, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(legi_grid_14, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(organizational), 100, 0.12).
narrative_ontology:measurement(legi_grid_15, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(structural), 0, 0.65).
narrative_ontology:measurement(legi_grid_16, legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance(structural), 100, 0.15).
narrative_ontology:measurement(legi_grid_17, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(class), 0, 0.25).
narrative_ontology:measurement(legi_grid_18, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(class), 100, 0.35).
narrative_ontology:measurement(legi_grid_19, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(individual), 0, 0.3).
narrative_ontology:measurement(legi_grid_20, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(individual), 100, 0.28).
narrative_ontology:measurement(legi_grid_21, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(organizational), 0, 0.2).
narrative_ontology:measurement(legi_grid_22, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(organizational), 100, 0.42).
narrative_ontology:measurement(legi_grid_23, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(structural), 0, 0.15).
narrative_ontology:measurement(legi_grid_24, legitimacy_of_practice_standardization__endogenous_displacement_reading, stakes_inflation(structural), 100, 0.38).
narrative_ontology:measurement(legi_grid_25, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(class), 0, 0.15).
narrative_ontology:measurement(legi_grid_26, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(class), 100, 0.22).
narrative_ontology:measurement(legi_grid_27, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(individual), 0, 0.12).
narrative_ontology:measurement(legi_grid_28, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(individual), 100, 0.15).
narrative_ontology:measurement(legi_grid_29, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(organizational), 0, 0.1).
narrative_ontology:measurement(legi_grid_30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(organizational), 100, 0.2).
narrative_ontology:measurement(legi_grid_31, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(structural), 0, 0.08).
narrative_ontology:measurement(legi_grid_32, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression(structural), 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The legitimacy-of-practice-standardization kernel comprises three structurally distinct constraints instantiating three readings. The endogenous_displacement_reading (this story) grounds legitimacy in voluntary adoption and utility perception, producing ε=0.38 (moderate extraction through progressive institutional lock-in). The exogenous_override_reading grounds legitimacy in state decree and collective benefit, producing ε≈0.55–0.65 (high extraction from those resisting standardization). The dual_practice_equilibrium_reading asserts that both old and new practices can remain legitimate in domain-partitioned authority structures, producing ε≈0.20 (low extraction, stable coexistence). These are not the same constraint viewed differently; they instantiate different answers to the fundamental question of what makes practice change legitimate. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. The network links connect them as a decomposed constraint family. Comparative historical cases (Gregorian calendar, metric system, French Revolutionary calendar, Japanese Meiji modernization) provide evidence for which reading best explains specific transitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
