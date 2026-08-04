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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Reading of Practice Standardization Legitimacy
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the endogenous displacement reading of the
 *   practice-standardization kernel: calendar, measurement, or dress
 *   conventions shift because voluntary adopters — traders, elites,
 *   professionals — find the new practice more useful in an increasingly
 *   interconnected environment, and the shift diffuses outward through
 *   visible adoption curves, regional variation, and elite-to-mass emulation
 *   rather than through decree. Under this reading, resistance from holdout
 *   communities is temporary friction in a diffusion process, and the 'double
 *   life' of maintaining both old and new practice is a transitional phase
 *   that thins out as network effects compound. This reading shares its
 *   kernel with two siblings authored as separate constraints: the
 *   exogenous_override_reading (where a state authority decrees the change
 *   for modernization or fiscal reasons) and the
 *   dual_practice_equilibrium_reading (where legitimacy is domain-partitioned
 *   between state and traditional authority rather than resolved by
 *   displacement). The three readings describe the same historical episode of
 *   practice change through structurally distinct lenses, each with its own
 *   ε: this reading's ε is comparatively low (0.28) because voluntary
 *   utility-driven adoption genuinely lowers coercive overhead relative to
 *   decree, even though displaced ritual practitioners still bear real,
 *   rising cost as institutions stop cross-referencing the old system.
 *
 * KEY AGENTS:
 *   - early_adopter_elites: primary beneficiary (powerful/mobile) — converts early adoption into status and diplomatic/commercial advantage
 *   - cross_border_traders: beneficiary (moderate/arbitrage) — direct utility from interoperability, can run dual systems transitionally
 *   - urban_professional_class: beneficiary/payer (moderate/mobile) — institutional credentialing favors new practice, mild identity cost
 *   - rural_traditionalist_communities: payer (powerless/constrained) — rising practical cost of non-adoption framed as lag, not suppression
 *   - practitioners_of_displaced_ritual_calendars: payer (powerless/identity_locked) — bears the deepest cost as institutions stop recognizing the old system
 *   - diffusion_historians: analytical observer — documents the gradualism and regional variation this reading rests on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice Standardization Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e3101840-958c-494e-9700-d77cd28163cb').
narrative_ontology:cs_kernel_codification('e3101840-958c-494e-9700-d77cd28163cb', distributed).
narrative_ontology:cs_authority_grounding('e3101840-958c-494e-9700-d77cd28163cb', practice).
narrative_ontology:cs_interpretation_layer_present('e3101840-958c-494e-9700-d77cd28163cb').
narrative_ontology:cs_reading_relation('e3101840-958c-494e-9700-d77cd28163cb', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3101840-958c-494e-9700-d77cd28163cb', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('e3101840-958c-494e-9700-d77cd28163cb', foundational, legitimacy_derives_from_voluntary_uptake).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_voluntary_uptake, holdable).
narrative_ontology:cs_axiom_grounding('e3101840-958c-494e-9700-d77cd28163cb', legitimacy_derives_from_voluntary_uptake, empirically_contingent).
narrative_ontology:cs_axiom('e3101840-958c-494e-9700-d77cd28163cb', secondary, diffusion_gradualism_evidences_non_coercion).
narrative_ontology:cs_axiom_status(diffusion_gradualism_evidences_non_coercion, holdable).
narrative_ontology:cs_axiom_grounding('e3101840-958c-494e-9700-d77cd28163cb', diffusion_gradualism_evidences_non_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('e3101840-958c-494e-9700-d77cd28163cb', decentralized_utility_diffusion_norm).
narrative_ontology:cs_drift_state('e3101840-958c-494e-9700-d77cd28163cb', post_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3101840-958c-494e-9700-d77cd28163cb', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, cross_border_traders).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_professional_class).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_traditionalist_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, practitioners_of_displaced_ritual_calendars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_professional_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt the new calendar or dress convention first because it opens diplomatic, commercial, or social doors that the older practice does not. They bear the initial social cost of visible departure from custom but convert that cost into standing among peers who have made or are making the same shift. Their exit from the old practice is voluntary and reversible in principle, though rarely reversed once status accrues to the new one.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, beneficiary,
    powerful, biographical, mobile, national).

% Adopt the standardized practice because it reduces friction in transactions with parties who have already adopted it elsewhere. The utility is direct and calculable: fewer conversion errors, faster settlement, easier travel. They can maintain the old practice privately while using the new one commercially, and often do, for a transitional period.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cross_border_traders, beneficiary,
    moderate, biographical, arbitrage, regional).

% Take up the new practice through professional and educational institutions that increasingly assume it, gaining credential and employment advantages. Some bear a mild identity cost — feeling estranged from a grandparent's calendar or dress — but frame this as generational cultural evolution rather than imposed loss.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_professional_class, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_professional_class, payer).

% Continue the older practice on its own terms but find that markets, schools, and administrative touchpoints increasingly assume the standardized one, so the old practice becomes progressively less legible to institutions they must still interact with. Under this reading their disadvantage is framed as lag in a voluntary diffusion curve, not as suppression, though the practical cost to them of not adopting rises over time regardless of framing.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_traditionalist_communities, payer,
    powerless, generational, constrained, regional).

% Hold ritual and agricultural knowledge indexed to the older calendar or dress system. As the standardized practice diffuses through elite and mercantile networks, the institutions that once cross-referenced the old system (courts, markets, schools) stop doing so, leaving practitioners to maintain a 'double life' — old system at home and in ritual, new system in every external transaction. Their exit is blocked by identity: the old practice constitutes who they are, not merely what they do.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, practitioners_of_displaced_ritual_calendars, payer,
    powerless, generational, identity_locked, local).

% Study adoption curves, regional variation, and elite-to-mass diffusion patterns to argue that the practice shift was substantially voluntary and utility-driven rather than decreed, pointing to gradual uptake timelines and persistent regional holdouts as evidence against a purely coercive account.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffusion_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of interoperability: once enough parties adopt a common calendar, measurement, or dress convention, transaction costs across the adopting network fall for everyone already inside it, and the practice's utility to a new adopter rises with the number of prior adopters.
% TRANSFER_FUNCTION: Moves institutional legibility away from communities that keep the older practice and toward those who adopt the new one; no direct payment moves, but administrative, commercial, and educational recognition — and the practical value of retaining the old practice — shift from laggard to adopting populations.
% ABSENT_VOICES: Rural traditionalist communities and ritual-calendar practitioners are rarely surveyed in the diffusion-curve accounts that document this reading; their experience of rising practical cost for non-adoption is folded into aggregate adoption statistics rather than heard as a distinct complaint.
% DISAPPEARANCE_RATIONALE: If the legitimating frame of voluntary adoption disappeared, the standardized practice itself would likely persist (institutions have already reorganized around it), but its legitimacy claim would revert to bare fact-of-dominance; adopting elites would lose the ability to distinguish their choice from imposition, and holdout communities would lose the (thin) protection of being framed as merely 'not yet caught up' rather than resisting an override.
% FOUNDING_PROBLEM: Fragmented local practices (calendars, measures, dress conventions) created friction for anyone who needed to coordinate across communities — traders, travelers, administrators — and no single community had authority to compel a common standard on the others.
% FOUNDING_PROBLEM_CORROBORATION: Diffusion historians outside the adopting elite corroborate that adoption curves were genuinely gradual and regionally uneven, supporting the voluntary-utility reading for the early diffusion phase. Practitioners of the displaced ritual calendar attest that the founding problem (interoperability friction) was real for traders and elites but was never their own problem to solve, and that its 'solution' was experienced by them as institutional abandonment rather than voluntary uptake.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.28 at interval end) because under this reading the mechanism genuinely is utility-driven voluntary uptake — no single authority compels adoption, and much of the diffusion is explained by network effects that benefit early and late adopters alike. Suppression is authored low (0.22): there is no active coercive apparatus enforcing the shift, consistent with the reading's own premise. Resistance is moderate (0.30), reflecting genuine regional holdouts and the 'double life' phenomenon, but authored as temporary friction rather than active suppression per the reading's expected structural delta. Accessibility collapse is moderate (0.35) — the old practice remains legally and socially available for a long transitional window, it simply becomes progressively less institutionally legible, which is a different and gentler mechanism than outright foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters, traders, and the urban professional class sit toward the beneficiary end: they choose the new practice because its marginal utility to them, given the emerging adoption network, is genuinely positive, and their exit options (mobile, arbitrage) reflect real choice. Rural traditionalist communities and ritual-calendar practitioners sit toward the target end not because anyone extracts from them directly, but because the diffusing standard's rising utility for adopters correspondingly raises the practical cost of the old practice's declining institutional legibility — a structural externality of voluntary adoption rather than an enforced transfer. Ritual practitioners are identity_locked rather than merely constrained because their relationship to the old calendar is constitutive of ritual and agricultural knowledge, not a preference they could costlessly update.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing the practice shift into pure extraction: the coordination function (interoperability across an expanding trade and administrative network) is real, and much of the observed adoption curve is genuinely explained by utility rather than command. Classifying this as rope rather than tangled_rope or snare avoids over-reading coercion into a process that, from this reading's evidence base (gradual uptake, regional variation, elite-to-mass diffusion), was substantially voluntary. The victims named here are not victims of an enforcement apparatus but of a diffusion externality — a distinction this reading is built to preserve against readings that would flatten it into either pure coordination (erasing the ritual practitioners' real cost) or pure decree (erasing the traders' and elites' real agency).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_vs_disguised_decree,
    'Was the observed adoption curve genuinely driven by decentralized utility calculations, or did state or institutional actors seed, subsidize, or selectively enforce early adoption in ways that manufactured the appearance of organic diffusion?',
    'Archival examination of whether early-adopter elites received state subsidy, tax preference, or administrative requirement conditioning their adoption, versus adopting purely for commercial/diplomatic utility with no state involvement.',
    'If early adoption was state-seeded rather than organic, this story''s premises collapse into the exogenous_override_reading and its low extractiveness/suppression values would need substantial revision upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_disguised_decree, empirical, 'Whether the diffusion evidence base for this reading is contaminated by unacknowledged state seeding.').

omega_variable(
    externality_vs_extraction_for_holdouts,
    'Is the rising practical cost borne by rural traditionalist and ritual-calendar communities a genuine externality of voluntary network adoption elsewhere, or does it function as a de facto extraction mechanism because the institutions that stopped recognizing the old system did so by policy choice rather than pure market response?',
    'Trace specific institutional decisions (court filings, school curricula, market recognition rules) to determine whether de-recognition of the old practice was a passive byproduct of network effects or an active administrative choice made by identifiable agenda-setters.',
    'If de-recognition was an active administrative choice, the victims named here experience something closer to enforced displacement than diffusion externality, which would push this reading toward tangled_rope and narrow the gap with the exogenous_override_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_vs_extraction_for_holdouts, conceptual, 'Whether holdout cost is a pure diffusion externality or a disguised administrative extraction.').

omega_variable(
    reading_selection_evidentiary_basis,
    'What specific historical signals (adoption timeline shape, regional variance, elite-to-mass ordering, absence of decree instruments) justify selecting the endogenous_displacement_reading over the exogenous_override_reading for a given historical episode, and how confident can that selection be absent complete archival access?',
    'Comparative case coding across multiple historical practice-change episodes, scoring each on adoption-curve shape and decree-instrument presence, to calibrate how reliably these signals discriminate between readings.',
    'Low confidence in reading selection means this story''s ε and the exogenous_override_reading''s ε could both be defensible for the same underlying episode depending on which archival signals are weighted, which is exactly the kernel-contest structure this decomposition is meant to make explicit rather than resolve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'How reliably diffusion-curve evidence discriminates the endogenous reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legitimacy_of_practice_standardization__endogenous_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Three-story kernel decomposition of legitimacy_of_practice_standardization. This story (endogenous_displacement_reading) authors the voluntary-utility-diffusion account with low-moderate ε (0.28), rope-leaning classification, and a diffusion-externality victim structure. The exogenous_override_reading authors the state-decree account with a distinct beneficiary (the decreeing authority) and higher expected ε from active enforcement. The dual_practice_equilibrium_reading authors a domain-partition account where no displacement occurs at all, with correspondingly minimal victim structure. Each story keeps a single stable ε per DP-001; the kernel contest itself is not adjudicated by any one story but is visible only across the network edges linking all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
