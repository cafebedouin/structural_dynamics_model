% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone as Commemorative Husk (Directive Lost Behavioral Force)
 *   domain: disaster anthropology/institutional memory/land-use governance
 *
 * SUMMARY:
 *   This story instantiates the commemorative_husk_reading of the
 *   aneyoshi_stone_directive kernel: the tsunami stone at Aneyoshi carries an
 *   inscription ('do not build homes below this point') erected after the
 *   1896 and 1933 tsunamis, but during the 78-year inter-catastrophe period
 *   (1933-2011), settlement, infrastructure, and reconstruction proceeded
 *   below the marked line. Under this reading, the stone's behavioral force
 *   decayed into pure commemoration well before the 2011 tsunami revalidated
 *   its original warning — the directive functioned, for most of the
 *   interval, as a heritage object rather than an operative land-use
 *   constraint. This is a false-summit-adjacent structure read through a
 *   piton lens: what looks like durable ancestral wisdom (a mountain-like,
 *   self-evidently protective marker) is, on this reading, an arrangement
 *   whose actual behavioral content had eroded to theater while development
 *   interests captured the vacated ground. The sibling reading
 *   (behavioral_competence_reading) holds the opposite: that the directive
 *   retained binding force across the full 78 years without ever being
 *   empirically re-validated. These are not the same constraint measured two
 *   ways — they disagree about what actually governed settlement decisions
 *   during the interval, which is a factual/structural claim, not merely an
 *   evaluative one. Per the kernel rules, this file authors only the
 *   commemorative_husk reading; the sibling is a separate constraint linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - coastal_development_interests: primary beneficiary (organized/mobile) — captures land value from decayed directive
 *   - tourism_and_heritage_administrators: institutional beneficiary/agenda_setter (institutional/arbitrage) — administers the memorial framing and profits from it without conceding regulatory failure
 *   - post_disaster_reconstruction_contractors: secondary beneficiary (powerful/mobile) — profited from siting reconstruction below the marker
 *   - low_lying_settlement_residents: primary payer (powerless/constrained) — bore tsunami losses the marker was meant to prevent
 *   - future_tsunami_exposed_households: deferred payer (powerless/trapped) — inherits risk from current permitting decisions
 *   - prewar_and_postwar_survivor_lineages: excluded originating voice (powerless/trapped) — authored the directive but has no institutional enforcement channel
 *   - disaster_anthropology_researchers: analytical observer (analytical/analytical) — documents the behavioral-force decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.71).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.28).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Tsunami Stone as Commemorative Husk (Directive Lost Behavioral Force)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster anthropology/institutional memory/land-use governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, 'e3244274-3649-445e-a5ca-1594b0897e98').
narrative_ontology:cs_kernel_codification('e3244274-3649-445e-a5ca-1594b0897e98', fixed_text).
narrative_ontology:cs_authority_grounding('e3244274-3649-445e-a5ca-1594b0897e98', practice).
narrative_ontology:cs_interpretation_layer_present('e3244274-3649-445e-a5ca-1594b0897e98').
narrative_ontology:cs_reading_relation('e3244274-3649-445e-a5ca-1594b0897e98', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('e3244274-3649-445e-a5ca-1594b0897e98', foundational, inscribed_directives_require_active_institutional_maintenance).
narrative_ontology:cs_axiom_status(inscribed_directives_require_active_institutional_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('e3244274-3649-445e-a5ca-1594b0897e98', inscribed_directives_require_active_institutional_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('e3244274-3649-445e-a5ca-1594b0897e98', secondary, commemorative_accession_signals_lapsed_regulatory_status).
narrative_ontology:cs_axiom_status(commemorative_accession_signals_lapsed_regulatory_status, holdable).
narrative_ontology:cs_axiom_grounding('e3244274-3649-445e-a5ca-1594b0897e98', commemorative_accession_signals_lapsed_regulatory_status, conventional).
narrative_ontology:cs_reference_frame('e3244274-3649-445e-a5ca-1594b0897e98', post_1933_survivor_mandate).
narrative_ontology:cs_drift_state('e3244274-3649-445e-a5ca-1594b0897e98', pre_2011_settlement_equilibrium, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e3244274-3649-445e-a5ca-1594b0897e98', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, tourism_and_heritage_administrators).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, post_disaster_reconstruction_contractors).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, low_lying_settlement_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, future_tsunami_exposed_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Real estate developers, fishing-industry employers, and local business associations who want to build and site workers below the stone's marked line because that land is flatter, closer to harbor infrastructure, and cheaper to develop. They benefit directly whenever the stone is read as historical commemoration rather than an active zoning instruction, since that reading removes any normative obstacle to construction below the marker.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    organized, biographical, mobile, regional).

% Municipal and prefectural cultural-heritage offices that catalog the stone as a protected historical monument, fund its preservation, and feature it in disaster-education tourism. They administer its official status and could in principle re-designate it as an active land-use instrument, but doing so would require conceding decades of settlement below the marker was mistaken; framing it as memorial artifact costs them nothing and generates tourism and grant revenue.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, tourism_and_heritage_administrators, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, tourism_and_heritage_administrators, agenda_setter).

% Construction and engineering firms contracted for post-2011 rebuilding who profited from siting reconstruction below the historical marker line, where land was available and infrastructure connections cheaper. Their revenue depended on the stone's directive not being treated as an enforceable constraint on where rebuilding could occur.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, post_disaster_reconstruction_contractors, beneficiary,
    powerful, biographical, mobile, national).

% Households and small businesses who settled or rebuilt below the stone's marked boundary during the inter-catastrophe decades. They inherited land, jobs, and community ties tied to the low-lying zone; leaving would mean abandoning housing equity, school enrollment, and employment. They bore the successive tsunami losses the marker was meant to prevent, most visibly in 2011, without institutional warning that the marker still applied to their situation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, low_lying_settlement_residents, payer,
    powerless, biographical, constrained, local).

% Households not yet resident but who will occupy housing stock currently being built or permitted below the marker line under the commemorative-husk reading. They have no voice in current siting decisions and inherit whatever risk exposure today's permitting choices create.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, future_tsunami_exposed_households, payer,
    powerless, generational, trapped, local).

% Descendants of the 1896 and 1933 tsunami survivors who erected the stone with an explicit behavioral instruction ('do not build below this point'). Their intent is preserved in the stone's inscription but has no living institutional voice enforcing it as policy; the memorial reading treats their directive as historical sentiment rather than an active claim on land-use decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, prewar_and_postwar_survivor_lineages, excluded,
    powerless, civilizational, trapped, local).

% Scholars studying inter-generational disaster memory who document the stone's status shift from binding local norm to commemorative object and analyze why some tsunami stones nearby retained behavioral force while Aneyoshi's diminished, then partially re-hardened after 2011.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_anthropology_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In this reading, none remains active: the stone no longer coordinates settlement location decisions. Its residual function is purely commemorative — marking a historical event and providing a disaster-education touchpoint, not adjudicating where building may occur.
% TRANSFER_FUNCTION: Land value and development opportunity below the marker line transfer from the safety margin the original inscription intended to preserve, to developers, contractors, and heritage administrators who benefit from treating the stone as symbolic rather than regulatory. The cost of that transfer falls on residents occupying reoccupied low-lying land who absorb the tsunami risk the stone was erected to prevent.
% ABSENT_VOICES: The stone's original authors — 1896 and 1933 survivors — have no living institutional representative empowered to assert the inscription as binding policy; their intent is preserved in text but structurally excluded from land-use decision-making. Their descendants in the settlement are present but hold no formal veto over permitting.
% DISAPPEARANCE_RATIONALE: Under this reading the directive's behavioral force is already absent during the inter-catastrophe period — removing the stone itself would change tourism and commemorative programming but would not alter present land-use patterns, since settlement below the marker line already proceeded for decades without the stone functioning as an enforced constraint.
% FOUNDING_PROBLEM: The stone was erected to prevent future generations from resettling land that had been repeatedly destroyed by tsunamis, using a durable physical marker because institutional memory of the 1896 and 1933 disasters would fade faster than the stone would.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropology researchers documenting the pre-2011 land-use record attest that the marker's original behavioral function had lapsed for decades before the 2011 tsunami, based on settlement patterns and permitting records; this corroboration comes from outside both the development interests and the heritage administrators who benefit from the memorial framing. No coastal development or tourism authority attests the directive was still functionally binding prior to 2011.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because, on this reading, the vacated safety margin below the marker was actively captured by development, tourism, and reconstruction interests who benefited precisely because the directive's behavioral content had lapsed — this is a transfer from foregone safety to realized land value, not a coordination cost. Suppression is authored moderate-low (0.28) because nothing coercive prevented residents from heeding the stone; the failure is one of institutional non-enforcement and memory decay, not active suppression of alternatives — no one blocked people from staying above the line, the marker simply stopped functioning as a behavioral instruction for most of the interval. Theater ratio is authored very high (0.82 by interval end) because heritage administration substituted commemorative maintenance (plaques, tourism narratives, preservation grants) for the marker's original regulatory function — the rising theater_ratio series traces this substitution directly, from largely functional (0.15 near founding) to almost entirely performative (0.82) just before 2011. Accessibility collapse is authored moderate (0.35): safer upland sites remained physically available throughout, they were simply not chosen because the institutional signal had gone quiet. Resistance is authored moderate (0.55) reflecting periodic local voices (elders, occasional planning objections) that the directive still mattered, without ever mounting an effective institutional challenge to development below the line.
 *
 * PERSPECTIVAL GAP:
 *   From the tourism and heritage administrator seat, the arrangement looks like successful cultural preservation — the stone is protected, documented, and celebrated. From the low-lying resident seat, the same arrangement is invisible failure: an institution that could have re-asserted the directive as binding zoning instead let it become scenery. The engine should compute these seats to diverge sharply: agenda-setter/beneficiary seats read as low-extraction stewardship, payer seats read as high-extraction abandonment.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal development interests, reconstruction contractors, and heritage administrators are declared beneficiaries because the commemorative-husk state of the directive directly enabled the land-use and revenue outcomes they captured — their d sits near the beneficiary end. Low-lying residents and future exposed households are declared victims because they absorbed the risk the directive's decay left unaddressed, with residents' exit constrained by economic and social entrenchment and future households' exit trapped entirely (they do not yet exist as decision-makers). The excluded survivor lineages are neither beneficiary nor victim in the transactional sense but are the structurally silenced originating authority — their exclusion is what allows the husk reading to persist without contest.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a clear mandatrophy case under the commemorative_husk reading: the founding problem (prevent resettlement of tsunami-prone low ground) is authored as dead well before 2011, yet the arrangement persisted as heritage infrastructure — precisely the founding_problem_status='dead' + disappearance_verdict='world_unchanged' mismatch pattern the R5 consumer is built to catch. Classifying this as piton (rather than snare) reflects that no single concentrated beneficiary captures enough to have engineered the decay deliberately; extraction is diffuse across development, tourism, and contracting interests who each benefited incidentally from an institutional memory failure none of them caused. This prevents mislabeling institutional forgetting as either pure coordination (it manifestly failed to coordinate settlement away from risk) or as an engineered snare (no actor is shown deliberately suppressing the directive's force — the decay is closer to inertial atrophy that beneficiaries then exploited passively).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_kernel_disagreement,
    'Did the Aneyoshi stone''s behavioral directive actually retain binding normative force across the 1933-2011 inter-catastrophe period (behavioral_competence_reading), or did it decay into a commemorative artifact that ceased to shape settlement decisions well before 2011 (this reading)?',
    'Historical permitting and settlement records for land below the marker line, oral history interviews with pre-2011 residents about whether the stone was cited in siting decisions, and municipal planning archive review for any period in which the directive was formally or informally invoked to block development.',
    'If the behavioral_competence_reading is correct, this story''s high-extraction beneficiary structure (development interests profiting from decay) does not exist as authored — the directive was doing real coordination work and this file''s ε would be a misreading of a still-functional mountain-adjacent rope. If this reading is correct, the sibling''s implicit institutional-competence claim is a false summit obscuring 78 years of unaddressed risk transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_kernel_disagreement, empirical, 'The core kernel disagreement between the two readings of the Aneyoshi stone directive: whether behavioral force persisted or lapsed during the inter-catastrophe interval.').

omega_variable(
    commemoration_vs_regulation_boundary,
    'Is a stone inscription with no attached statutory or administrative enforcement mechanism ever properly classified as an active land-use directive, or is ''directive'' itself an anachronistic backward projection once the object has been formally accessioned as a cultural heritage monument?',
    'Comparative analysis of other Japanese tsunami stones and whether any retained enforceable status through formal incorporation into municipal zoning codes, versus those (like Aneyoshi, on this reading) that remained purely inscriptional.',
    'If commemorative status and regulatory status are conceptually incompatible once heritage accession occurs, this reading''s classification as piton (decayed regulatory function) is strengthened; if heritage status and regulatory force can coexist, the commemorative framing may itself be a beneficiary-serving reclassification rather than a neutral description.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemoration_vs_regulation_boundary, conceptual, 'Whether heritage/commemorative status and behavioral/regulatory status are mutually exclusive categories for a marker object.').

omega_variable(
    post_2011_rehardening_asymmetry,
    'After the 2011 tsunami vindicated the stone''s original warning, did behavioral force actually re-harden into enforceable zoning, or did the commemorative framing persist even after empirical vindication?',
    'Post-2011 municipal land-use planning records and relocation policy documents for the Aneyoshi area and comparable Tohoku coastal communities.',
    'If force re-hardened post-2011, this supports treating the pre-2011 period as a distinct, closed piton episode rather than an ongoing one; if the commemorative framing persisted even after vindication, that would suggest the theater_ratio trajectory authored here should extend upward rather than plateau at the interval end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2011_rehardening_asymmetry, empirical, 'Whether the 2011 disaster reversed the commemorative-husk dynamic or left it intact even after empirical vindication of the original warning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 13, 0.3).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 26, 0.48).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 39, 0.62).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 52, 0.74).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 65, 0.8).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 78, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 13, 0.35).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 26, 0.5).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 39, 0.62).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 52, 0.68).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 65, 0.72).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 78, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.1).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story (commemorative_husk_reading) and its sibling (behavioral_competence_reading, not authored in this file) are two readings of the single aneyoshi_stone_directive kernel. They disagree on a structural, not merely evaluative, question: whether the stone's inscription retained operative behavioral force across the 1933-2011 interval. This reading authors high extractiveness (0.71) and a piton classification, reflecting decayed function captured by development interests; the sibling reading is expected to author low extractiveness and a rope-or-mountain-adjacent classification reflecting sustained, unvalidated institutional competence. Per the ε-invariance principle these are two distinct constraints sharing one text and one physical object, not one constraint measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
