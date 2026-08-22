% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive as Binding Land-Use Limit (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   After the 1896 and 1933 Sanriku tsunamis, the Aneyoshi village stone was
 *   erected instructing descendants not to build their homes below the marked
 *   high-water line. Across the 78-year inter-catastrophe interval modeled
 *   here (1933-2011), the village retained the directive as a binding
 *   land-use rule without any validating event; when the 2011 Tohoku tsunami
 *   arrived, Aneyoshi's dwellings sat above the line and the village survived
 *   while neighboring settlements that had built seaward were destroyed. This
 *   file instantiates the behavioral_competence_reading of the
 *   aneyoshi_stone_directive kernel: the directive as a live, binding
 *   arrangement whose content is fixed by physical geography and whose
 *   persistence requires no enforcement machinery, no collector, and no
 *   suppressed alternative. KEY AGENTS (by structural relationship):
 *   aneyoshi_households - bound party (moderate/constrained), sites dwellings
 *   above the mark, bears the foregone-lowland cost and receives the
 *   protective byproduct; aneyoshi_stone_keepers - administering party
 *   (organized/identity-fused), maintains the inscription and transmits its
 *   meaning with ceremony rather than coercion;
 *   coastal_reconstruction_planners - excluded institutional actor
 *   (institutional/mobile), would site public assets below the line;
 *   hazard_researchers - analytical observer (analytical/analytical),
 *   documents the case, bears no cost and collects no protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.03).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive as Binding Land-Use Limit (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '2cbd97d1-331a-4b90-86ab-62695848bdf6').
narrative_ontology:cs_kernel_codification('2cbd97d1-331a-4b90-86ab-62695848bdf6', fixed_text).
narrative_ontology:cs_authority_grounding('2cbd97d1-331a-4b90-86ab-62695848bdf6', lineage).
narrative_ontology:cs_interpretation_layer_present('2cbd97d1-331a-4b90-86ab-62695848bdf6').
narrative_ontology:cs_reading_relation('2cbd97d1-331a-4b90-86ab-62695848bdf6', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('2cbd97d1-331a-4b90-86ab-62695848bdf6', foundational, directive_retained_binding_force).
narrative_ontology:cs_axiom_status(directive_retained_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('2cbd97d1-331a-4b90-86ab-62695848bdf6', directive_retained_binding_force, empirically_contingent).
narrative_ontology:cs_axiom('2cbd97d1-331a-4b90-86ab-62695848bdf6', secondary, hazard_knowledge_needs_material_carrier).
narrative_ontology:cs_axiom_status(hazard_knowledge_needs_material_carrier, holdable).
narrative_ontology:cs_axiom_grounding('2cbd97d1-331a-4b90-86ab-62695848bdf6', hazard_knowledge_needs_material_carrier, empirically_contingent).
narrative_ontology:cs_reference_frame('2cbd97d1-331a-4b90-86ab-62695848bdf6', operative_inundation_boundary).
narrative_ontology:cs_drift_state('2cbd97d1-331a-4b90-86ab-62695848bdf6', tohoku_arrival_2011, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2cbd97d1-331a-4b90-86ab-62695848bdf6', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, intergenerational_hazard_memory_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live above the inscribed high-water line on the hillside, walking down to fish and farm the coast but keeping dwellings, wells, and graves above the stone's mark. The arrangement costs them the convenience and scarcity value of lowland house plots; it returns protection that earlier generations paid for in tsunami deaths. Ignoring the mark would not require leaving the village - leaving the fishing grounds and kin network would - so their practical alternative to compliance is narrower than relocation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households, beneficiary).

% Elders and household heads who maintain the stone: clearing its face, leading the annual retelling of the water's reach, and teaching children why the mark sits where it does. They administer the arrangement with ceremony, memory, and reproach rather than any coercive apparatus. Setting the maintenance aside would amount to repudiating the ancestors who carved the warning after burying their dead; within the village's self-concept that is not a choice anyone treats as available, so their attachment to the arrangement is fused with who they understand themselves to be.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_keepers, agenda_setter,
    organized, generational, identity_locked, local).

% Prefectural and municipal officials allocating reconstruction funds along the Sanriku coast. They evaluate seaward sites - ports, processing sheds, housing pads on reclaimed lowland - against budgets and deadlines, and the village's inscribed limit enters their plans only as an obstacle to efficient siting. They take no part in the village's normative order around the stone and would place public assets below the line wherever the land were available.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, coastal_reconstruction_planners, excluded,
    institutional, biographical, mobile, regional).

% Geologists, ethnographers, and disaster scholars who document the stone, reconstruct paleotsunami deposits, and compare settlements that kept the mark with settlements that built past theirs. They bear none of the arrangement's costs and receive none of its protections; their stake is what the case shows about carrying hazard knowledge across spans longer than living memory.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, hazard_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__behavioral_competence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits an empirically observed tsunami inundation limit across generations in durable inscribed form, solving the problem that living memory of a catastrophe decays faster than the inter-catastrophe interval returns.
% TRANSFER_FUNCTION: Moves nothing material in steady state. It reallocates land use: the lowland strip below the mark is withdrawn from residential siting and held as a de facto buffer, and the foregone convenience and value of that strip is borne by would-be lowland builders for the sake of future occupants.
% ABSENT_VOICES: Would-be lowland users - fisheries needing shorefront access, tourism operators, and the reconstruction planners who would site public assets seaward - object from outside the village's normative order and are not party to it. Younger villagers weighing economic opportunity against the inherited rule are present but outranked by the keeper seat. The founders themselves cannot speak; their intent survives only as mediated by the keepers' retelling.
% DISAPPEARANCE_RATIONALE: The physical hazard would persist unchanged, but the settlement pattern would not: with the directive's authority gone, houses would creep below the high-water line within a generation or two, exactly as occurred in neighboring Sanriku villages whose stones were disregarded. Had Aneyoshi's arrangement lapsed, the 2011 tsunami would have found the village occupied to the shoreline. The arrangement's product - the siting pattern - demonstrably depended on the retained directive.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami (roughly 22,000 dead) and the 1933 Showa Sanriku tsunami, survivors needed to prevent descendants with no living memory of the water's reach from resettling the inundation zone. The directive was built to carry a hazard boundary across a span longer than any witness's lifetime.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: paleotsunami sediment records on the Sanriku coast showing recurrent inundation layers across roughly six centuries (Tohoku University and Geological Survey of Japan studies), the instrumental and mortality record of the 2011 Tohoku tsunami, and prefectural disaster-history archives - none of which are parties to the village's normative order. The village's own attestation agrees, but the genealogy does not rest on it.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.06, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.06 at interval end) because the arrangement's entire steady-state cost is the foregone convenience and scarcity value of lowland house plots, and that foregone value accrues to no recipient. Suppression is near-zero (0.03): there is no enforcement apparatus to coerce compliance - the only enforcer is the ocean, which punishes violation physically rather than compelling obedience, and it appears in no derivation as an agent. Accessibility_collapse is high (0.88) because once the inundation limit is understood, building below the mark is not a workable alternative the arrangement suppresses; the sea collapses it. Resistance is low (0.08): the historical record shows 78 years of compliance with no movement inside the village to build past the mark. Theater_ratio is authored low (0.16) with a mild rise across the interval - ritual accretion around a functioning limit as living memory faded - and a dip at t=78 when the 2011 arrival re-functionalized the stone's salience. The claimed_type (mountain) and the metrics are authored independently: I believe the structure is mountain-shaped (no beneficiary of compliance, content fixed by geography) and I believe these are the descriptively true operating values; neither was tuned toward the other or toward a predicted engine output. Boltzmann coordination_type is information_standard: the arrangement's primary function is recording and transmitting a measured hazard bound across generations - the failure mode if it vanished is knowledge loss, not allocation breakdown or bond dissolution. No suppression_requirement series is authored: the enforcement picture is static by construction (there is no enforcement capacity to ratchet or decay), so the scalar carries it. Both tracked metrics run on one shared seven-point grid (0, 13, 26, 39, 52, 65, 78 - roughly generational spacing) with every metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   With no beneficiary or victim declarations, per-seat divergence is muted by construction - but it is not zero. From the excluded planner seat the arrangement presents as an unpriced obstacle to efficient siting; an institutional actor computing from that seat would register friction where the village registers background reality. From the household seat the directive is less a rule than the shape of the ground. From the analytical seat it is a natural experiment in transmitted knowledge. The engine computes these per-seat classifications from the power and exit atoms; this file does not adjudicate them. The sharpest perspectival split in this kernel runs BETWEEN readings rather than within this one: the sibling file authors the same stone from the seat where its behavioral force has already died, and that divergence is carried there, not averaged into here.
 *
 * DIRECTIONALITY LOGIC:
 *   Deliberately no beneficiaries and no victims are declared. Nothing is transferred to a collector: the arrangement's entire steady-state product is a siting pattern, its entire steady-state cost is foregone lowland value, and the foregone value accrues to no seat - it is simply not developed. Households hold role payer with secondary beneficiary because they bear the opportunity cost and receive the avoided-catastrophe byproduct, but the byproduct is produced by bathymetry and run-up physics, not by any party's collection, so it does not enter the derivation as benefit flow. The sea enforces nothing and collects nothing; it is not an agent and appears in no derivation. Effective extraction should therefore compute near the base value for every seat, which is the mountain signature this reading asserts. gain_flow is authored as 'diffuse' after checking every named seat: households' protection is a byproduct of physics, keepers collect ceremony and standing rather than receipts, and no seat captures the arrangement's yields.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - recurrent Sanriku tsunamis outliving living memory - is live, so no mandatrophy is declared and no sunset clause is authored: a hazard boundary completes no transition. The classification risk this domain runs is the inverse of the usual one: during a long quiescent gap, a live natural-limit arrangement becomes observationally indistinguishable from a dead one, because a functioning arrangement on rare hazards produces no visible output for decades. A 78-year validation gap manufactures decay-looking evidence (ritual maintenance, faded memory, no recent rescues) for what this reading holds to be a live arrangement. Holding claimed_type at mountain while authoring the mild theater accretion honestly keeps the two signals separable: theater here measures ceremony around a functioning limit, not substitution of performance for function. On the receipt surface, fixing_cost is deliberately omitted: the remove-versus-keep cost frame presumes a benefit to removal, and here removal's only yield is hazard exposure, so no cost class is established.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dispute_behavioral_force,
    'This constraint is one reading (behavioral_competence_reading) of the aneyoshi_stone_directive kernel; the sibling commemorative_husk_reading holds that the directive lost behavioral force during the inter-catastrophe interval. Which reading matches the inscription''s actual causal role in settlement decisions?',
    'Comparative analysis of building footprints against the inscribed high-water line across 1933-2011, oral-history testimony on siting decisions, and the matched contrast with Sanriku villages that disregarded equivalent stones.',
    'If the sibling reading is right, this file''s epsilon, theater_ratio, and mountain claim misdescribe the artifact - the stone belongs to the memorial family with high performative maintenance and inertial persistence, and this reading''s axioms lose their empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dispute_behavioral_force, conceptual, 'Whether the stone''s directive retained behavioral force (this reading) or went ceremonially inert (sibling reading).').

omega_variable(
    natural_limit_vs_constructed_norm,
    'Is the operative arrangement the physical inundation limit the stone records, or the inscribed rule as a social norm that could bind independently of updated hazard science?',
    'Observe whether siting tracks revised run-up models where they diverge from the inscribed line; post-2011 reassessments provide the test case.',
    'If the physics is the operative element, the stone is a measuring instrument and the mountain structure is clean; if the inscription binds independently, a constructed-norm component exists and the arrangement acquires coordination overhead the mountain claim excludes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_limit_vs_constructed_norm, conceptual, 'Whether naturality attaches to the hazard limit or to the inscribed rule.').

omega_variable(
    runup_envelope_currency,
    'Do the high-water marks encoded in 1896/1933 remain valid bounds under contemporary sea-level rise, land subsidence, and revised seismic coupling, or has the true inundation envelope moved relative to the inscribed line?',
    'Paleotsunami sediment cores, post-2011 geodetic subsidence data, and modern run-up modeling compared against the stone''s elevation.',
    'A landward-shifted envelope means the directive under-protects and its true cost profile worsens over time; a seaward shift means compliance over-constrains and the authored extractiveness understates the foregone-value component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(runup_envelope_currency, empirical, 'Currency of the inscribed hazard bound under changing geophysics.').

omega_variable(
    averted_loss_attribution,
    'How much mortality and asset destruction did retained compliance avert in the 2011 Tohoku tsunami relative to matched Sanriku settlements that built below their stones'' lines?',
    'Matched-pair comparison of 2011 mortality and damage registries for Aneyoshi against elevation- and exposure-matched neighboring settlements.',
    'Large averted-loss differentials support attributing survival to the retained arrangement (strengthening this reading''s foundational axiom); a null differential would attribute survival to topography alone and weaken the directive''s causal role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(averted_loss_attribution, empirical, 'Size of the 2011 averted-loss differential attributable to compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(aney_tr_t0, observed).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 13, 0.09).
narrative_ontology:measurement_basis(aney_tr_t13, observed).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 26, 0.12).
narrative_ontology:measurement_basis(aney_tr_t26, observed).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 39, 0.15).
narrative_ontology:measurement_basis(aney_tr_t39, observed).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 52, 0.18).
narrative_ontology:measurement_basis(aney_tr_t52, observed).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 65, 0.21).
narrative_ontology:measurement_basis(aney_tr_t65, observed).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.16).
narrative_ontology:measurement_basis(aney_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(aney_be_t0, observed).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 13, 0.04).
narrative_ontology:measurement_basis(aney_be_t13, observed).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 26, 0.04).
narrative_ontology:measurement_basis(aney_be_t26, observed).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 39, 0.05).
narrative_ontology:measurement_basis(aney_be_t39, observed).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 52, 0.05).
narrative_ontology:measurement_basis(aney_be_t52, observed).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 65, 0.06).
narrative_ontology:measurement_basis(aney_be_t65, observed).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.06).
narrative_ontology:measurement_basis(aney_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Aneyoshi tsunami stone' conflates two structurally distinct constraints: (1) the directive as a binding behavioral limit on land use (this file, behavioral_competence_reading, very low epsilon, mountain structure, no beneficiary of compliance), and (2) the stone as a memorial artifact maintained after its directive lost behavioral force (commemorative_husk_reading, authored separately with its own epsilon and payer structure). Per the epsilon-invariance principle the label was decomposed into two stories linked here; the disagreement between them is located in one structural element - whether the inscription caused siting behavior above the high-water line or merely accompanied it - and each file authors its own side.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
