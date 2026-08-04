% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the commemorative-husk reading of the Aneyoshi
 *   stone kernel: the tsunami warning stones erected after 1896 and 1933 are
 *   treated as having decayed, well before 2011, from an operative land-use
 *   rule into a symbolic object whose invocation in disaster narratives
 *   postdates and is largely disconnected from the actual settlement
 *   decisions made below the marker line across the 78 intervening years.
 *   Under this reading, ε rises over the interval not because enforcement
 *   intensified but because the gap between the stone's inscribed directive
 *   and actual construction behavior widened, while its symbolic and
 *   narrative utility to heritage and tourism institutions grew. This is a
 *   different constraint from the sibling behavioral_competence_reading,
 *   which holds that the stone retained operative force — the two readings
 *   assign structurally different beneficiary/victim sets and different ε
 *   trajectories over the same interval, per the ε-invariance principle; they
 *   are linked, not merged.
 *
 * KEY AGENTS:
 *   - aneyoshi_village_council_historical: agenda_setter at founding, authority eroding over time
 *   - aneyoshi_residents_below_marker: bore uninternalized risk, no behavioral constraint
 *   - local_tourism_and_heritage_bodies: beneficiary of the symbolic/narrative value
 *   - post_disaster_national_narrative: vindicated proposition, not an actor
 *   - future_coastal_settlers: inherit the same unconstrained risk
 *   - disaster_researchers: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.66).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.28).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'c7e95d5f-ab75-4123-924c-cd5c922476af').
narrative_ontology:cs_kernel_codification('c7e95d5f-ab75-4123-924c-cd5c922476af', fixed_text).
narrative_ontology:cs_authority_grounding('c7e95d5f-ab75-4123-924c-cd5c922476af', practice).
narrative_ontology:cs_interpretation_layer_present('c7e95d5f-ab75-4123-924c-cd5c922476af').
narrative_ontology:cs_reading_relation('c7e95d5f-ab75-4123-924c-cd5c922476af', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('c7e95d5f-ab75-4123-924c-cd5c922476af', foundational, inscription_absent_enforcement_is_inert).
narrative_ontology:cs_axiom_status(inscription_absent_enforcement_is_inert, holdable).
narrative_ontology:cs_axiom_grounding('c7e95d5f-ab75-4123-924c-cd5c922476af', inscription_absent_enforcement_is_inert, empirically_contingent).
narrative_ontology:cs_axiom('c7e95d5f-ab75-4123-924c-cd5c922476af', secondary, symbolic_reverence_is_not_behavioral_compliance).
narrative_ontology:cs_axiom_status(symbolic_reverence_is_not_behavioral_compliance, holdable).
narrative_ontology:cs_axiom_grounding('c7e95d5f-ab75-4123-924c-cd5c922476af', symbolic_reverence_is_not_behavioral_compliance, conventional).
narrative_ontology:cs_reference_frame('c7e95d5f-ab75-4123-924c-cd5c922476af', post_1933_ancestral_warning_covenant).
narrative_ontology:cs_drift_state('c7e95d5f-ab75-4123-924c-cd5c922476af', pre_2011_settlement_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c7e95d5f-ab75-4123-924c-cd5c922476af', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_bodies).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_national_narrative).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents_below_marker).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, future_coastal_settlers).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, ancestral_wisdom_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Erected and maintained the 1896/1933 memorial stones carrying the inscription warning against building below the marked line. In this reading, the council's authority over land use eroded across generations as the direct experiential memory of the two prior tsunamis faded; the stone's text persisted as an object of local reverence, but no institutional mechanism (zoning, permitting, enforceable covenant) translated the inscription into an active land-use rule binding descendants or newcomers.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_village_council_historical, agenda_setter,
    powerless, generational, constrained, local).

% Households that settled or remained in the lower elevation zone the stone warns against, in the decades before 2011, without any regulatory or social mechanism preventing that settlement. In this reading their survival in the 2011 tsunami is attributed to the single household reportedly following the marker plus broader luck and evacuation timing, not to the stone functioning as an operative constraint on where anyone actually built. They bore the risk the inscription described without the stone exerting behavioral force to reduce it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents_below_marker, payer,
    powerless, biographical, trapped, local).

% Municipal and prefectural heritage offices, and post-2011 disaster-tourism operators, present the stone as proof of indigenous disaster wisdom and as a memorial/educational site. They collect visitor attention, funding for preservation, and reputational capital from the stone's symbolic status. Their interest is served by the stone remaining a revered artifact; it is not served by an audit of whether it actually constrained construction, since a null behavioral finding does not disturb the narrative value they extract.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_bodies, beneficiary,
    moderate, generational, mobile, regional).

% The national story that traditional, low-tech ancestral warnings 'worked' in 2011 circulates in disaster-preparedness discourse, textbooks, and international media as evidence that local memory outperforms bureaucratic planning. This narrative is not an actor but a circulating proposition that benefits from the stone's continued symbolic reading; it collects no rents directly but is vindicated by, and helps sustain, the commemorative framing.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_national_narrative, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_national_narrative).

% People who will decide where to build along this coastline after the current memorial-repair cycle, relying on whatever land-use signal the stone and its surrounding institutions actually provide. If the stone is read as a inert commemorative object rather than as a binding rule, they inherit exactly the same absence of enforceable constraint that the pre-2011 residents inherited, with no correction from the disaster itself.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, future_coastal_settlers, payer,
    powerless, generational, trapped, local).

% Anthropologists and disaster-risk scholars who examine construction records, settlement patterns, and survivor testimony to determine whether the stone's inscription had operative force in land-use decisions over its 78-year history, or whether its invocation post-2011 is a retrospective narrative fitted onto outcomes that had other causes.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_bodies).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its origin, the stone was meant to solve a genuine intergenerational coordination problem: transmitting hazard knowledge past the lifespan of any single generation of survivors, so that land-use decisions decades later would still be informed by the location of the 1896 and 1933 tsunami run-up lines.
% TRANSFER_FUNCTION: In this reading, no material transfer of risk-reducing behavior occurs from the stone to residents' actual decisions; what is transferred instead is symbolic capital — reputational and narrative value — from the stone's inert presence to tourism bodies and national disaster-preparedness discourse, while the residual flood risk remains with whoever settles below the marker.
% ABSENT_VOICES: The households that built below the marker line before 2011, and their descendants, are not present in the heritage-site framing to explain why the stone did not function as a rule for them; the commemorative narrative is authored largely by parties who were not exposed to the land-use decision themselves.
% DISAPPEARANCE_RATIONALE: In this reading, if the stone were removed or forgotten tomorrow, land-use patterns in the settlement would not change, because the stone was not the operative mechanism governing where people built in the decades preceding 2011 — the same absence of binding constraint that existed with the stone present would exist without it. Only the symbolic/tourism economy built around the object would be disrupted.
% FOUNDING_PROBLEM: The stones were carved after the 1896 and 1933 tsunamis specifically to prevent future generations from resettling the flood-prone lower ground, using inscribed warnings ('do not build homes below this point') as a durability mechanism outlasting living memory.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 field surveys and settlement-history research (cited in disaster-anthropology literature examining the Aneyoshi case) note that construction below the marker occurred over the intervening decades without institutional obstruction, and that the single household credited with 'following' the marker is consistent with either genuine adherence or coincidence; researchers outside the heritage/tourism apparatus are the corroborating source, since the tourism and heritage bodies that publicize the stone have an interest in the founding problem being read as still-live and successfully solved.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as substantial (0.66 by 2011) because, in this reading, the stone's symbolic value is extracted by heritage/tourism/narrative institutions from a population that bore the actual, uncompensated flood risk the stone described but did not prevent. Suppression is low-moderate (0.28) because nothing coercive prevented residents from building below the line — the failure is absence of behavioral force, not active suppression of alternatives. Theater ratio is high and rising (0.78 by 2011) because an increasing share of the stone's social function is performative commemoration rather than operative hazard mitigation. Accessibility collapse is moderate (0.35): alternatives to building below the marker were never foreclosed, they were simply not constrained by the stone at all — low collapse is the diagnostic signature of a commemorative husk, distinguishing it from a genuine binding rule.
 *
 * PERSPECTIVAL GAP:
 *   From the heritage/tourism seat, the stone is unambiguous evidence of successful ancestral coordination — a rope. From the seat of residents who built below the marker and were exposed to real risk with no institutional correction, the same object is inert: neither coordinating nor extracting anything material from their land-use decisions, its only live function being to generate symbolic capital for others after the fact. The engine's computed divergence between these seats is exactly the signal this reading is authored to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents below the marker and future coastal settlers are declared victims: they carry the uncompensated flood risk that the stone's inscription describes without the stone translating into any zoning, permitting, or enforced setback that would have reduced their exposure. Tourism/heritage bodies and the national disaster-preparedness narrative are declared beneficiaries: they extract reputational, financial, and narrative capital from the stone's continued symbolic prestige, a benefit that does not depend on the stone having actually constrained anyone's land use. The village council sits at the origin as agenda_setter but its authority is read as having decayed across the interval — by 2011 it is no longer the operative locus of any enforceable rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting hazard knowledge past living memory to constrain resettlement) is read here as dead in operative terms — the mechanism that would have kept it alive (enforceable land-use restriction) never existed — while the symbolic apparatus around the stone persists and has arguably intensified since 2011. This is the classic piton signature: no concentrated beneficiary captures enough to be a snare, but the diffuse cost (uncorrected settlement risk) is borne by a powerless population while heritage/tourism/national-narrative interests maintain the object's prestige through low-cost commemorative activity rather than through the costlier work of actually converting the inscription into a binding rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settlement_pattern_evidentiary_gap,
    'Do historical land-registry, construction-permit, or settlement-pattern records from Aneyoshi across 1933-2011 show behavioral responsiveness to the stone''s marker line, or independent settlement irrespective of it?',
    'Archival research into local land records, aerial/cadastral survey history, and oral history interviews with residents present before 2011, specifically coding building locations against the marker elevation over time.',
    'If records show systematic avoidance of construction below the marker correlated with stone visibility/maintenance, the commemorative_husk_reading is empirically disconfirmed in favor of the behavioral_competence_reading, and this story''s claimed_type and ε trajectory would need to be retracted rather than merely revised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_pattern_evidentiary_gap, empirical, 'Whether settlement records support the husk reading or the behavioral-competence reading.').

omega_variable(
    attribution_of_2011_survival,
    'Was the survival of the one household credited with following the stone''s marker attributable to the marker''s behavioral influence, or to confounds (evacuation speed, topography, timing, unrelated risk aversion)?',
    'Case-level reconstruction of the household''s decision history: when they settled, whether they cited the stone contemporaneously (not retrospectively), and comparison with other households at similar elevation who did or did not survive.',
    'A contemporaneous, pre-2011 citation of the stone as a reason for the household''s building location would weaken this reading''s central claim; a purely retrospective attribution strengthens it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attribution_of_2011_survival, empirical, 'Whether the survival anecdote is causal evidence or retrospective narrative-fitting.').

omega_variable(
    husk_vs_rule_framing_ambiguity,
    'Is the stone better modeled as a single constraint that changed state over time (from live rule to husk), or as two coexisting constraints — a formally-worded rule and a socially-lived commemorative object — that were never actually the same mechanism?',
    'Compare this decomposition against a hypothetical single-story treatment with a time-varying epsilon; assess whether stakeholders and mandatrophy analysis remain coherent under merger.',
    'If the two readings are better modeled as a single constraint with drift rather than two structurally distinct constraints, the kernel/reading decomposition itself would need revision — this is a conceptual question about the decomposition, not about the stone''s history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_rule_framing_ambiguity, conceptual, 'Whether decomposing into two sibling readings versus one drifting constraint is the right modeling choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.25).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1950, 0.4).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1970, 0.55).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1990, 0.68).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2000, 0.72).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.78).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.66).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_commitment__behavioral_competence_reading are sibling readings of the same kernel (aneyoshi_stone_commitment). They share the physical object and the 1933-2011 interval but diverge on whether the stone exerted operative behavioral force on land-use decisions. This story (commemorative_husk_reading) authors high ε, a piton-flavored claimed_type, and a dead founding-problem status; the sibling authors lower ε and a live founding-problem status consistent with sustained coordination. Neither story's ε should be treated as an average or hedge across the two readings — each is a distinct, ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
