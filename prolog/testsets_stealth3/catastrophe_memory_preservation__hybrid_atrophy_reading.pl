% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe-Memory Ritual — Hybrid Atrophy Reading (Survival Function Lost, Mourning Form Persisting)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A riverine community keeps an obligatory annual commemoration of a
 *   founding flood: a fast, a night of lament recitation naming the dead,
 *   processions along the old waterline, and ceremony contributions funding a
 *   hereditary officiant line. The hybrid atrophy reading — the reading
 *   instantiated here — holds that the rite was founded to transmit
 *   survival-competence (precursor recognition, protective routines,
 *   rebuilding procedure) and that under professionalized hazard management
 *   its operational content has dropped out of the canon while the mourning
 *   form persisted and expanded. What remains is a costly practice whose
 *   original payoff no longer arrives, sustained by lineage inertia and the
 *   sincerity of grief rather than by anyone's deliberate gain. KEY AGENTS
 *   (by structural relationship): ritual_officiants — administering lineage
 *   (organized/identity_locked), could reform the rite only at existential
 *   cost to its own office; communal_memory_elders — diffuse beneficiary
 *   (organized/identity_locked), collects standing, guards the narrative;
 *   observant_households — principal paying seat with incidental identity
 *   return (moderate/constrained); younger_generation_members — inheriting
 *   payers without adaptive payoff (powerless/mobile); reform_faction_members
 *   — payers whose objections never reach the agenda (moderate/constrained);
 *   civil_defense_planners — excluded institutional actor holding the
 *   substitute function (institutional/analytical); ritual_studies_scholars —
 *   analytical observers documenting the redaction record
 *   (analytical/analytical). Family note: the sibling stories author
 *   different epsilon over the same referent — survival_competence_reading
 *   prices the costs as live training (low epsilon),
 *   mourning_practice_reading weighs costs against symbolic payoff alone;
 *   this file authors 0.46 against the standing arrangement assessed as
 *   costly-without-adaptive-payoff. The claim/metric gap discipline applies:
 *   claimed_type is authored from structure (atrophied former function,
 *   inertia persistence, no capturer), the metrics from descriptive
 *   operation, independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.46).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe-Memory Ritual — Hybrid Atrophy Reading (Survival Function Lost, Mourning Form Persisting)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'bfeaebdb-03c1-419d-b4ff-96b069c68c3c').
narrative_ontology:cs_kernel_codification('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', fixed_text).
narrative_ontology:cs_authority_grounding('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', lineage).
narrative_ontology:cs_interpretation_layer_present('bfeaebdb-03c1-419d-b4ff-96b069c68c3c').
narrative_ontology:cs_reading_relation('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', foundational, ritual_origin_was_operational_survival_training).
narrative_ontology:cs_axiom_status(ritual_origin_was_operational_survival_training, holdable).
narrative_ontology:cs_axiom_grounding('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', ritual_origin_was_operational_survival_training, empirically_contingent).
narrative_ontology:cs_axiom('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', foundational, ritual_operational_content_decayed_under_modernity).
narrative_ontology:cs_axiom_status(ritual_operational_content_decayed_under_modernity, holdable).
narrative_ontology:cs_axiom_grounding('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', ritual_operational_content_decayed_under_modernity, empirically_contingent).
narrative_ontology:cs_axiom('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', secondary, mourning_form_is_residual_not_independent).
narrative_ontology:cs_axiom_status(mourning_form_is_residual_not_independent, holdable).
narrative_ontology:cs_axiom_grounding('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', mourning_form_is_residual_not_independent, empirically_contingent).
narrative_ontology:cs_reference_frame('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', ritual_as_operational_survival_instrument).
narrative_ontology:cs_drift_state('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', contemporary_hazard_professionalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bfeaebdb-03c1-419d-b4ff-96b069c68c3c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_officiants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, communal_memory_elders).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, observant_households).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, younger_generation_members).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, reform_faction_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, observant_households).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, ancestral_transmission_legitimacy).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, communal_endurance_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary line of celebrants who conduct the annual commemoration: they keep the liturgical scrolls, set the order of service, train successors, and receive customary stipends and offerings sized by old fixed custom rather than by negotiation. Their standing, marriages, and daily schedule are built around the observance calendar. They could propose shortening the lament cycles or handing the instructional slot to civil-defense trainers, but doing so would declare their own lineage's transmission incomplete, and no holder of the office has ever moved such a proposal past the elders.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_officiants, agenda_setter,
    organized, generational, identity_locked, regional).

% Lay custodians, mostly older members, who curate the catastrophe narrative: they choose which testimonies are read aloud, guard the naming lists of the dead, and arbitrate disputes about proper observance. The season is the source of their local standing and their sense of usefulness after leaving their trades; they collect honor and deference each anniversary. They do not run the rite day to day and bear little of its physical burden.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, communal_memory_elders, beneficiary,
    organized, generational, identity_locked, regional).

% Families who keep the observance: they fast, close shops for the anniversary, contribute to the ceremony fund, and sit children through long lament recitations. What returns to them is belonging, a container for inherited grief, and assurance that their dead are counted. Leaving the practice would mean visible dishonor before neighbors and kin, and most cannot imagine raising children outside the calendar even while privately finding the vigils exhausting.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, observant_households, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, observant_households, beneficiary).

% Children and young adults who inherit the obligation: they memorize lament sequences and the genealogy of the dead but receive no instruction in flood signs, structural triage, or evacuation — the practical content their great-grandparents' version of the rite carried. Many attend under family pressure, drift from the calendar as they move to cities, and experience the cost as lost weekends and diminished standing when they object. None of them has a voice in the order of service.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, younger_generation_members, payer,
    powerless, biographical, mobile, regional).

% A small circle of teachers, nurses, and returned migrants who argue the anniversary should carry a drill and a hazards lesson alongside the laments. They attend and contribute like everyone else while petitioning the elders for a revised order of service; petitions stall for years without formal refusal. Their standing rises when they volunteer for the ceremony and falls when they press the reform, so most alternate between the two postures.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, reform_faction_members, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, reform_faction_members, excluded).

% Regional disaster-management officers who maintain evacuation maps, warning systems, and school drill programs. Casualty modeling shows the commemorative week produces no measurable preparedness gain, and the agency has offered joint programming — a drill appended to the anniversary — but the religious calendar is administered by the officiant line and the elders, and no channel exists for the agency to table proposals there. They plan around the community rather than with it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, civil_defense_planners, excluded,
    institutional, generational, analytical, national).

% Comparative researchers of catastrophe commemoration who archive liturgical redactions, interview surviving lineages, and place this community's practice beside parallel rites elsewhere. They document which instructional verses dropped out of the canon in successive editions and publish on the widening gap between the rite's stated purpose and its content. They hold no standing in the community and bear none of its obligations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes the community on a single annual return to the catastrophe: one shared calendar slot, one shared narrative of what happened and who was lost, a public boundary between members and outsiders, and — in the founding era — a synchronized window for seasonal protective work (moving stores, inspecting embankments, rehearsing routes) that modern hazard agencies now perform continuously by other means.
% TRANSFER_FUNCTION: Moves time, fasting discomfort, ceremony contributions, and children's instruction hours from observant households to the ceremonial apparatus — funding the officiant line's stipends and the annual observance itself. In the founding era it additionally moved preparedness labor from households to shared infrastructure, a flow that now has no modern counterpart.
% ABSENT_VOICES: Civil-defense planners who could convert the commemorative week into preparedness training have no seat in liturgy decisions; younger members who bear the obligation have no vote on the order of service; and the survivors who once carried the operational knowledge were mourned rather than installed as teachers, so the practical curriculum died with them while the lament curriculum was canonized.
% DISAPPEARANCE_RATIONALE: The calendar, the officiant line's livelihood, the elders' standing, and the community's annual rhythm all depend on the observance; overnight disappearance would rupture identity and succession arrangements. Nothing adaptive would rearrange — no preparedness capacity would be lost, because on this reading none currently flows through the rite.
% FOUNDING_PROBLEM: After a catastrophic flood destroyed the settlement, the rebuilt community needed descendants who could recognize the precursors their ancestors missed, execute protective routines under stress, and rebuild again — knowledge too intricate for casual telling, so it was embedded in an obligatory annual rite of lament, rehearsal, and renewal.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: disaster-history scholarship documents the same arc in parallel rites, with operational verses dropping out of canon as state hazard agencies professionalized; successive liturgical redactions archived by ritual-studies researchers show instructional portions removed as obsolete while lament portions expanded; civil-defense casualty modeling finds no preparedness differential between participant and non-participant households. Community leadership attests continuity of meaning, not of function, and does not corroborate the founding problem's persistence.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and declining (0.62 to 0.46 across the interval): the burdens are real — fasting, long vigils, ceremony levies, children's instruction hours spent on lament rather than safety — but they shrink as dispensations multiply, liturgies shorten, and attendance voluntary-izes. Suppression (0.42 at end) tracks the same decay: communal obligation has softened from enforced duty to customary expectation, so the suppression_requirement series falls rather than rises; enforcement-capacity erosion is precisely the dynamic this story traces. Theater_ratio is high and rising (0.45 to 0.71) — the classic Goodhart signature of a proxy (performed continuity) displacing the original function (transmitted competence): each decade the lament portions expand while the last instructional verses drop from the canon. Accessibility_collapse is low (0.30): the alternatives are known and available — secular memorial ceremonies, civil-defense drills, museum curricula — and the community simply does not adopt them, which is inertia, not closure. Resistance is low (0.28): the burden is moderate enough that most comply or quietly lapse rather than fight; the reform faction petitions without traction. All three series run on one shared grid (points 0-60 by decades) with every metric authored at every point. No oscillation is modeled: the drift is monotonic decay, not a crisis-reform cycle. Identity-lock note: the officiants' and elders' identity_locked exits are institutional-identity fusion — the office has become the lineage's self-concept, so exit is unthinkable without dissolving the self that exits; a documented failure the rite failed to prevent (or a successor generation refusing ordination en masse) is the frame-break that would make reform thinkable.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the officiant and elder seats the rite is sacred continuity they personally sustain — low experienced extraction, high legitimacy. From the household and younger-generation seats it is cost without the promised payoff — higher experienced extraction with a shrinking benefit side. The scholar seat sees the atrophy arc itself; the civil-defense seat sees a vacant instructional slot it could fill. Same-level differentiation appears between elders and reform-faction members — both organized-to-moderate community insiders — separated by exit posture (identity_locked vs. constrained) and role, showing that nominal standing does not determine experienced position. Inter-institutionally, the religious line and the hazard agency hold substitute versions of the same memory function with no interface between them; the agency's exclusion from the liturgy conversation is what keeps the instructional slot vacant.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: officiants and elders sit at the subsidized end (low d, damped effective extraction) — officiants receive custom-fixed stipends, elders collect standing. Household, youth, and reform-faction declarations as victims drive them toward the target end (high d, amplified effective extraction), with households partially damped by their secondary beneficiary position (identity and grief returns) but net-targeted on this reading's judgment that the returns no longer cover the costs. One derivation limit is noted rather than overridden: the officiants' true position is nearer symmetric than a pure-beneficiary derivation prices, since their stipends are compensation for labor rendered, not rent — but the override mechanism keys on power atom, and the elders share the 'organized' atom while genuinely sitting nearer the beneficiary pole, so a per-atom override would misprice one seat to correct the other. The residual imprecision is left to the engine and flagged here. Suppression is authored as a raw structural property and is deliberately not scaled; only extractiveness rides directionality and scope in the engine's arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting survival-competence through obligatory annual practice — is dead: professionalized forecasting, engineered defenses, and state disaster agencies solved the underlying problem by other means, and the rite no longer delivers the competence it was built to carry. Yet the world rearranges socially if the rite vanishes (calendar, livelihoods, standing, succession all depend on it), producing the dead-problem x world-rearranges mismatch that flags zombie persistence — consistent with the piton path this story claims. The classification prevents two mislabels: reading the arrangement as pure extraction (snare) would require a capturer, and the receipt surface affirmatively establishes none — gains diffuse across identity and stipend-for-labor, no seat captures surplus; reading it as healthy coordination (rope) would require the identity-and-grief returns to cover household costs, which this reading judges they no longer do. What remains is the third thing: a form performing a function it no longer performs, maintained because fixing it costs its administrators their own meaning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of the catastrophe_memory_preservation kernel — the hybrid_atrophy_reading. Would instantiating a sibling reading change the computed classification?',
    'Generate the sibling stories (catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading) and compare per-seat classifications across the family; the disagreement is located in the present-state premise (does operational transfer persist?) and the origin premise (did it ever exist?).',
    'Under survival_competence_reading the rite''s costs price as training and epsilon drops toward coordination-cost levels, recomputing toward rope; under mourning_practice_reading costs are judged against symbolic payoff alone and the type recomputes away from piton. The piton verdict holds only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: classification is reading-relative within the catastrophe_memory_preservation kernel.').

omega_variable(
    functional_origin_historicity,
    'Was the ritual ever genuinely operational — did its founding form actually transmit survival-competence — or is the functional origin a retrospective projection onto a practice that was always symbolic?',
    'Archaeology of liturgical redactions (do earliest strata contain instructional content later removed?), comparative ethnography of pre-modern hazard rites, and documentary traces of embedded protective routines tied to the observance calendar.',
    'If the origin is projected, this reading loses its temporal bridge, collapses toward mourning_practice_reading, and epsilon must be re-referenced to a purely symbolic arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_origin_historicity, empirical, 'Historicity of the survival-competence origin claim — the load-bearing premise distinguishing this reading.').

omega_variable(
    atrophy_reversibility,
    'Is the lost operational content recoverable — could the rite''s instructional function be revived — or is it irreversibly gone?',
    'Knowledge-testing of current participants against reconstructed protocol elements from archived redactions; pilot programs appending drills to the anniversary week with retention measured against secular-only training.',
    'Recoverable content would register revival_pressure and trend the classification back toward a coordinated training arrangement; confirmed irreversibility locks the inertial-attractor path this story claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_reversibility, empirical, 'Whether the atrophy is terminal or a reversible dormancy.').

omega_variable(
    residual_identity_payoff_sufficiency,
    'Do the identity-continuity and grief-containment returns households actually receive sufficiently compensate the costs they bear, or is the residue net-costly?',
    'Willingness-to-keep studies under anonymized opt-out (does revealed preference survive when social sanction is removed?), longitudinal wellbeing comparison of participating vs. lapsed households, and deliberative polling inside the community.',
    'If the symbolic payoff is judged sufficient, the arrangement is a working identity-coordination mechanism and the atrophied-residue claim fails; if insufficient — this reading''s judgment — the excess cost is inertial residue sustained by nobody''s deliberate benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_identity_payoff_sufficiency, preference, 'Value question separating this reading''s verdict from the mourning_practice_reading''s.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the compliance that remains driven by structural-communal sanction (family pressure, standing losses, ceremony-fund expectation) or by internalized obligation that would persist after exit?',
    'Post-exit trajectory of emigrant households: if fasting and anniversary observance persist abroad absent communal sanction, the internalized share is substantial; if lapsing is rapid and guilt-free, suppression was structural.',
    'An internalized share means suppression outlives the community''s enforcement capacity, raising durable suppression above the structural measure; purely structural suppression decays with community cohesion, accelerating the hollowing trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized composition of the remaining compliance pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_hybrid_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cmp_hybrid_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement(cmp_hybrid_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(cmp_hybrid_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(cmp_hybrid_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.66).
narrative_ontology:measurement(cmp_hybrid_tr_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 50, 0.69).
narrative_ontology:measurement(cmp_hybrid_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.71).

% Extraction over time
narrative_ontology:measurement(cmp_hybrid_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(cmp_hybrid_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(cmp_hybrid_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(cmp_hybrid_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(cmp_hybrid_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(cmp_hybrid_be_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(cmp_hybrid_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(cmp_hybrid_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cmp_hybrid_su_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(cmp_hybrid_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(cmp_hybrid_su_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(cmp_hybrid_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(cmp_hybrid_su_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(cmp_hybrid_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, state_hazard_management_system).

% DUAL FORMULATION NOTE:
% The colloquial label 'catastrophe-memory ritual' covers three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story family sharing the kernel catastrophe_memory_preservation. survival_competence_reading (upstream: its historical claim is cited by the other two) authors low epsilon — the rite's costs price as live training and the type computes as coordination. mourning_practice_reading authors epsilon against a symbolic-payoff referent — identity and grief returns weighed against costs, no operational claim either way. This file, hybrid_atrophy_reading, is the mediating story: it accepts the upstream account of the past and the mourning account of the present, and authors epsilon (0.46, declining) against the standing arrangement judged costly-without-adaptive-payoff. Same referent, reading-indexed values. state_hazard_management_system is included as the causal neighbor whose rise drained the rite's function; contamination analysis should treat professionalization shocks there as accelerants of this story's theater_ratio trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
