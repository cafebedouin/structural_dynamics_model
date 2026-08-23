% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Catastrophe Memory Transmission - Hybrid Embedded Reading (Form-Function Co-Constitution)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   Composite scenario drawn from ethnographic cases of catastrophe-encoding
 *   ritual: coastal villages whose annual observances rehearse surge-season
 *   movement past tsunami marker stones, fire-regime cultures whose seasonal
 *   burning liturgies maintain landscape-scale fuel management, famine-memory
 *   festivals that train grain storage and famine-food preparation, and
 *   epidemic-era purification calendars that encode quarantine spacing and
 *   water discipline. The hybrid_embedded_reading asserts that survival
 *   competence is encoded within symbolic form and the two are inseparable:
 *   the enacted sequence is not a container for the knowledge but its
 *   substance, so altering form degrades function, and the function exists
 *   only insofar as the form is enacted. The standing arrangement under
 *   assessment is the fidelity demand itself - the community's requirement
 *   that the sequence be performed faithfully on calendar - and epsilon is
 *   authored for that arrangement as this reading sees it, not for any
 *   documentary substitute this reading rejects. KEY AGENTS (by structural
 *   relationship): - ritual_practicing_communities: principal beneficiary
 *   body (organized/constrained) - bears the rehearsal calendar and receives
 *   the transmitted repertoire - ritual_officiants_and_elders: administrator
 *   and concentrated status-receipt seat (organized/identity_locked) - sets
 *   fidelity standards and collects interpretive authority -
 *   apprentice_cohorts: deferred beneficiary (moderate/constrained) - pays
 *   learning labor now, collects competence later - descendant_generations:
 *   ultimate intended recipient with no voice (powerless/trapped, secondary
 *   excluded) - inherits the outcome of upkeep decisions made without them -
 *   hazard_exposed_neighbor_settlements: incidental beneficiary
 *   (moderate/constrained) - protected by neighbors' practiced responses
 *   without being bound by them - skeptical_reform_members: cost-bearing seat
 *   (moderate/mobile) - shoulders fidelity burdens while doubting the payoff;
 *   exit is open - comparative_ritual_scholars: analytical observer
 *   (analytical/analytical) - tests the transmission claim against
 *   cross-tradition evidence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.3).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission - Hybrid Embedded Reading (Form-Function Co-Constitution)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'fadb2af4-0796-4842-b7da-98e96e4b317a').
narrative_ontology:cs_kernel_codification('fadb2af4-0796-4842-b7da-98e96e4b317a', distributed).
narrative_ontology:cs_authority_grounding('fadb2af4-0796-4842-b7da-98e96e4b317a', distributed).
narrative_ontology:cs_reading_relation('fadb2af4-0796-4842-b7da-98e96e4b317a', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('fadb2af4-0796-4842-b7da-98e96e4b317a', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_axiom('fadb2af4-0796-4842-b7da-98e96e4b317a', foundational, form_function_inseparability).
narrative_ontology:cs_axiom_status(form_function_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('fadb2af4-0796-4842-b7da-98e96e4b317a', form_function_inseparability, empirically_contingent).
narrative_ontology:cs_axiom('fadb2af4-0796-4842-b7da-98e96e4b317a', foundational, enacted_knowledge_acquisition_requirement).
narrative_ontology:cs_axiom_status(enacted_knowledge_acquisition_requirement, holdable).
narrative_ontology:cs_axiom_grounding('fadb2af4-0796-4842-b7da-98e96e4b317a', enacted_knowledge_acquisition_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('fadb2af4-0796-4842-b7da-98e96e4b317a', coconstituted_form_function_whole).
narrative_ontology:cs_drift_state('fadb2af4-0796-4842-b7da-98e96e4b317a', contemporary_documentation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fadb2af4-0796-4842-b7da-98e96e4b317a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practicing_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants_and_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, apprentice_cohorts).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, descendant_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, hazard_exposed_neighbor_settlements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, skeptical_reform_members).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, apprentice_cohorts).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, skeptical_reform_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_knowledge_transmission_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_form_function_coconstitution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that gather on a fixed calendar to enact the inherited sequence - processions, fasts, recitations, seasonal labors. Through repeated joint performance each cohort absorbs hazard-sign reading, food and water discipline, mutual-aid routines, and settlement-siting habits that no member could state fully as explicit rules. Leaving the calendar would not be forbidden; it would simply dissolve the shared occasions on which the knowing lives, and with it the competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practicing_communities, beneficiary,
    organized, generational, constrained, regional).

% Elders and officiants schedule the observances, correct deviations in gesture and wording, and decide what counts as faithful performance. Decades of holding the sequence give them the deepest command of its practical content, and their standing in the community rests on being its indispensable custodians - they collect interpretive authority and social honor from that position. Stepping aside would cost them the role through which they are known.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants_and_elders, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants_and_elders, beneficiary).

% Children and young adults learn the sequence by doing it beside elders - carrying, chanting, cooking, reading weather with trained eyes. The years of instruction demand real labor from them before any payoff arrives, and leaving the community would forfeit both the competence and the belonging it travels with. What they gain is theirs to keep and later to teach.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, apprentice_cohorts, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, apprentice_cohorts, payer).

% People not yet born who will inherit either a working repertoire of survival practice or a gap where it used to be, depending on whether the chain of performance holds through the coming decades. They take part in no deliberation about the calendar's upkeep; they arrive after the decisions that shape what they receive, and they cannot decline the inheritance either way.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, descendant_generations, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, descendant_generations, excluded).

% Settlements near the practicing communities that are not themselves bound to the calendar but stand downwind, downstream, or along the same coast. Their safety rides partly on the neighbors' timely seasonal burns, flood-season movements, and stored-grain customs. They have no seat in deciding how faithfully those observances are kept, and relocating away from the hazard zone is costly and often impossible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, hazard_exposed_neighbor_settlements, beneficiary,
    moderate, biographical, constrained, regional).

% Members who attend out of family and neighborly obligation while doubting that the old sequence carries anything a well-written manual could not convey more cheaply. They shoulder the hours and expense of observance with little felt return, though they still eat from the granaries and shelter under the siting customs the practice maintains. Unlike their grandparents they can imagine life outside the tradition, and some have already moved away.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, skeptical_reform_members, payer,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, skeptical_reform_members, beneficiary).

% Researchers who compare transmission practices across dozens of traditions and catastrophe settings, tracking which communities kept hazard competence through upheaval and which lost it. They test claims about embodied transmission against archival and field evidence and publish outside any practicing community's control; their livelihood does not depend on any single tradition remaining intact.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants_and_elders).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves intergenerational transmission of survival-critical knowledge that resists propositional storage: hazard sign-reading, seasonal timing, food and water discipline, mutual-aid choreography, settlement siting. The annual cycle schedules rehearsal so each cohort acquires the repertoire by performing it beside elders, keeping the knowledge distributed across bodies rather than dependent on any archive.
% TRANSFER_FUNCTION: Moves rehearsal labor, attention, and material resources from every cohort member into a jointly enacted archive; moves practical competence from elder performers to apprentices; moves interpretive authority and standing to those who administer fidelity; and moves protection forward in time to members and neighbors not yet born or not themselves bound.
% ABSENT_VOICES: Descendant generations - the arrangement's principal intended recipients - cannot object, consent, or redirect what they will inherit. Emigrants and lapsed members who lost access to the competence are likewise outside the room, as are neighboring settlements exposed to the consequences of others' observance without a seat in its upkeep.
% DISAPPEARANCE_RATIONALE: If every community stopped enacting the sequence overnight, the encoded repertoire would begin decaying immediately: no document carries the weather-reading, the timing judgments, the bodily routines. Within a generation or two, settlements would meet floods, fires, and famines with improvised guesswork where drilled response used to operate, and mortality in the next comparable event would register the gap. Reform-minded members reply that documented curricula could replace the calendar; the embodied-transmission evidence so far says otherwise, which is why the verdict is rearrangement rather than indifference - the dispute itself is logged in the omegas.
% FOUNDING_PROBLEM: Repeated catastrophes - floods, storm surges, wildfires, famines, epidemics - erased communities that failed to carry hazard-response and recovery practice across generations. Written instruction was unavailable to most of humanity through most of history, and the needed knowledge was largely perceptual, situational, and bodily: it could not be fully said, only shown and rehearsed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: hazard historians and disaster ethnographers document cases where ritualized warning and response preserved settlements (marker stones and annual drill observances preceding recorded surges; fire-regime cultures whose seasonal burning is validated by fire ecology); geomorphology and sediment records corroborate the events the observances encode; and archival mortality series attest the losses where transmission chains broke. No attesting source depends on the traditions' own standing.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is rope because the arrangement solves a genuine collective-action problem - intergenerational transmission of knowledge that cannot be fully propositionalized - with minimal coercive overhead and no identifiable victim while the practice holds; the metrics describe what the arrangement actually costs and how it is drifting, without being tuned to land anywhere. Extractiveness is 0.30: fidelity imposes real costs (hours, materials, foregone flexibility, the rigidity penalty when environments shift faster than liturgies) but purchases a durable good, so net extraction is low-to-moderate and rising only as felt benefit thins. Suppression is 0.38 as a raw structural property, unscaled by power or scope: enforcement is social rather than coercive machinery - expectation, correction of gesture, family obligation, mild sanction - and the rising suppression_requirement series is authored deliberately because the narrative tracks enforcement-capacity change: as voluntary participation eroded under modernization, fidelity demands hardened. Theater_ratio is 0.36 and rising: under this reading most enactment is functional by construction (the performance IS the archive), but a commemorative shell accumulates where environmental pressure has relaxed, growing the purely memorial share. Accessibility_collapse is 0.74 - high, reflecting the mountain substrate running through the arrangement: once it is understood that perceptual-motor and situational knowledge fails to survive documentation, the documentary/schooling alternatives collapse on natural grounds, not engineered ones; the arrangement is nonetheless a constructed human coordination solution resting on that substrate, so emerges_naturally is false and no mountain claim is made. Resistance is 0.28: skeptic members and modernizer factions supply real but limited friction; most compliance is willing.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently from the same structure. From the officiants' position the arrangement is near-pure coordination: they experience duty, depth of command, and accumulated standing, and the status rent they collect reads as fair return on instructional labor. From the skeptical members' position the same calendar operates as cost without felt return - an extraction-flavored experience despite their lifetime net benefit - and their open exit option is exactly what differentiates them from equally-placed devout members at the same power level. Apprentices experience deferred exchange: cost now, competence later, with the discount rate set by whether the next catastrophe arrives within their biography. Descendant generations occupy the sharpest gap of all: the seat the entire arrangement exists to serve has no experience available and no voice; the arrangement's principal beneficiary computes nothing and consents to nothing. Scholars, with analytical exit, classify what practitioners live.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (practicing communities, officiants, apprentices, descendants, exposed neighbors) all derive directionality toward the subsidized end: the arrangement transfers competence to them and they bear its costs only as participants. Officiants are dual-positioned - they administer fidelity and collect the arrangement's one concentrated gain (interpretive authority and standing), which keeps them near the beneficiary pole while giving the receipt surface its named seat. Skeptical reform members are the wrinkle: they bear disproportionate cost-side weight but are deliberately NOT declared victims, because over a lifetime they remain net beneficiaries and the rope structure has no victim while practice holds; the derivation may not fully capture their experienced asymmetry, and a blanket power-atom override was rejected as less truthful than leaving the aggregate derivation intact. Descendants sit at maximal subsidy with zero agency - the directionality the engine computes for them is favorable, but no d-value can represent the absence of voice. Suppression remains a raw unscaled property throughout; only extractiveness is context-scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - catastrophes erase communities that fail to carry hazard practice across generations - remains live wherever cyclic hazards recur, so founding_problem_status is live and pairs with world_rearranges: no mismatch flag, no zombie signature. The classification prevents mislabeling in both directions. Reading the fidelity demand as pure extraction ignores that its costs purchase an irreplaceable embodied archive; reading it as timeless pure coordination ignores the accumulating commemorative shell visible in the theater_ratio series. The theater series is therefore the story's early-warning instrument: if the commemorative share crosses majority, persistence becomes inertial and the degraded-inertia questions open - at which point the mandate-outlived-function declaration would become honest. No mandatrophy_resolved flag is declared now because the mandate has not been outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This file instantiates only the hybrid_embedded_reading of kernel catastrophe_memory_transmission; what would the sibling readings (symbol_continuity_reading, operational_competence_reading) change structurally if instantiated?',
    'Generate the sibling stories as separate epsilon-invariant files and compare beneficiary structures, substitutability parameters, and computed types; the disagreement is located in whether symbolic form is constitutive of the transmitted capacity or merely its carrier.',
    'Under symbol_continuity_reading the operational referent drops out entirely: beneficiaries become the identity-communal body, epsilon attaches to commemorative goods, and classification turns on the value of mourning and continuity practice. Under operational_competence_reading form is severed from function: documentation and drilling become viable substitutes, accessibility_collapse falls sharply, and redesign pressure rises. This reading''s co-constitution claim is the middle position and produces the family''s highest accessibility_collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and disagreement location.').

omega_variable(
    embodiment_necessity_status,
    'Is enacted practice the only reliable transmission channel for this class of survival knowledge (a natural limit on how tacit knowledge propagates), or a contingent artifact of pre-documentary technology that simulation and immersive media will eventually close?',
    'Controlled longitudinal comparison of apprenticeship-style rehearsal versus high-fidelity simulation and documented curricula for hazard-response, timing-judgment, and food-discipline skills, measuring retention under stress.',
    'If technology closes the gap, accessibility_collapse falls sharply, the practice''s monopoly on transmission erodes, and the arrangement drifts toward a transitional classification whose justification expires with the technology gap; if enactment remains necessary, the low-substitutability reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodiment_necessity_status, empirical, 'Whether the enactment requirement is a natural limit or a technological artifact.').

omega_variable(
    performance_attribution_ambiguity,
    'When observers (including scholars) perceive ritual segments as mere performance, is that observer error (function present but invisible to outsiders) or accurate detection of segments whose operational content has already thinned?',
    'Competency testing of practitioners against matched non-participants after comparable exposure, segment by segment: segments where practitioners outperform reveal invisible function; segments with no differential confirm theatrical residue.',
    'Misattributing function as theater inflates theater_ratio and invites a degraded-inertia misdiagnosis; confirming genuine theatrical residue dates the onset of drift and identifies which segments are load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_attribution_ambiguity, empirical, 'Observer-relative theater attribution versus actual functional decay.').

omega_variable(
    fidelity_pressure_internalization,
    'Is the rising enforcement pressure around fidelity structural (revived sanction machinery, attendance expectation, family obligation) or internalized (guilt, duty, and identity fusion that persist independently of external sanction)?',
    'Post-exit trajectory study of disaffiliated members: if duty-bound guilt and calendar-habituation persist after all external pressure is removed, a substantial internalized share is established.',
    'An internalized share means the scalar suppression measure understates the force members actually carry with them, and exit costs are higher than the structural picture suggests; a purely structural profile means enforcement would relax quickly if sanction machinery were withdrawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_pressure_internalization, empirical, 'Structural versus internalized component of fidelity enforcement.').

omega_variable(
    discontinuity_victim_emergence,
    'No victim group is declared while the practice holds; do victims crystallize only upon discontinuation - the people who would have inherited the competence and did not?',
    'Study communities after transmission-chain breaks: identify who bore the losses at the next catastrophe, and whether anyone could have articulated that objection ex ante or whether the decision was made entirely by the departing generation.',
    'A discontinuation event retroactively supplies a victim set and forces asymmetric-extraction analysis onto whoever chose discontinuation on behalf of the unconsulted; until then the arrangement remains victim-free and coordination-dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discontinuity_victim_emergence, conceptual, 'Conditional victimhood: whether harm appears only when practice lapses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmt_hybrid_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cmt_hybrid_tr_t5, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(cmt_hybrid_tr_t10, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(cmt_hybrid_tr_t15, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(cmt_hybrid_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(cmt_hybrid_tr_t25, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(cmt_hybrid_tr_t30, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 30, 0.36).

% Extraction over time
narrative_ontology:measurement(cmt_hybrid_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(cmt_hybrid_be_t5, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(cmt_hybrid_be_t10, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(cmt_hybrid_be_t15, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(cmt_hybrid_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(cmt_hybrid_be_t25, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(cmt_hybrid_be_t30, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 30, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cmt_hybrid_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(cmt_hybrid_su_t5, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(cmt_hybrid_su_t10, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(cmt_hybrid_su_t15, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(cmt_hybrid_su_t20, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(cmt_hybrid_su_t25, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(cmt_hybrid_su_t30, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family for kernel catastrophe_memory_transmission, decomposed per the epsilon-invariance principle into three stories with distinct epsilon values. This hybrid_embedded_reading authors the middle epsilon: form-function co-constitution yields the family's highest accessibility_collapse (alternatives fail by nature, not by enforcement) and a single mild capture seat (officiant status rent) atop a predominantly diffuse benefit flow. symbol_continuity_reading drops the operational referent entirely - its epsilon attaches to identity and mourning goods with a different beneficiary structure. operational_competence_reading keeps the survival referent but treats competence as rehearsable content, raising substitutability and lowering accessibility_collapse. Edges: this reading coexists_with the symbol reading and influences the operational reading (fidelity findings constrain its redesign program). Family members link through affects_constraints; neither sibling is described inside this story's constraint proper.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
