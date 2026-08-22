% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission — Operational Competence Reading
 *   domain: religious/collective_memory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the annual ritual cycle through
 *   which catastrophe-experienced communities transmit disaster memory: the
 *   Passover seder with its haste elements (unleavened bread, bitter herbs,
 *   the scripted recitation of a departure performed with staff in hand), the
 *   Tisha B'Av fast with its deprivation discipline, and the memorial days
 *   layered onto the calendar as new catastrophes arrived. This story
 *   instantiates one reading of that arrangement — the
 *   operational_competence_reading — under which the cycle's function is to
 *   encode and transmit survival competence: pattern recognition, resource
 *   coordination under scarcity, and threat-assessment rehearsal. On this
 *   reading ritual elements are evaluated by operational yield; the
 *   beneficiary is future survival capacity; and the candidate victim is the
 *   participant who mistakes the symbol for the substance, carrying the
 *   performance without the readiness. ε is authored for the standing
 *   arrangement as actually practiced, assessed by this reading's own lights
 *   — not for any reformed calendar this reading would endorse. Claim and
 *   metrics are authored independently: the claim is rope (a coordination
 *   mechanism for the witness-decay collective-action problem); the metrics
 *   describe low-to-moderate extraction with a rising theatrical share — a
 *   divergence profile the engine should test rather than one the author
 *   reconciles.
 *
 * KEY AGENTS:
 *   - ritual_authorities: agenda-setting seat (institutional power, identity-locked exit) — administers the calendar, rules on observance, trains officiants
 *   - practicing_households: beneficiary seat with a cost-bearing aspect (moderate power, constrained exit) — performs the cycle and is where training either converts or fails
 *   - descendant_community: full beneficiary seat (powerless, no exit) — inherits transmitted patterns and the obligation to continue them
 *   - symbol_substitutes: primary target seat (moderate power, identity-locked exit) — performs without converting, carries false readiness
 *   - preparedness_professionals: excluded seat (organized power, mobile exit) — would evaluate operational yield, not in the conversation
 *   - collective_memory_researchers: analytical observer — documents transmission and decay across generations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.3).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.25).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission — Operational Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__operational_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '7f809200-5cf9-4ad7-aa9b-e079cb345b2b').
narrative_ontology:cs_kernel_codification('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', distributed).
narrative_ontology:cs_authority_grounding('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', practice).
narrative_ontology:cs_interpretation_layer_present('7f809200-5cf9-4ad7-aa9b-e079cb345b2b').
narrative_ontology:cs_reading_relation('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', catastrophe_memory_transmission__hybrid_embedded_reading, forecloses).
narrative_ontology:cs_axiom('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', foundational, operational_yield_evaluability).
narrative_ontology:cs_axiom_status(operational_yield_evaluability, holdable).
narrative_ontology:cs_axiom_grounding('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', operational_yield_evaluability, empirically_contingent).
narrative_ontology:cs_axiom('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', foundational, embodied_rehearsal_transmits_competence).
narrative_ontology:cs_axiom_status(embodied_rehearsal_transmits_competence, holdable).
narrative_ontology:cs_axiom_grounding('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', embodied_rehearsal_transmits_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', haste_readiness_rehearsal_regime).
narrative_ontology:cs_drift_state('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', post_witness_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f809200-5cf9-4ad7-aa9b-e079cb345b2b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, practicing_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, descendant_community).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, symbol_substitutes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, practicing_households).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, embodied_rehearsal_transmits_competence).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_decays_without_synchronized_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and maintain the ritual calendar: fix the liturgy, rule on which commemorations are obligatory, decide how each new catastrophe enters the cycle, and train the next generation of officiants. Deference, institutional continuity, and the vocation itself flow to them through the calendar they administer; stepping outside it would cost them the standing the role confers, so leaving the arrangement would mean leaving the vocation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Prepare and perform the annual cycle — the seder with its haste script, the fast days, the memorial observances. They spend real time, money, and caloric deprivation on the rehearsals, and they are the seat where the training either converts into readiness or does not. Reducing observance to the minimum or drifting into secularity remains open to them, but it cuts them off from parents, community, and the schooling and marriage networks their children depend on.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, practicing_households, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, practicing_households, payer).

% The children and the not-yet-born who will inherit whatever memory structure the present maintains. The transmitted patterns and the duty to continue them both arrive unchosen; what they inherit cannot be returned, and their situation is fixed entirely by decisions made before they could consent.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, descendant_community, beneficiary,
    powerless, generational, trapped, global).

% Participate fully, often beautifully, in the performances but never convert them into readiness: no household plan, no resource buffer, no practiced response — while carrying the conviction that performing the memory is the same as holding it. The performance is fused with who they understand themselves to be — the people who remember — so confronting the gap would cost them their self-conception rather than merely a habit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbol_substitutes, payer,
    moderate, biographical, identity_locked, global).

% Disaster researchers and emergency-management practitioners who study what actually preserves readiness in populations. They sit outside the ritual conversation and are rarely consulted on whether annual rehearsals produce transferable skill; they would ask for scenario realism, measured response times, and comparison groups. Nothing binds them to the arrangement and they can take their questions anywhere.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, preparedness_professionals, excluded,
    organized, biographical, mobile, global).

% Historians and social scientists of collective memory who track how communities retain or lose catastrophe knowledge across generations. They document the decay curve of witness memory and the mechanics of transmission without administering or performing the cycle, and their findings are equally available to every other seat.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, collective_memory_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the witness-decay problem: direct experience of catastrophe fades within roughly two generations, single households cannot sustain preparedness drills alone, and the operative knowledge — departure haste, scarcity discipline, escalation recognition — survives only if rehearsed on a synchronized calendar. The ritual cycle is that synchronized schedule, maintained at communal scale.
% TRANSFER_FUNCTION: Moves time, attention, and embodied effort from present community members into rehearsed patterns carried by future members; moves resources into scarcity-discipline practices (fasts, unleavened bread) that train deprivation tolerance; and converts each new catastrophe's fresh witness memory into repeatable liturgical form.
% ABSENT_VOICES: Preparedness professionals and disaster researchers are wholly outside the conversation and would demand measured evidence of transfer before crediting the rehearsals. Secular descendants who left the cycle would object that the obligation was imposed without consent and its costs never opened to negotiation. The symbol-substitute seat is present in body but voiceless in the arrangement: nothing in the cycle asks performers whether the rehearsal trained them, because asking would surface the gap between performing the memory and holding the readiness.
% DISAPPEARANCE_RATIONALE: The texts and archives would remain, but the synchronized embodied rehearsal would be gone: within about two generations the departure script, the scarcity discipline, and the escalation patterns would decay into literature, household readiness would fall entirely to state and market provision, and the calendar that structures communal time and the intergenerational handoff would dissolve into ordinary commemoration.
% FOUNDING_PROBLEM: Recurring catastrophes — expulsion, destruction, flight — write operational lessons deep into their witnesses: depart in haste, travel light, expect scarcity, read escalation early. Direct experience fades within about two generations, and the arrangement was built to keep those lessons operative in bodies and calendars after the last witness died.
% FOUNDING_PROBLEM_CORROBORATION: Historians of collective memory outside the tradition (the Zakhor line of scholarship) attest the witness-decay problem from an academic seat; disaster-sociology research attests that rehearsal sustains readiness in secular populations; and advocates of the symbol-continuity reading — outside this reading's beneficiary frame — attest the transmission problem while disputing its mechanism. No source outside the tradition attests that the specific operational mandate (departure readiness, scarcity discipline) remains live in secure host contexts; that liveness is asserted mainly from within, on recurrence history.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.30) is low-to-moderate because, on this reading's own lights, most of the arrangement's cost is the training itself — the fast is the scarcity rehearsal and the seder's haste script is the departure drill — so only the commemorative elaboration that trains nothing, the obligation on the unconsenting, and the false-confidence harm count as extraction above coordination cost. Suppression (0.25) is a raw structural property, unscaled by power or scope: observance is socially enforced and partly internalized in roughly equal measure, but written records, secular preparedness training, and exit into secularity remain open, so alternatives are not suppressed. Theater_ratio (0.30) is the commemorative share: narrative and elegiac segments dominate the calendar's center while the operational segments have thinned to a minority. Accessibility_collapse (0.25) is low — manuals, archives, and civil-defense drills perform the same function by other means — which is disqualifying for any natural-law reading and consistent with a constructed coordination mechanism. Resistance (0.30) is moderate-low: dropout, liturgical fatigue, and recurring why-do-we-still-do-this disputes are real but marginal inside practicing communities. The three measurement series share one six-point grid across the interval (t=0..80, approximately 1945–2025): extractiveness and theater rise as the witness generation dies and commemorative layers accumulate, and suppression_requirement rises in parallel because the arrangement shifted from self-enforcing traumatic urgency to built-up institutional enforcement as urgency decayed. Coordination type is information_standard because this reading's dominant function is standardizing and scheduling rehearsal of survival procedures — a protocol maintained by communal practice; identity_coordination would be the symbol reading's choice and is not this reading's claim. Receipt surface: the arrangement's costs convert into diffuse household and descendant competence and no named seat captures them, so gain_flow is 'diffuse' as an affirmative checked claim; yield-oriented reform of the calendar is prohibitive for the distributed, identity-fused authority that would have to attempt it against custom and dispersed adjudication, so fixing_cost is 'prohibitive'.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat experiences the arrangement as an inheritance it stewards: from inside the authority role the calendar is the community's continuity, and questioning it is self-undermining. The practicing household experiences cost that mostly converts — rehearsal that pays in competence — and so sits near the beneficiary end despite bearing the calendar's full price. The symbol-substitute seat experiences the same performances as pure cost: time and deprivation purchased at par and redeemed for nothing, plus the harm that the performance certifies a readiness that does not exist. The descendant seat cannot experience the arrangement at all yet — it is the arrangement's product. One nominal community, four different effective arrangements; the engine computes the per-seat divergence from power, exit, and declared position, and the divergence here is driven almost entirely by conversion — whether rehearsal becomes competence is the single variable separating the payer seat from the beneficiary seats. Identity-lock operates differently at the two locked seats: for ritual_authorities it is institutional (the vocation has become the role); for symbol_substitutes it is relational-ideological (self-concept constituted through being the people who remember). If either frame broke, the authority seat could reform the calendar and the substitute seat would convert or exit, and both seats' classifications would shift.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: practicing_households receive the training they pay for (d near the beneficiary end; extraction damped) and descendant_community receive transmitted capacity at zero present cost (full beneficiary end; χ damped toward subsidy). Victim declaration: symbol_substitutes bear the full calendar cost, receive no competence, and carry the false-confidence harm (full target end; χ amplified), with identity-locked exit holding them at the trapped end of the exit modulation. Ritual authorities sit near the beneficiary end on the derivation — they are community members who train too — while their enforcement role and identity-locked vocation are what hold the calendar in place. No directionality override is authored: the beneficiary/victim data plus exit options already produce the right relationships, and a power-atom-level override could not distinguish the two moderate-power seats whose directionality genuinely differs (practicing_households versus symbol_substitutes) — the structural declarations, not overrides, carry that distinction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping catastrophe lessons operative after the witnesses die — recurs with each catastrophe, which is why the arrangement is not declared mandatrophy-resolved: the mandate renews (new kinot, new memorial days) even as any single mandate goes latent in secure host contexts. The rope classification matters in both directions. It prevents mislabeling the arrangement as pure extraction: the costs are mostly the coordination good itself, and reading the fast as pure imposition would erase the scarcity-training function. It equally prevents mislabeling it as costless coordination: the theater_ratio series tracks the specific decay mode available to this arrangement — as witnesses die and contexts secure, the rehearsal share shrinks toward performance, and an arrangement that coordinates today can degrade toward inert performance (theater without yield, diffuse gains, prohibitive reform) without any seat deciding anything. The mismatch consumer should watch founding_problem_status=contested against the rising theater series: if the operational-yield omega resolves negatively, the arrangement's persistence becomes inertia plus identity rather than function, and the classification should be re-run.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the operational_competence_reading of the kernel catastrophe_memory_transmission; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'The disagreement is located in one structural element: whether ritual''s symbolic form and its operational content are separable such that elements can be evaluated (kept, redesigned, discarded) by independent operational yield. The symbol_continuity_reading denies that operational content is the point at all; the hybrid_embedded_reading denies separability outright. Comparative classification of the sibling stories against this one locates the disagreement empirically: whichever framing communities actually invoke to justify reform or retention reveals the operative reading.',
    'Under symbol_continuity the symbol_substitutes victim class dissolves (symbol IS the substance, so no one mistakes it) and the arrangement becomes identity coordination with a different beneficiary set; under hybrid_embedded element-level yield evaluation is incoherent and this reading''s coordination-function grounding fails. This story''s classification is valid only within this reading''s separability premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the catastrophe-memory kernel; sibling readings would restructure the beneficiary and victim sets.').

omega_variable(
    operational_yield_verifiability,
    'Does participation in the catastrophe-ritual cycle actually produce measurable preparedness differentials — faster evacuation, better resource buffering, sharper threat recognition — relative to comparable communities without the cycle?',
    'Disaster-sociology and community-resilience studies comparing observant and non-observant populations with matched socioeconomic profiles on realized catastrophe responses; natural experiments where the same population''s response is observable across generations of ritual participation.',
    'If no differential exists, this reading''s core claim fails: the arrangement''s costs buy no competence, the beneficiary declaration collapses toward the symbol reading, and the story reclassifies toward identity maintenance with pure overhead. If a differential exists, the coordination-function claim is corroborated and the rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_yield_verifiability, empirical, 'Whether ritual participation yields measurable survival-competence differentials.').

omega_variable(
    false_confidence_displacement,
    'Does symbolic mastery of the catastrophe scripts actively displace operational preparation (a false-confidence mechanism), or does it merely fail to produce preparation?',
    'Household-level studies correlating ritual participation intensity with concrete preparedness indicators (supplies, plans, drills) among participants with low competence conversion; interview studies of what participants believe the rehearsal trained them to do.',
    'If displacement is real, symbol_substitutes are genuine targets of the arrangement (the performance suppresses the preparation it replaces) and the victim declaration stands with elevated extraction; if the effect is mere absence of benefit, the victim declaration should be withdrawn and the arrangement moves toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_confidence_displacement, empirical, 'Whether symbolic performance actively suppresses the readiness it simulates.').

omega_variable(
    unconsented_inheritance,
    'Is the obligation imposed on descendant_community — future members who inherit both the transmitted competence and the duty to continue the cycle without having chosen either — a transfer to them or a cost imposed on them?',
    'No empirical resolution is available; the question turns on whether inheritance of communal obligation counts as consent-by-membership. Resolution would come from the tradition''s own frameworks for intergenerational obligation (covenantal consent doctrines) weighed against exit-cost evidence for those who try to decline the inheritance.',
    'If intergenerational imposition counts as a cost borne without consent, descendant_community carries a payer aspect and the arrangement drifts toward hybrid coordination/extraction; if inheritance counts as the good itself, the beneficiary declaration stands unqualified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unconsented_inheritance, conceptual, 'Whether unchosen intergenerational obligation is receipt or imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t48, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 48, 0.25).
narrative_ontology:measurement_basis(cata_tr_t48, observed).
narrative_ontology:measurement(cata_tr_t64, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 64, 0.28).
narrative_ontology:measurement_basis(cata_tr_t64, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement_basis(cata_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 32, 0.23).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t48, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 48, 0.25).
narrative_ontology:measurement_basis(cata_be_t48, observed).
narrative_ontology:measurement(cata_be_t64, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 64, 0.28).
narrative_ontology:measurement_basis(cata_be_t64, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement_basis(cata_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 16, 0.17).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 32, 0.2).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t48, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 48, 0.22).
narrative_ontology:measurement_basis(cata_su_t48, observed).
narrative_ontology:measurement(cata_su_t64, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 64, 0.24).
narrative_ontology:measurement_basis(cata_su_t64, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 80, 0.25).
narrative_ontology:measurement_basis(cata_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, information_standard).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'catastrophe memory transmission' conflates structurally distinct claims about what ritual transmits. This file authors the operational_competence_reading only: transmission content is survival competence, evaluated element-by-element by operational yield, with ε assessed for the standing ritual arrangement as this reading sees it. The symbol_continuity_reading (symbolic form as the survival mechanism; identity as the good) and the hybrid_embedded_reading (competence inseparable from symbolic form) are separate constraints with their own ε, beneficiary sets, and victim sets, linked here as family members. The readings disagree on one structural element — separability of form from content — and this reading's victim class exists only if they are separable; upstream empirical confidence about witness-memory decay is shared across the family, while downstream classification diverges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
