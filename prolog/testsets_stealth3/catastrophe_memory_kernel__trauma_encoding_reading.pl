% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Mourning-Ritual Trauma Transmission as Intergenerational Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the catastrophe-memory kernel:
 *   mourning-practice understood as a mechanism that encodes
 *   intergenerational trauma and deploys it as a collective warning system.
 *   The standing arrangement under contest — the ε referent — is the existing
 *   mandatory transmission complex (calendar obligations, liturgy, communal
 *   pedagogy) assessed by this reading's own lights: it genuinely preserves
 *   threat-recognition capacity no single lifetime could rebuild, and it
 *   installs psychological burden in descendants who never consented to
 *   receive it. Beneficiary is the threat-vigilance capacity held by the
 *   descendant community (plus the leadership that administers transmission);
 *   victim is the descendant who carries the burden. KEY AGENTS (by
 *   structural relationship): - communal_ritual_leadership: Agenda-setter and
 *   concentrated beneficiary (institutional/identity_locked) — administers
 *   transmission, captures authority-rents - descendant_community_collective:
 *   Primary beneficiary (organized/constrained) — holds the early-warning
 *   asset - descendant_generation_children: Primary target
 *   (powerless/trapped) — bears formation-cost before consent -
 *   trauma_burdened_adult_descendants: Target with partial benefit
 *   (moderate/identity_locked) - assimilation_inclined_members: Excluded
 *   objectors (moderate/constrained) - trauma_transmission_researchers:
 *   Analytical observer (institutional/analytical) -
 *   historical_catastrophe_victims: Invoked non-agent authority (agent:
 *   false) The claimed_type (tangled_rope) and the metrics are authored
 *   independently: the claim states my structural belief that a real
 *   coordination function and a real extraction run through the same
 *   practice; the metrics state what I take to be descriptively true of its
 *   operation. Where the engine's computed per-seat types diverge from the
 *   claim, that divergence is the datum.
 *
 * KEY AGENTS:
 *   - communal_ritual_leadership: agenda-setter and concentrated beneficiary (institutional/identity_locked) — administers transmission, captures authority-rents
 *   - descendant_community_collective: primary beneficiary (organized/constrained) — holds the early-warning asset
 *   - descendant_generation_children: primary target (powerless/trapped) — bears formation-cost before consent
 *   - trauma_burdened_adult_descendants: target with partial benefit (moderate/identity_locked)
 *   - assimilation_inclined_members: excluded objectors (moderate/constrained)
 *   - trauma_transmission_researchers: analytical observer (institutional/analytical)
 *   - historical_catastrophe_victims: invoked non-agent authority (agent: false)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.55).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Mourning-Ritual Trauma Transmission as Intergenerational Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1').
narrative_ontology:cs_kernel_codification('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', fixed_text).
narrative_ontology:cs_authority_grounding('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', lineage).
narrative_ontology:cs_interpretation_layer_present('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1').
narrative_ontology:cs_reading_relation('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', foundational, transmitted_pain_is_functional_warning).
narrative_ontology:cs_axiom_status(transmitted_pain_is_functional_warning, holdable).
narrative_ontology:cs_axiom_grounding('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', transmitted_pain_is_functional_warning, empirically_contingent).
narrative_ontology:cs_axiom('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', foundational, remembrance_duty_precedes_descendant_consent).
narrative_ontology:cs_axiom_status(remembrance_duty_precedes_descendant_consent, holdable).
narrative_ontology:cs_axiom_grounding('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', remembrance_duty_precedes_descendant_consent, deontological).
narrative_ontology:cs_reference_frame('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', intact_warning_inheritance_transmission).
narrative_ontology:cs_drift_state('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', clinical_trauma_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f72e14e3-72f5-45f9-9a37-7b13b7b9f8b1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community_collective).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, communal_ritual_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generation_children).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, trauma_burdened_adult_descendants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, assimilation_inclined_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, never_again_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the mourning calendar, edits the liturgy, and decides how intensively catastrophe history is taught in schools and from the pulpit. Draws standing, livelihood, and moral authority from administering the transmission; is personally descended from the catastrophes commemorated and observes the practices they require of others. Stepping away would mean losing vocation, communal standing, and the frame that organizes a life's work all at once.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, communal_ritual_leadership, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, communal_ritual_leadership, beneficiary).

% Holds, as a body, what the transmission maintains: documented pattern-recognition about how persecutions escalate, mutual-aid networks drilled by remembered emergencies, and a shared playbook for flight and resistance that no single member could reconstruct alone. The asset exists only while transmission continues; individuals can leave the community, but the body cannot relocate its memory somewhere else.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community_collective, beneficiary,
    organized, generational, constrained, global).

% Are formed by the practices years before they could weigh them: catastrophe narratives presented as personal inheritance, attendance at commemorations, household vigilance habits, and the expectation that they will one day transmit in turn. The formation lands during the years when identity is built; by adulthood it is constitutive rather than chosen.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generation_children, payer,
    powerless, biographical, trapped, regional).

% Carry the transmitted load in adult life: watchfulness tuned to persecution signals, anxiety organized around historical catastrophe, and a self-concept threaded with inherited loss. They also receive what the transmission preserves — the recognition patterns and mobilization templates — so benefit and burden arrive in the same package; the burden was installed without their consent and persists even in long stretches of safety.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_burdened_adult_descendants, payer,
    moderate, biographical, identity_locked, global).

% Would prefer thinner commemoration — private remembrance, therapeutic processing, civic memorial — and read the warning-framing as turning grief into obligation. Inside communal deliberation their position registers as disloyalty to the dead or assimilationist drift, so it shapes practice mainly through exit and attrition rather than argument. They bear the same transmitted formation while holding no seat where the objection counts.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, assimilation_inclined_members, excluded,
    moderate, biographical, constrained, national).

% Study transmission directly: survivor-offspring cohorts, stress-marker studies, outcomes of differing commemoration regimes. They publish outside the community's authority structure, hold no role in setting practice, and supply the main external evidence on both the burden and any protective effect.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_transmission_researchers, observer,
    institutional, generational, analytical, global).

% The murdered generations whose memory the practices invoke. They collect nothing from the arrangement; this entry marks the invocation itself — leadership speaks in their name when defending transmission intensity — and is excluded from all derivation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, historical_catastrophe_victims, beneficiary,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, historical_catastrophe_victims).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, communal_ritual_leadership).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a cross-generational knowledge-preservation problem: persecution escalations outpace any single lifetime's learning, so each generation hands the next a pre-built recognition of escalation patterns, mobilization templates, and the emotional salience that keeps the knowledge operative under stress.
% TRANSFER_FUNCTION: Moves formation-cost forward in time — each generation's childhood identity is shaped around catastrophe before consent — and moves status and interpretive authority to those who administer the transmission; the vigilance asset itself moves forward to whoever inherits it.
% ABSENT_VOICES: Descendant children (future inheritors cannot attend the deliberations that set what they will receive), assimilation-inclined members whose thinner-commemoration preference is heard as betrayal, and clinicians who would argue for consent-based memory work. Their common objection — that warning-value does not license pre-consent formation — rarely enters the forums where practice intensity is set.
% DISAPPEARANCE_RATIONALE: Within a generation or two the standing early-warning asset would lapse: pattern-recognition would have to be rebuilt from scratch under threat rather than inherited ahead of it, mutual-aid drills would lose their template, and clinicians would expect measurable shifts in transmitted-stress markers among subsequent cohorts. Nothing physical collapses, but a maintained capacity and the identity that carries it both unwind.
% FOUNDING_PROBLEM: Recurrent catastrophe: expulsions, pogroms, and genocide repeatedly caught the community unprepared. The disciplines were built to make forgetting impossible, so the next generation would recognize escalation early enough to flee, resist, or mobilize aid.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: historiography of persecution cycles corroborates that the founding problem was real when the disciplines were codified; hate-crime and security data are cited by both defenders and critics on whether it remains live; clinical transmission research corroborates the mechanism while disputing its net adaptive value. No fully disinterested party attests the practice's current necessity — corroboration splits along threat assessment, which is itself signal.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the formation-cost is real, front-loaded into childhood, and installed without consent, but it purchases a capacity the payers themselves inherit as adults — hence moderate-to-high rather than severe. Suppression 0.55: enforcement is communal sanction, family pressure, and liturgical obligation rather than state force; it is real and increasingly institutionalized but not coercive in the hard sense. Theater 0.34: the transmission is mostly functional, but public ceremony, museum economies, and commemorative travel have grown faster than the drill-and-template function, so a rising minority of activity performs remembrance rather than exercising it. Accessibility_collapse 0.45: alternatives (secular processing, private remembrance, civic memorial) remain conceivable and are practiced at the margins, but inside the frame they register as inadequate or disloyal once the warning-logic is understood. Resistance 0.5: assimilation drift, generational pushback, and periodic reform movements that thin the calendar. The three measurement series run on ONE shared grid (t=0..80, decade steps) so every metric is authored at every examined time point; trajectories are monotone (institutionalization drift), not cyclical — no intermittent-reinforcement mechanism is claimed. Suppression is authored as a raw structural property; only extractiveness gets scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the leadership seat the arrangement is a lived vocation: chosen in adulthood, freighted with meaning, and rewarded with standing — coordination-forward. From the child seat it is extraction in nearly pure form: cost borne at maximum plasticity, benefit not yet receivable, exit impossible. From the adult burdened seat it is mixed: the benefit arrives (recognition patterns, mobilization templates) but welded to a burden installed pre-consent. Same-power divergence appears between leadership and researchers — both institutional, but identity_locked versus analytical exit produces opposite relationships to the same evidence. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the collective and the leadership sit near the beneficiary end (low d, damped or inverted effective extraction); the leadership's personal burden as descendants does not overturn this because their net position includes status, livelihood, and an adult-chosen role — no override is needed, the structural data already yields the right d. Victim declarations drive the target end: children sit nearest full-target (trapped exit amplifies toward d≈1.0), adult burdened descendants slightly below (they receive the adult-stage benefit), assimilation-inclined members high but moderated by their constrained-but-real exit. Scope is global for the diaspora-wide institutions, which modestly amplifies effective extraction through verification difficulty; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric errors. Reading the practice as pure extraction erases the coordination function that historically worked — communities that kept catastrophe memory did detect escalation patterns earlier and mobilize faster. Reading it as pure coordination erases the unconsented burden — the cost is imposed on people who could not refuse and persists in safety. Tangled_rope holds both. On obsolescence: the founding problem is contested rather than dead (threat levels are disputed, not settled), so no mandatrophy resolution is declared; the R5 mismatch consumer reads status=contested x verdict=world_rearranges, which raises no zombie flag. The rising theater series is watched but does not yet indicate atrophy — the drill-and-template function remains exercised, not merely performed. If a successor story finds the warning function fully displaced by heritage performance, this constraint's lifecycle endpoint is piton, reached through Goodhart drift rather than sudden capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of catastrophe_memory_kernel (trauma_encoding_reading); what would each sibling reading change structurally if adopted instead?',
    'Compile the sibling stories and compare beneficiary/victim structures: symbol_continuity_reading locates benefit in identity persistence and shrinks the victim class; survival_competence_reading recasts the transmitted costs as training inputs; boundary_maintenance_reading relocates victims to would-be leavers and outsiders.',
    'Under symbol_continuity the arrangement computes closer to rope (little asymmetric extraction); under boundary_maintenance the victim set changes identity and the enforcement story dominates; under survival_competence the burden is reframed as investment. The epsilon and classification of THIS story hold only within the trauma-encoding reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which kernel reading this is and what siblings would alter.').

omega_variable(
    warning_capacity_efficacy,
    'Does the transmitted vigilance actually deliver early-warning value at current threat levels, or does it produce miscalibration — seeing persecution everywhere, or failing to see novel forms?',
    'Comparative outcome studies of high-transmission versus low-transmission communities facing comparable threat events: response latency, mobilization success, false-alarm rates.',
    'If efficacy is low, the coordination component is largely vestigial and the arrangement shifts toward snare; if high, the extraction is the price of a working warning system and the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warning_capacity_efficacy, empirical, 'Whether the warning-benefit side of the ledger is real at current threat levels.').

omega_variable(
    intergenerational_ratification,
    'Can a formation-cost imposed on unconsenting children count as extraction when those children, reaching adulthood, predominantly ratify the practice and choose to transmit it onward?',
    'Track ratification rates: among burden-carriers who reach autonomy, what fraction maintain and retransmit versus thin or abandon the practice; distinguish ratification under continued communal pressure from ratification after exit.',
    'High informed ratification reframes the burden as a constitutive good the payers endorse, lowering effective extraction for the payer seats; low ratification confirms the imposition reading and supports stronger extraction scoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_ratification, conceptual, 'Whether later-life ratification dissolves the pre-consent imposition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (communal sanction, family pressure, liturgical obligation) or internalized (guilt and filial duty experienced as self-generated)?',
    'Post-exit suppression trajectory: follow members who leave the community — if vigilance, guilt, and commemorative compulsion persist after the enforcing structure is removed, a substantial share is internalized.',
    'If largely internalized, effective suppression exceeds the structural measure and survives formal liberalization of the practice; if largely structural, easing communal enforcement would release the burden quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized split in the enforcement of transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 70, 0.33).
narrative_ontology:measurement_basis(cata_tr_t70, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement_basis(cata_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 70, 0.65).
narrative_ontology:measurement_basis(cata_be_t70, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement_basis(cata_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement_basis(cata_su_t50, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 70, 0.55).
narrative_ontology:measurement_basis(cata_su_t70, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(cata_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'catastrophe memory ritual' decomposes into four structurally distinct claims per the epsilon-invariance principle — symbol continuity, survival competence, boundary maintenance, and trauma-encoded warning. Each is a separate story with its own epsilon, beneficiaries, and victims; measuring the practice through different observables yields different extraction profiles, which is the signature of distinct constraints sharing one label. Upstream/downstream: symbol_continuity is the most established claim (identity persistence is uncontroversial) and is routinely cited as evidence for the more contested downstream claims; trauma_encoding (this story) is the most contested and carries the highest epsilon, since its victim class is constituted by the transmission itself. All four files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
