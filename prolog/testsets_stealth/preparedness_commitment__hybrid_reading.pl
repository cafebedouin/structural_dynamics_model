% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Layered Disaster Preparedness Mandate and Memorial System (Hybrid Reading)
 *   domain: institutional/civic-governance
 *
 * SUMMARY:
 *   Modern disaster preparedness, as this reading construes it, is a layered
 *   institutional arrangement held together by a mandate-and-accreditation
 *   apparatus that conditions grants, licensure, and funding on
 *   participation. A memorial layer — anniversary ceremonies, commemorative
 *   exercises, legacy documentation, disaster memorials and the institutions
 *   that curate them — keeps the last catastrophe publicly present so that
 *   political and organizational will to prepare does not decay during quiet
 *   intervals. A competence layer — live exercises, equipment maintenance,
 *   training pipelines, interoperability testing — keeps response capability
 *   operationally real for the interval when the disaster actually arrives.
 *   The layers draw on the same budgets and the same staff time, and the
 *   boundary between commemoration and rehearsal is permanently contested, so
 *   holding both is a standing maintenance cost rather than a one-time build.
 *   This story authors the standing arrangement itself: extractiveness is
 *   assessed over the mandated layered system as it operates, with its
 *   genuine stabilization and capability functions and its compliance,
 *   vendor, and ceremonial overhead taken together; base_properties values
 *   reflect the 2025 end-state and match the measurement series endpoints.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda-setter and structural beneficiary (institutional/constrained) — administers the mandate, designs the exercise calendar, interprets after-action review; its budget and existence are products of the arrangement it runs
 *   - accreditation_bodies: co-agenda-setter (institutional/constrained) — defines and audits the standards that condition licensure and funding, without bearing service cost
 *   - preparedness_training_vendors: extraction recipient (organized/mobile) — collects the compliance-mandated spending on exercise design, platforms, and consulting
 *   - frontline_responders: primary target (organized/identity_locked) — executes the drill and documentation burden on top of service delivery; professional identity makes refusal unthinkable
 *   - non_emergency_service_budgets: payer (moderate/constrained) — hospital, school, and municipal budgets that fund the layers out of core service money
 *   - at_risk_populations: intended beneficiary with payer costs (powerless/constrained) — the protected population; funds the system through taxes and cannot exit hazard exposure
 *   - disaster_memorial_institutions: memorial-layer carrier (moderate/constrained) — keeps disaster memory publicly present; their continuity is the transmission mechanism
 *   - elected_officeholders: political beneficiary (powerful/immediate/mobile) — gains visible action and blame insurance on an election-cycle horizon
 *   - hazard_mismatched_communities: excluded voice (powerless/trapped) — communities whose actual hazards fall outside the canonical scenarios; absent from scenario selection
 *   - preparedness_researchers: analytical observer (analytical/analytical) — measures commitment decay and exercise realism; collects nothing from the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.62).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Layered Disaster Preparedness Mandate and Memorial System (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/civic-governance").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'bbd63e44-1171-4587-a456-2b28b02cf709').
narrative_ontology:cs_kernel_codification('bbd63e44-1171-4587-a456-2b28b02cf709', formalized).
narrative_ontology:cs_authority_grounding('bbd63e44-1171-4587-a456-2b28b02cf709', lineage).
narrative_ontology:cs_interpretation_layer_present('bbd63e44-1171-4587-a456-2b28b02cf709').
narrative_ontology:cs_reading_relation('bbd63e44-1171-4587-a456-2b28b02cf709', preparedness_commitment__husk_reading, influences).
narrative_ontology:cs_reading_relation('bbd63e44-1171-4587-a456-2b28b02cf709', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_axiom('bbd63e44-1171-4587-a456-2b28b02cf709', foundational, memorial_stabilization_is_load_bearing).
narrative_ontology:cs_axiom_status(memorial_stabilization_is_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('bbd63e44-1171-4587-a456-2b28b02cf709', memorial_stabilization_is_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('bbd63e44-1171-4587-a456-2b28b02cf709', secondary, dual_layer_maintenance_obligation).
narrative_ontology:cs_axiom_status(dual_layer_maintenance_obligation, holdable).
narrative_ontology:cs_axiom_grounding('bbd63e44-1171-4587-a456-2b28b02cf709', dual_layer_maintenance_obligation, instrumental).
narrative_ontology:cs_reference_frame('bbd63e44-1171-4587-a456-2b28b02cf709', balanced_layered_maintenance).
narrative_ontology:cs_drift_state('bbd63e44-1171-4587-a456-2b28b02cf709', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bbd63e44-1171-4587-a456-2b28b02cf709', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, preparedness_training_vendors).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, at_risk_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, elected_officeholders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, disaster_memorial_institutions).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, non_emergency_service_budgets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, at_risk_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate the exercise calendar, administer grant conditionality, and run the after-action review cycle that interprets what counts as adequate preparedness. Their budgets, headcount, and statutory existence are products of the arrangement they administer; they also bear its documentation load and answer politically when a real disaster exposes gaps. Exit would mean dismantling their own mandate.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, emergency_management_agencies, beneficiary).

% Set and audit exercise, documentation, and capability standards for hospitals and response organizations; compliance with their standards conditions licensure and funding. They do not deliver response capacity themselves; they define and enforce what the layers must contain, and they bear none of the service cost of compliance.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, accreditation_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Sell exercise design, training platforms, documentation software, and compliance consulting into the market the mandate creates. Their revenue tracks the compliance burden directly, and they can and do pivot between compliance markets when any one of them contracts.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, preparedness_training_vendors, beneficiary,
    organized, biographical, mobile, national).

% Live in flood plains, seismic zones, and storm coasts; they are the population the maintained capacity exists to protect. They fund the layers through taxes, bear drill-time and opportunity costs, and cannot individually exit their hazard exposure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, at_risk_populations, beneficiary,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, at_risk_populations, payer).

% Cut the ribbon at commemorative exercises and fund preparedness surges after visible disasters; the arrangement gives them visible action and blame insurance. Their horizon is the election cycle, which favors the memorial layer's visibility, and they leave office while the arrangement persists.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, elected_officeholders, beneficiary,
    powerful, immediate, mobile, national).

% Museums, anniversary commissions, and memorial foundations that carry the disaster-memory layer: curating the events, anniversaries, and narratives that keep the commitment publicly present. Their programming budgets depend on the memorial layer remaining funded, and their institutional continuity is the transmission mechanism for disaster memory.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_memorial_institutions, beneficiary,
    moderate, generational, constrained, national).

% Fire, EMS, hospital, and public-health staff who execute the exercise calendar and produce its documentation on top of service delivery. Drill fatigue and paperwork are their recurring grievances, but refusing participation is unthinkable inside a professional identity built on readiness; exit runs through leaving the vocation, not through opting out of the drills.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    organized, biographical, identity_locked, national).

% Hospital operating budgets, school districts, and municipal departments that must fund drills, supplies, and compliance documentation out of core service money. Accreditation and funding conditions leave them no lawful exit; every memorial or compliance dollar is a service dollar forgone.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, non_emergency_service_budgets, payer,
    moderate, biographical, constrained, national).

% Communities whose dominant hazards fall outside the canonical exercise scenarios and the memorial calendar — heat, wildfire smoke, dam failure in regions drilled for hurricane or earthquake. They would object that the layers are tuned to someone else's disaster; they are not in the room where scenarios are chosen and cannot relocate their exposure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, hazard_mismatched_communities, excluded,
    powerless, generational, trapped, regional).

% Disaster sociologists and public-administration researchers who measure commitment decay, exercise realism, and the memorial layer's effects across jurisdictions. They see the full layered structure, publish on its drift, and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, preparedness_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, preparedness_training_vendors).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of sustaining disaster response capacity across long quiet intervals: preparedness is a public good whose benefits are contingent and distant, so without a maintained structure of mandates, exercises, and commemoration, funding and attention decay between disasters. The memorial layer keeps the commitment politically alive; the competence layer keeps the capability operationally real.
% TRANSFER_FUNCTION: Moves budget, staff time, and attention from general operations and taxpayers to emergency-management institutions, training vendors, and memorial programming; moves disaster memory and response doctrine from one generation of practitioners to the next.
% ABSENT_VOICES: Communities whose actual hazards do not match the canonical drill scenarios (hazard_mismatched_communities) are not in the room where exercise scenarios and memorial priorities are set. Frontline staff who experience the drill and documentation burden directly have voice only through unions and professional associations, not in program design.
% DISAPPEARANCE_RATIONALE: If the layered arrangement vanished overnight, agencies would lose mandate and budget, the training and compliance economy would collapse, exercise and memorial calendars would lapse, and disaster memory would stop transmitting institutionally. When the next major disaster arrived, response capacity would depend on whatever improvisation remained — the rearrangement would be visible in the first catastrophic response gap and in the political blame that follows it.
% FOUNDING_PROBLEM: Societies repeatedly discovered, after catastrophic response failures, that capacity had decayed during quiet intervals: agencies defunded, equipment obsolete, personnel untrained, institutional memory lost. The layered arrangement was built to solve commitment decay — keeping the will to prepare alive across generations (memorial layer) while keeping the capacity to respond real (competence layer).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legislative-commissioned after-action reports on major disaster responses, the disaster sociology literature documenting commitment and funding decay between events, and insurance and actuarial analyses of preparedness gaps. Note the corroboration attests the decay problem itself, not the adequacy of the current arrangement; only the benefiting parties attest that this arrangement is the right solution.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58: the arrangement performs real stabilization and capability work, but a substantial share of mandated spending converts to vendor revenue and compliance overhead rather than capability — the share grew with each post-disaster funding cycle and plateaued as the apparatus matured. Suppression is 0.62, authored as a raw structural property (the engine scales only extractiveness, by directionality and scope): participation is coerced by grant conditionality and accreditation rather than by force, and the voluntary alternative fails by free-riding, which closes the practical exit without any single actor wielding visible force. Theater_ratio is 0.46 and rising: the memorial layer is performative in form, and this reading's claim is precisely that the performance is partly load-bearing — the omega memorial_load_bearing_fraction marks the unresolved share rather than the metric settling it. Accessibility_collapse is 0.58: pure-voluntary and no-preparation alternatives fail identifiably (free-riding, catastrophe), but alternative designs — leaner competence-only regimes, community-based models — remain conceivable, so alternatives collapse only partially. Resistance is 0.45: drill fatigue, documentation pushback, and budget fights are real but bounded by genuine hazard exposure. The claimed_type (tangled_rope) is authored from structure — a genuine coordination function and asymmetric extraction through the same apparatus, held by active enforcement — independently of these metric values; the engine computes each seat's type. The three measurement series share one grid (1980, 1989, 1996, 2001, 2005, 2011, 2017, 2025; T0 is the modern emergency-management founding era) so every metric is authored at every examined point. suppression_requirement is tracked because the narrative is an enforcement ratchet: conditionality and accreditation hardened after 2001 and 2005 and then stabilized, rather than remaining flat while only extraction moved.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute differently. From emergency_management_agencies the arrangement is the structure it administers, draws budget from, and answers for — coordination it built and staffs. From frontline_responders the same exercise calendar is compliance burden layered on service delivery, and their identity lock means the burden is experienced as duty rather than as a cost they could decline. From preparedness_training_vendors it is revenue; from at_risk_populations it is protection they fund but do not control; from non_emergency_service_budgets it is a claim on money that would otherwise deliver care or schooling. Same-level divergence: two institutional agenda-setters (agencies, accreditors) share a power atom but differ in relationship — agencies bear the compliance costs they impose downstream, while accreditors impose without bearing. And two organized actors (vendors, frontline responders) differ sharply on exit: vendors are mobile across compliance markets, responders are identity-locked into the vocation the drills express — the lock is professional identity, career-path dependence fused with a vocational self-concept in which readiness is who they are; if that frame broke, drill refusal would become a live option and the payer seat's computed position would shift.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality for emergency_management_agencies, preparedness_training_vendors, disaster_memorial_institutions, and elected_officeholders — each collects budget, revenue, programming, or political credit from the arrangement's operation. at_risk_populations are declared beneficiaries with payer costs (taxes, drill-time, and the opportunity cost of memorial versus mitigation spending), so their directionality sits low but not at the floor. Victims map to high directionality: frontline_responders and non_emergency_service_budgets bear the transfer directly, and the responders' identity lock and the budgets' accreditation constraints hold both near the full-target end despite their organizational power — organizational power without exit does not damp effective extraction. hazard_mismatched_communities are excluded rather than coordinated: scenario selection is the enforcement object they are absent from, which is why they are authored as excluded rather than as a victim group.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — commitment and capability decay between disasters — is live: hazards recur and quiet intervals keep arriving, so this is not a mandate outliving its function. The mandatrophy risk is nonetheless the precise content the husk_reading names: if the memorial layer's stabilization function atrophied while the ceremonies continued, the arrangement would drift toward theatrical maintenance of an atrophied function, and the theater_ratio trajectory (0.25 to 0.46 over the interval) is the drift signal to watch. The tangled-rope classification is what prevents mislabeling in both directions: reading the arrangement as pure extraction erases the genuine stabilization and capability functions the memorial and competence layers perform; reading it as pure coordination erases the vendor capture, compliance burden, and memorial bloat that ride the same structure. The hybrid reading holds the tension between layers as permanent maintenance cost rather than a defect awaiting resolution — omega layer_tension_resolvability marks whether that permanence is itself an open empirical question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the hybrid_reading of kernel preparedness_commitment. Would instantiating the husk_reading or the competence_reading instead change the constraint''s structural identity?',
    'Comparative classification: author the sibling readings as separate constraint stories and compare computed types, beneficiary structures, and drift states across the three. The disagreement is located in the memorial layer''s load-bearing share, which this reading affirms, the husk_reading denies outright, and the competence_reading treats as dispensable.',
    'If the husk_reading is correct, the memorial layer here is performance without function and the classification shifts toward piton or snare territory with theater_ratio near 1.0; if the competence_reading is correct, the memorial layer is pure overhead, extraction rises, and the coordination function narrows to the exercise regime alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one reading of the preparedness_commitment kernel; sibling readings would restructure the memorial layer''s status.').

omega_variable(
    memorial_load_bearing_fraction,
    'What fraction of memorial-layer activity (anniversary ceremonies, commemorative exercises, legacy documentation) genuinely stabilizes commitment to preparedness, and what fraction is pure overhead?',
    'Natural experiments: compare commitment decay (budget trajectories, exercise participation, staffing continuity) in jurisdictions that curtailed memorial programming against matched jurisdictions that maintained it, controlling for disaster salience.',
    'A high load-bearing fraction vindicates the hybrid structure and books memorial spending as coordination cost; a low fraction means the memorial layer is overhead riding on the competence layer and the arrangement drifts toward the husk diagnosis with theater_ratio climbing past functional share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_load_bearing_fraction, empirical, 'Share of memorial-layer activity that performs real commitment-stabilization work.').

omega_variable(
    layer_tension_resolvability,
    'Is the tension between the memorial and competence layers a permanent structural feature of preparedness maintenance, or can integrated formats (realistic scenario exercises that also serve commemorative functions) dissolve it?',
    'Evaluate integrated exercise programs (disaster-anniversary drills with full operational objectives) against separated programs on both commitment metrics and capability metrics over multiple exercise cycles.',
    'If resolvable, the maintenance cost is a transitional inefficiency and the arrangement could migrate toward lower-overhead coordination; if structural, the dual-layer overhead is a permanent tax and the tangled-rope structure is stable rather than transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(layer_tension_resolvability, empirical, 'Whether the memorial/competence tension is engineerable away or structural.').

omega_variable(
    abandonment_counterfactual,
    'Would preparedness commitment actually decay to abandonment in the absence of the memorial layer, or do recurring disasters alone sustain it?',
    'Historical comparison of preparedness trajectories in hazard contexts with infrequent disasters (where memory fades) versus frequent ones, and of programs that lost their memorial carriers (defunded commissions, discontinued anniversaries).',
    'If recurring disasters alone sustain commitment, the memorial layer''s coordination claim fails and its budget is extractive; if commitment decays between events without memorial reinforcement, the memorial layer is the arrangement''s load-bearing stabilization mechanism and cutting it is the false economy the hybrid reading warns against.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abandonment_counterfactual, empirical, 'Counterfactual: does the memorial layer prevent abandonment that would otherwise occur?').

omega_variable(
    authority_grounding_framing,
    'Is the adjudicating authority for what counts as adequate preparedness grounded in lineage (continuity with the commemorated disasters, transmitted through the memorial layer) or in practice (the professional exercise regime whose action is its own standard)?',
    'Trace whose interpretations bind when the two groundings conflict: when after-action practice contradicts commemorative doctrine, which one revises the standards, and which one gets deferred to the next anniversary cycle?',
    'A practice-grounded authority would make the observed drift read as internal professional evolution rather than departure from a memorial reference frame; a lineage-grounded authority makes the memorial layer constitutive of legitimacy, raising the cost of cutting it and changing which stakeholder seat the engine treats as the interpretive buffer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: dual grounding (memorial lineage vs. professional practice) of the preparedness authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1980, preparedness_commitment__hybrid_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(prep_tr_t1989, preparedness_commitment__hybrid_reading, theater_ratio, 1989, 0.22).
narrative_ontology:measurement(prep_tr_t1996, preparedness_commitment__hybrid_reading, theater_ratio, 1996, 0.28).
narrative_ontology:measurement(prep_tr_t2001, preparedness_commitment__hybrid_reading, theater_ratio, 2001, 0.33).
narrative_ontology:measurement(prep_tr_t2005, preparedness_commitment__hybrid_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(prep_tr_t2011, preparedness_commitment__hybrid_reading, theater_ratio, 2011, 0.4).
narrative_ontology:measurement(prep_tr_t2017, preparedness_commitment__hybrid_reading, theater_ratio, 2017, 0.44).
narrative_ontology:measurement(prep_tr_t2025, preparedness_commitment__hybrid_reading, theater_ratio, 2025, 0.46).

% Extraction over time
narrative_ontology:measurement(prep_be_t1980, preparedness_commitment__hybrid_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(prep_be_t1989, preparedness_commitment__hybrid_reading, base_extractiveness, 1989, 0.33).
narrative_ontology:measurement(prep_be_t1996, preparedness_commitment__hybrid_reading, base_extractiveness, 1996, 0.41).
narrative_ontology:measurement(prep_be_t2001, preparedness_commitment__hybrid_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(prep_be_t2005, preparedness_commitment__hybrid_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(prep_be_t2011, preparedness_commitment__hybrid_reading, base_extractiveness, 2011, 0.57).
narrative_ontology:measurement(prep_be_t2017, preparedness_commitment__hybrid_reading, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement(prep_be_t2025, preparedness_commitment__hybrid_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1980, preparedness_commitment__hybrid_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement(prep_su_t1989, preparedness_commitment__hybrid_reading, suppression_requirement, 1989, 0.2).
narrative_ontology:measurement(prep_su_t1996, preparedness_commitment__hybrid_reading, suppression_requirement, 1996, 0.33).
narrative_ontology:measurement(prep_su_t2001, preparedness_commitment__hybrid_reading, suppression_requirement, 2001, 0.44).
narrative_ontology:measurement(prep_su_t2005, preparedness_commitment__hybrid_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(prep_su_t2011, preparedness_commitment__hybrid_reading, suppression_requirement, 2011, 0.6).
narrative_ontology:measurement(prep_su_t2017, preparedness_commitment__hybrid_reading, suppression_requirement, 2017, 0.61).
narrative_ontology:measurement(prep_su_t2025, preparedness_commitment__hybrid_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'disaster preparedness' decomposes into three readings of one kernel (preparedness_commitment): husk_reading (memorial performance without operational substance, extractiveness concentrated in ceremonial overhead), competence_reading (live exercised knowledge as sufficient, extractiveness near the coordination floor), and this hybrid_reading (layered system, extractiveness split between genuine stabilization function and compliance/vendor overhead). The readings disagree about the memorial layer's load-bearing share — that disagreement is the located structural difference — so each is authored as a separate constraint with its own extractiveness per the epsilon-invariance principle. This story sits structurally between its siblings and is the only one that funds and legitimates both layers; it links to both as its constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
