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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered System: Memorial Stabilization with Competence Maintenance (Hybrid Reading)
 *   domain: civic-governance/institutional-memory
 *
 * SUMMARY:
 *   A catastrophic flood founded a standing preparedness compact built in two
 *   entwined layers. The competence layer maintains operational function: a
 *   statutory multi-agency drill calendar, certification and succession
 *   pipelines, equipment refresh cycles, interoperability standards. The
 *   memorial layer stabilizes commitment: annual remembrance of the founding
 *   disaster, anniversary exercises staged with officials present, memorial
 *   trusts and archives, named infrastructure. The hybrid reading —
 *   instantiated by this story — holds both layers load-bearing: the memorial
 *   layer prevents abandonment of the commitment between disasters, when the
 *   problem is invisible; the competence layer prevents the catastrophic
 *   failure that abandonment would permit; and the tension between them
 *   (ceremony displacing training hours, training budgets quietly
 *   cannibalizing observance) is the compact's characteristic maintenance
 *   cost. This file is one reading of the preparedness_commitment kernel; the
 *   husk and competence sibling readings are separate constraints (separate
 *   files, linked via network) with their own epsilon values and victim sets.
 *   The epsilon referent throughout is the standing layered compact itself,
 *   assessed by this reading's own lights — never the competence-only
 *   arrangement this reading might prefer. KEY AGENTS (by structural
 *   relationship): - emergency_management_directorate: agenda-setting
 *   administrator (institutional/constrained) — runs both layers, captures
 *   budget growth and statutory permanence - frontline_responders: principal
 *   bearer of the tension cost (organized/constrained) — training hours
 *   diverted to ceremonial staging, funded equipment in return -
 *   at_risk_coastal_communities: intended protectee and incidental payer
 *   (moderate/trapped) — receives competence-layer protection, pays the levy,
 *   cannot relocate - general_taxpayers: diffuse payer
 *   (powerless/constrained) — funds the national share with no standing voice
 *   - memorial_civic_institutions: memorial-layer beneficiary
 *   (organized/identity_locked) — exists because the commemorative layer
 *   exists - survivor_advocacy_networks: commitment anchor
 *   (organized/identity_locked) — constitutes the political demand for the
 *   memorial layer - elected_officials: credit collector (powerful/arbitrage)
 *   — appropriates and presides, rotates out before costs land -
 *   volunteer_rescue_networks: excluded alternative provider
 *   (moderate/constrained) — operates outside the mandate loop -
 *   state_audit_office: analytical observer (institutional/analytical) —
 *   audits realism versus ceremony on both layers
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.35).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered System: Memorial Stabilization with Competence Maintenance (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "civic-governance/institutional-memory").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '62d37d09-c057-4d56-a556-fcba7c976b55').
narrative_ontology:cs_kernel_codification('62d37d09-c057-4d56-a556-fcba7c976b55', formalized).
narrative_ontology:cs_authority_grounding('62d37d09-c057-4d56-a556-fcba7c976b55', lineage).
narrative_ontology:cs_interpretation_layer_present('62d37d09-c057-4d56-a556-fcba7c976b55').
narrative_ontology:cs_reading_relation('62d37d09-c057-4d56-a556-fcba7c976b55', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('62d37d09-c057-4d56-a556-fcba7c976b55', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_axiom('62d37d09-c057-4d56-a556-fcba7c976b55', foundational, memorial_practice_stabilizes_long_term_commitment).
narrative_ontology:cs_axiom_status(memorial_practice_stabilizes_long_term_commitment, holdable).
narrative_ontology:cs_axiom_grounding('62d37d09-c057-4d56-a556-fcba7c976b55', memorial_practice_stabilizes_long_term_commitment, empirically_contingent).
narrative_ontology:cs_axiom('62d37d09-c057-4d56-a556-fcba7c976b55', secondary, dual_layer_system_requires_sustained_tension_investment).
narrative_ontology:cs_axiom_status(dual_layer_system_requires_sustained_tension_investment, holdable).
narrative_ontology:cs_axiom_grounding('62d37d09-c057-4d56-a556-fcba7c976b55', dual_layer_system_requires_sustained_tension_investment, instrumental).
narrative_ontology:cs_reference_frame('62d37d09-c057-4d56-a556-fcba7c976b55', founding_covenant_of_remembrance_and_readiness).
narrative_ontology:cs_drift_state('62d37d09-c057-4d56-a556-fcba7c976b55', present_day_mature_compact, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62d37d09-c057-4d56-a556-fcba7c976b55', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, at_risk_coastal_communities).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, emergency_management_directorate).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, memorial_civic_institutions).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, elected_officials).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, survivor_advocacy_networks).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, general_taxpayers).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, at_risk_coastal_communities).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, memorial_stabilization_hypothesis).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, generational_capacity_decay).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers both halves of the compact: it owns the statutory drill calendar, certification pipeline, and equipment refresh cycle, and it stages the annual remembrance and anniversary exercises. Its budget lines, headcount, and statutory permanence have grown with each added mandate, and its senior staff sit on the memorial commission that decides what the observance includes. Leaving is not a live option for its leadership — careers and the agency's reason for being are bound up in running the compact.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_directorate, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, emergency_management_directorate, beneficiary).

% Staff the boats, ambulances, and command posts the compact maintains. They train under the mandatory calendar and also stand in formation at anniversary exercises staged for officials and cameras, which internal surveys say consumes drill hours that would otherwise go to flood-rescue and swiftwater scenarios. They benefit from the equipment budgets and overtime the compact funds, but bear the hour-for-hour cost of ceremonial duty.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_responders, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, frontline_responders, beneficiary).

% Live behind the levees and in the surge zones the compact protects. They receive warning systems, evacuation planning, and a trained response cadre whose quality depends on how much of the budget reached exercises rather than podiums. They pay a dedicated local preparedness levy and attend the annual remembrance. Relocating away from the floodplain would mean abandoning home equity, extended family, and livelihoods, and they hold no seat in the compact's governance.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, at_risk_coastal_communities, beneficiary,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, at_risk_coastal_communities, payer).

% Fund the national share of both layers through general revenue. They see the appropriations line item, not the drill roster; their exposure to the compact is a recurring budget decision made elsewhere, and their main lever is episodic fiscal politics rather than any standing voice in how the money splits between capability and commemoration.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, general_taxpayers, payer,
    powerless, biographical, constrained, national).

% Foundations, memorial trusts, and remembrance societies built around the founding disaster. They administer the commemorative calendar, hold the archive of the founding event, and receive grants and endowed income tied to observance. Their charters, staff careers, and donor bases exist because the commemorative layer exists; winding it down would dissolve them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, memorial_civic_institutions, beneficiary,
    organized, generational, identity_locked, regional).

% Speak at the anniversary, cut ribbons at equipment deliveries, and cite the compact's readiness statistics in campaigns. They appropriate its budget and can reshape its mandates, but the electoral cycle carries each of them out of office long before the consequences of any funding choice land, and the visible half of the compact returns credit to whoever stands at the podium.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, elected_officials, beneficiary,
    powerful, immediate, arbitrage, national).

% Organizations of people who lived through the founding disaster and their successor members. They lobby for the commemorative layer's protection, testify at budget hearings that readiness is the debt owed to the dead, and treat any reduction in observance as betrayal. Their identity is constituted by remembrance; abandoning it would mean abandoning the meaning they made of their loss.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, survivor_advocacy_networks, beneficiary,
    organized, generational, identity_locked, regional).

% Community-based boat crews, radio nets, and neighborhood response teams that operated informally before the compact and still operate alongside it. They are outside the mandate loop: no levy funds, no certification reciprocity, no place in the drill calendar. Their organizers argue that a fraction of the commemorative budget spent equipping and integrating them would buy more rescuable hours per dollar.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, volunteer_rescue_networks, excluded,
    moderate, biographical, constrained, local).

% Audits both layers on a rotating schedule: whether mandated drills met scenario-realism thresholds, whether commemorative spending stayed within appropriation, and whether capability indicators moved between events. Its reports are the main external check on the compact and the source most cited by both its defenders and its critics.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, state_audit_office, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, emergency_management_directorate).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains expensive, rarely-used response capacity across generations, which no single locality could justify carrying alone: pooled funding, a standardized training and certification pipeline, shared equipment stockpiles, and interoperable procedures across jurisdictions. The memorial layer adds a commitment device — a recurring public act of remembrance that keeps the funding coalition assembled between disasters, when the problem the capacity addresses is invisible.
% TRANSFER_FUNCTION: Moves tax revenue and mandated organizational effort into response capability and commemorative infrastructure; moves political visibility and legitimacy toward officeholders and the administering agency; moves ceremonial duty hours from frontline responders to staged observances; and, in an event, moves rescue and aid to affected populations.
% ABSENT_VOICES: Volunteer rescue networks sit outside the mandate loop and would argue for devolving budget toward distributed community capability; local finance officers who must pass the levy each cycle would argue the commemorative share is discretionary; future residents of the at-risk zones, who will inherit whichever layer balance today's cohort strikes, have no seat at all. Each is absent because the compact is negotiated among the administering agency, survivor organizations, and appropriating officials.
% DISAPPEARANCE_RATIONALE: If the compact vanished overnight, the trained multi-jurisdiction cadre would thin within a few rotation cycles, equipment would age out unreplaced, and the funding coalition would not spontaneously reassemble — the founding problem was precisely that commitment decays once disaster memory fades. Communities would reorganize: some buying private insurance and accepting the residual risk, others rebuilding informal mutual-aid networks, localities competing to attract or shed exposed households.
% FOUNDING_PROBLEM: After the founding flood, each successive generation had forgotten the last one's lesson: equipment rotted, certified operators left, joint procedures lapsed, and the next event repeated deaths that standing investment would have prevented. The compact was built to keep both the memory and the capability alive past the lifetime of anyone who experienced the event firsthand.
% FOUNDING_PROBLEM_CORROBORATION: Survivor advocacy networks attest the problem is live, citing audit findings that capability indicators sag between events; the state audit office — outside the benefiting parties — independently documents recurring inter-event erosion of drill realism and equipment readiness. Fiscal-policy analysts and several levy-passing local governments attest from the other side that the original amnesia problem has been institutionally solved and that what persists is the compact's own perpetuation. No party disputes that the founding problem existed; they dispute whether it still does.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).
:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored for the standing layered compact as the hybrid reading assesses it, independently of the claimed type. Extractiveness 0.42: the competence layer demonstrably delivers the coordination good — capability persists across generations where the pre-compact baseline decayed — but the mature compact carries recurring costs beyond that good's marginal cost: duplicated administration across memorial and operations units, ceremonial exercise staging priced at full drill-hour cost, and commemorative capital spending. Suppression 0.35 is authored as a raw structural property (only extractiveness gets scaled downstream): statutory drill minima, earmarked levies, and certification gatekeeping bind participants, yet dissent is lawful and recurrent — audit criticism, levy revolts, and reform bills appear every cycle. Theater_ratio 0.47: close to half of observable activity is commemorative or staged; the hybrid reading's distinguishing claim is that part of this share performs commitment-stabilization work the competence layer cannot substitute for (see the memorial_layer_functionality omega), so the metric is authored honestly high without the claim asserting the theater is waste. Accessibility_collapse 0.30: alternative architectures remain constructible — competence-only regimes, devolved volunteer capability — nothing about the compact forecloses them once seen. Resistance 0.35: episodic fiscal challenge, blunted by disaster-salience surges. The temporal series run on one shared grid (all three metrics at every five-year point) and show the salience cycle: enforcement ratchets hard after each event (t=0, t=20, t=35) and decays between events, while theater and extraction resume underlying accretion after each reform episode trims them. The oscillation itself functions as intermittent reinforcement — each re-ratchet re-legitimates the whole compact including its theatrical share — so the cycle is part of the mechanism, not noise. Base properties were measured at t=50, a decay-phase point, which is why the scalar suppression sits below the series maximum. Receipt surface: the compact's net gains demonstrably accrue to the administering directorate (budget, headcount, and statutory permanence grew at every ratchet), hence gain_flow names that seat; fixing the layer imbalance is organizationally trivial but politically prohibitive for any actor able to attempt it — cutting commemoration draws survivor-network opposition and memorial-vote penalties — hence fixing_cost prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the directorate's chair the compact is a covenant faithfully administered: both layers are duties, growth is mandate fulfillment. From the responder's position the same structure is hour-for-hour diversion — every staged anniversary exercise is a swiftwater scenario not run. From the taxpayer's position it is a recurring line item whose split between levees and lecterns is invisible. From the memorial institutions' and survivors' positions it is existential: the observance is the commitment, and questioning its cost borders on sacrilege. Officeholders experience it as free legitimacy — they appropriate the cost but rotate out before its consequences land. The engine computes per-seat classifications from power, exit, and role data; these divergent experiences are what that computation should surface, and no authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. Elected_officials (beneficiary, arbitrage exit) sit nearest the beneficiary pole: they collect visibility at zero personal cost and exit via the electoral cycle. Emergency_management_directorate (agenda_setter, secondary beneficiary, constrained exit) derives low but not zero d: it captures budget and permanence while bearing real administrative burden and having no exit. Memorial_civic_institutions and survivor_advocacy_networks (both identity_locked) derive strongly beneficiary-side: their subsidy is identity-constituted and exiting would dissolve them. At_risk_coastal_communities derive near symmetric — genuine protection received against levy payments paid — with trapped exit pulling them target-ward on the verification margin, since they cannot audit the layer split from where they stand. Frontline_responders derive net-target: the diverted hours exceed the funded equipment at the margin, and constrained career exit blocks arbitrage. General_taxpayers derive nearest the full-target pole: they pay everything, decide nothing, and cannot escape the levy. No directionality_overrides are authored — the beneficiary/victim declarations plus exit atoms already distinguish every seat, and an override keyed by power atom could not (three seats share 'organized'). Suppression remains unscaled in this account per the structural-property rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cross-generational amnesia of disaster lessons — is contested rather than dead, so the classic zombie signature (dead founding problem plus a world_rearranges verdict) does not fire; R5 corroboration is deliberately sourced outside the benefiting parties (state audit office, fiscal analysts) to keep the genealogy from degenerating into a cover story. Classification discipline: a competence-only analyst sees commemorative spend as pure waste and would read this arrangement husk-ward; the memorial coalition frames the whole compact as sacred obligation — a naturality move that would dress the payment structure as if it were simply what remembering costs, mountain-ward. The tangled_rope claim preserves both structural facts at once: the compact genuinely coordinates capacity maintenance that nobody would carry alone, and the same structure channels measurable payment from taxpayers and responder time toward the memorial layer's constituencies. The tension between layers — ceremony crowding out training hours, training budgets quietly cannibalizing observance — is the arrangement's characteristic maintenance cost and is carried by the tension_cost_incidence omega rather than averaged away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_layer_functionality,
    'Does the memorial layer causally stabilize funding and political commitment between disasters, or would the statutory mandates sustain the same commitment if commemorative activity were cut?',
    'Austerity natural experiments and phased-reduction pilots: track budget continuity, drill completion rates, and capability-audit outcomes in jurisdictions that cut commemorative activity against matched controls that kept it.',
    'If memorial activity is load-bearing, the hybrid reading''s classification stands and part of the measured theater share is functional; if not, the compact''s structure converges on the husk reading''s description — high theater, low retained function — and epsilon should be revised sharply upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_layer_functionality, empirical, 'Whether commemorative activity performs commitment-stabilization work.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the hybrid_reading of the preparedness_commitment kernel; the husk_reading and competence_reading siblings instantiate different constraints over the same arrangements — where exactly does the disagreement bite?',
    'Conceptual: the disagreement is located in the causal status of the memorial layer (load-bearing stabilizer vs inert mimicry vs replaceable overhead). Resolution comes from the functionality evidence gathered under memorial_layer_functionality plus comparative classification of the sibling stories.',
    'Under the husk reading, epsilon for the same arrangements is far higher and the victim set expands to include the protected public; under the competence reading, epsilon collapses toward coordination-floor levels and the memorial constituencies become pure riders. This story''s values are valid only for the hybrid instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: sibling readings of the same kernel are separate constraints with different epsilon and victim sets.').

omega_variable(
    tension_cost_incidence,
    'Who bears the maintenance cost created by the tension between the memorial and competence layers — diffuse taxpayers through duplicated administration, or concentrated frontline responders through displaced training hours?',
    'Time-allocation audits separating ceremonial from scenario hours, paired with budget-line analysis of duplicated administrative units across the memorial and operations sides of the compact.',
    'Concentrated incidence pushes the responder seat toward strong target-directionality and the arrangement snare-ward; diffuse incidence keeps it nearer a managed tangled-rope configuration with no concentrated capturer beyond the administering agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tension_cost_incidence, empirical, 'Incidence of the dual-layer maintenance cost across payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_commit_hybrid_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t0, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t5, preparedness_commitment__hybrid_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t5, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t10, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t15, preparedness_commitment__hybrid_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t15, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t20, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t25, preparedness_commitment__hybrid_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t25, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t30, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t35, preparedness_commitment__hybrid_reading, theater_ratio, 35, 0.34).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t35, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t40, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t45, preparedness_commitment__hybrid_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t45, observed).
narrative_ontology:measurement(prep_commit_hybrid_tr_t50, preparedness_commitment__hybrid_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement_basis(prep_commit_hybrid_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(prep_commit_hybrid_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t0, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t5, preparedness_commitment__hybrid_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t5, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t10, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t15, preparedness_commitment__hybrid_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t15, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t20, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t25, preparedness_commitment__hybrid_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t25, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t30, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t35, preparedness_commitment__hybrid_reading, base_extractiveness, 35, 0.32).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t35, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t40, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t45, preparedness_commitment__hybrid_reading, base_extractiveness, 45, 0.39).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t45, observed).
narrative_ontology:measurement(prep_commit_hybrid_be_t50, preparedness_commitment__hybrid_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(prep_commit_hybrid_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_commit_hybrid_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t0, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t5, preparedness_commitment__hybrid_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t5, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t10, preparedness_commitment__hybrid_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t10, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t15, preparedness_commitment__hybrid_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t15, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t20, preparedness_commitment__hybrid_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t20, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t25, preparedness_commitment__hybrid_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t25, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t30, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t35, preparedness_commitment__hybrid_reading, suppression_requirement, 35, 0.51).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t35, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t40, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t45, preparedness_commitment__hybrid_reading, suppression_requirement, 45, 0.39).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t45, observed).
narrative_ontology:measurement(prep_commit_hybrid_su_t50, preparedness_commitment__hybrid_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement_basis(prep_commit_hybrid_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'disaster preparedness' decomposes into three structurally distinct readings of one kernel (preparedness_commitment), each a separate constraint story with its own stable epsilon per the epsilon-invariance principle. This hybrid reading (memorial layer load-bearing, competence layer functional, tension cost real) sits between its siblings: the husk reading describes the same observables as memorial performance without retained competence — far higher epsilon and an expanded victim set including the protected public — while the competence reading describes preparedness as live exercised knowledge — epsilon near the coordination floor with the memorial constituencies as pure riders. Upstream/downstream structure: the competence reading's exercise-effectiveness evidence base is cited by hybrid defenders as proof the competence layer works; the husk reading's audit findings press on the hybrid classification from below as theater accumulates. Each story links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
