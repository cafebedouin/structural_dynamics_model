% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred Divine Ordinance (Sacral Fidelity Reading)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The Lycurgan constraint in this reading holds that the laws attributed to
 *   the legendary lawgiver Lycurgus are divinely ordained, cosmically
 *   immutable, and binding on Spartan citizens across all time. Under this
 *   reading, the constraint is a mountain: unchangeability is not a design
 *   choice but a structural fact about the laws' cosmic status. Decline must
 *   be attributed to citizen failure to maintain fidelity, not to the
 *   system's inability to adapt. This reading is held by Spartan
 *   traditionists and by some ancient historians; it forecloses the
 *   demographic_trap_reading (which attributes decline to unrevisability) and
 *   coexists with the adaptive_fiction_reading (which treats sacrality as a
 *   noble lie masking covert revision).
 *
 * KEY AGENTS:
 *   - spartan_traditionist_faction: agenda_setter (institutional, civilizational, identity_locked) — controls interpretation and administration, claims lineage to Lycurgus, treats any revision as sacrilege
 *   - spartan_citizen_body: payer + beneficiary (organized, generational, trapped) — subject to the laws' demands, but also receive stability and sacred membership
 *   - helot_underclass: payer (powerless, generational, trapped) — bear the material cost through enserfment, have no voice in interpretation
 *   - oracular_priesthood: beneficiary (powerful, civilizational, constrained) — certify the divine origin, gain influence from legitimacy-gatekeeping role
 *   - revisionist_philosophers: excluded (powerful, biographical, mobile) — would argue for revision and democratic deliberation, structurally silenced within Spartan spaces
 *   - spartan_archaic_historians: observer (analytical, biographical, analytical) — detect evidence of covert adaptation and human choice beneath the sacral framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.15).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.08).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred Divine Ordinance (Sacral Fidelity Reading)").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, 'ed4bc041-e5aa-447d-a4d6-008b50f374b0').
narrative_ontology:cs_kernel_codification('ed4bc041-e5aa-447d-a4d6-008b50f374b0', fixed_text).
narrative_ontology:cs_authority_grounding('ed4bc041-e5aa-447d-a4d6-008b50f374b0', lineage).
narrative_ontology:cs_interpretation_layer_present('ed4bc041-e5aa-447d-a4d6-008b50f374b0').
narrative_ontology:cs_reading_relation('ed4bc041-e5aa-447d-a4d6-008b50f374b0', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed4bc041-e5aa-447d-a4d6-008b50f374b0', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_axiom('ed4bc041-e5aa-447d-a4d6-008b50f374b0', foundational, lycurgan_laws_cosmically_immutable).
narrative_ontology:cs_axiom_status(lycurgan_laws_cosmically_immutable, holdable).
narrative_ontology:cs_axiom_grounding('ed4bc041-e5aa-447d-a4d6-008b50f374b0', lycurgan_laws_cosmically_immutable, theological).
narrative_ontology:cs_axiom('ed4bc041-e5aa-447d-a4d6-008b50f374b0', foundational, immutability_as_political_virtue).
narrative_ontology:cs_axiom_status(immutability_as_political_virtue, holdable).
narrative_ontology:cs_axiom_grounding('ed4bc041-e5aa-447d-a4d6-008b50f374b0', immutability_as_political_virtue, deontological).
narrative_ontology:cs_reference_frame('ed4bc041-e5aa-447d-a4d6-008b50f374b0', lycurgan_divine_ordinance).
narrative_ontology:cs_drift_state('ed4bc041-e5aa-447d-a4d6-008b50f374b0', spartan_decline_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed4bc041-e5aa-447d-a4d6-008b50f374b0', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_traditionist_faction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_citizen_body).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, oracular_priesthood).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartan_citizen_body).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helot_underclass).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers the Lycurgan system as divinely ordained and immutable. Maintains ritual performance of the laws, controls pedagogical transmission to youth, and adjudicates which practices conform to the founding ordinance. Their authority rests on claiming direct lineage to Lycurgus and possession of his true intent. Frames any proposed change as sacrilege and cultural dissolution.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_traditionist_faction, agenda_setter,
    institutional, civilizational, identity_locked, local).

% Subject to the laws' demands: military training regimen, property redistribution, mandatory military service, regulated marriage and reproduction, austere consumption norms, and communal dining. They experience these as binding duty and divine obligation. They also receive social stability, clear status hierarchy, and membership in what is framed as an eternal, God-sanctioned community. Exit means loss of citizenship and belonging.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_citizen_body, payer,
    organized, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartan_citizen_body, beneficiary).

% Bear the material cost of the system: their agricultural surplus feeds the citizen body, enabling their freedom from economic production and availability for military training. They have no voice in interpretation or revision of the laws. The system's immutability is enforced partly through their enserfment.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_underclass, payer,
    powerless, generational, trapped, local).

% Validates the laws' divine origin through Delphic oracle pronouncements attributed to Apollo. Their authority to certify the constraint's sacral status is essential to its binding force. They benefit from the political stability the constraint provides and from their position as gatekeepers of legitimacy. They have constrained exit: breaking with the Lycurgan framing would cost them influence.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, oracular_priesthood, beneficiary,
    powerful, civilizational, constrained, regional).

% From Athens and other poleis, articulate arguments for constitutional revision, adaptive law-making, and democratic deliberation. They are excluded from Spartan political discourse: revisionist speech is treated as corrosive sophistry incompatible with sacred order. They would argue for treating the laws as human construction open to reasoned change; their voices are structurally silenced within Spartan institutional spaces.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, revisionist_philosophers, excluded,
    powerful, biographical, mobile, continental).

% Examine the historical record of Lycurgus, the actual laws, and their evolution over time. From this analytical seat, the claim of immutable divine origin conflicts with evidence of incremental adaptation and periodic revision (ephorate expansion, kingship modifications). They see through the sacral framing to the human political choices underneath, but their analytical perspective does not participate in Spartan legitimacy production.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_archaic_historians, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining a unified warrior society across generations: shared military training, redistributive property norms, and communal institutions bind the citizen body together against internal factionalism and external conquest. The claim is that the laws achieve this by standing outside human revision — unchangeability is the coordination mechanism itself.
% TRANSFER_FUNCTION: Moves agricultural and material surplus from the helot underclass to the citizen body, enabling their freedom from economic production and year-round military readiness. Moves authority to interpret and administer these distributions from the citizen body to the traditionist faction, who claim possession of Lycurgus's intent. Moves legitimacy validation to the oracular priesthood, who authenticate the divine origin.
% ABSENT_VOICES: Helots have no voice in the political order and no seat at any table where the laws are discussed or ratified. Revisionist philosophers are excluded by institutional closure — they have foreign domicile and sophistic credentials incompatible with Spartan sacred order. Subject youths born into the system have no choice point: they receive the laws as already-existing sacred fact, not as something their generation might deliberate.
% DISAPPEARANCE_RATIONALE: If the Lycurgan constraint and its claim to sacred immutability vanished overnight, Spartan political life would bifurcate: the citizen body would enter into constitutional deliberation about property, military organization, and helot status; power would diffuse from the traditionist faction to wider assemblies; the basis of the helot system would become visible as a human choice rather than cosmic ordinance. The entire social order would reorganize.
% FOUNDING_PROBLEM: Maintain unified warrior identity and military discipline across generations without the fragmentation that afflicted other Greek city-states; prevent wealthy families from undermining collective military readiness through private luxury; keep the citizen body loyal to a redistributive system that limits personal wealth accumulation.
% FOUNDING_PROBLEM_CORROBORATION: Spartan traditionist sources attest the problem is perennially live: luxury always threatens unity, citizen vice always tempts departure from the laws, and only absolute fidelity preserves the ancestral order. The oracular priesthood issues periodic pronouncements renewing the sacred binding of the laws. However, no source outside the benefiting parties (traditionist faction and priesthood) attests that the founding problem is live in its original form — Athenian observers and revisionist philosophers argue Sparta achieved its coordination goal centuries ago and now persists through inertia; demographic analysts argue the laws have become unfit for material reality. This reading asserts the problem is live; the demographic_trap_reading contests it.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.15 at interval end) because this reading treats the constraint as a natural law, not as institutional extraction. The sacral framing is understood as an accurate representation of cosmic fact, not as a cover story. Suppression is also low (0.08) — the constraint persists because citizens genuinely believe in its cosmic necessity, not because they are coerced into compliance. Theater ratio is minimal (0.12) — the ritual performance of the laws is treated as authentic observance, not as performative maintenance of a degraded function. Accessibility collapse is very high (0.92) — once Spartans accept that the laws are divinely immutable, alternatives (revision, adaptive amendment) become unthinkable. Resistance is near-zero (0.04) — the traditional citizen body does not resist laws they experience as cosmic ordinance. The measurement series shows very slight drift over the interval: minor increases in theater_ratio and suppression_requirement as the constraint ages, consistent with small incremental costs to maintaining the framing, but the overall trajectory is flat — the constraint is stable and unrevisable by definition. One shared time grid ensures every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The traditionist faction and the citizen body should compute this constraint identically from the sacral-fidelity frame: both see immutability as virtue, decline as citizen failure. The oracular priesthood computes similarly, though with added agency (they certify). The helots, if they could author a perspective, would likely experience this as snare-like (they pay, cannot exit, have no voice) — but the sacral framing prevents their perspective from reaching legitimacy-production spaces. The revisionist philosophers and historians compute this as a false summit or worse: they see the sacral framing as instrumental naturalization covering up human political choice. The engine computes per-seat directionality from beneficiary/victim + exit: the traditionist faction is a beneficiary (controls interpretation, derives prestige and authority); the helots are victims (trapped, pay material cost, have no voice); the citizen body sits near symmetric (genuine coordination benefit mixed with mandatory submission). The authored claim is mountain; the presence of beneficiaries triggers false-summit-mountain evaluation.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares one beneficiary group: the spartan_traditionist_faction. They benefit from the constraint by monopolizing interpretation authority, deriving prestige from lineage claims to Lycurgus, and preserving a stable hierarchical order that confirms their power. The helot_underclass is structurally victimized: they bear the material cost through enserfment, have zero voice in revision or reinterpretation, and cannot exit (trapped). The citizen body is dual-positioned (secondary_role acknowledged): they genuinely benefit from the coordination the constraint provides (stable, unified warrior society), but they also pay through mandatory military service, property redistribution, austere consumption, and loss of individual economic choice. Their exit_options are 'trapped' because leaving Sparta means loss of citizenship and identity. The traditionists have identity_locked exit because challenging the Lycurgan framing would dissolve their authority and institutional role — their entire position rests on claiming possession of Lycurgus's intent. Directionality derivation would yield: beneficiary faction near d=0.0 (they collect authority and prestige with minimal cost); helots near d=1.0 (they pay and cannot exit); citizen body near d=0.5 (symmetric coordination benefit and symmetric cost). No overrides are needed: the structural data is coherent.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the Lycurgan constraint as a living founding problem with live status: the coordination problem of maintaining unified warrior society across generations persists, and the laws remain the solution. The sacral-fidelity reading would not classify this as mandatrophy — the mandate (maintain a unified, militarized citizen body) is still operative, and the constraint still serves it. However, the presence of beneficiaries (traditionist faction) on a mountain constraint is a false-summit-mountain candidate: if the constraint's binding force depends on the sacral framing, and the sacral framing benefits the traditionist faction by concentrating interpretation authority, then the constraint may be a tangled_rope (beneficiaries coordinating via natural-law framing) rather than a true mountain. The omega variables document this ambiguity. The demographic_trap_reading (constraint family sibling) would argue that the founding problem is dead (Sparta declined, the coordination failed) and the constraint persists inertially — that would trigger mandatrophy. This reading asserts the founding problem is live and the constraint successfully solves it; the divergence between readings is exactly the empirical-historical disagreement the corpus exists to track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacral_vs_constructed_distinction,
    'Is the Lycurgan constraint a genuinely immutable natural law of political ordering, or a constructed institutional system whose sacralization serves to block revision?',
    'Forensic examination of the historical Lycurgus figure and dating of the laws; comparison of Spartan institutions across time periods to detect covert revision; analysis of whether the constraint''s binding force persists if the divine framing is removed.',
    'If the constraint emerges naturally from political physics independent of the sacral framing, it is a mountain. If the binding force dissolves when the divine origin claim is challenged, then the sacral framing is the extraction mechanism, and the constraint is a false summit (beneficiary faction controls the narrative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacral_vs_constructed_distinction, conceptual, 'Whether sacral immutability is intrinsic or instrumental to the constraint''s persistence.').

omega_variable(
    virtue_vs_victimization_ambiguity,
    'Does Spartan decline flow from citizen failure to maintain fidelity to the sacred laws, or from structural inability to revise unrealistic constraints in response to demographic and material change?',
    'Demographic modeling of sustainable birth rates under Lycurgan marriage regulation; comparative institutional analysis of adaptive-capacity in systems with revision mechanisms; analysis of whether the laws'' prescriptions were internally consistent.',
    'If decline is primarily attributable to citizen vice, the sacral immutability framing is vindicated. If decline is attributable to constraints that could not adapt, then immutability is a trap, not virtue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(virtue_vs_victimization_ambiguity, empirical, 'Whether Spartan decline was caused by violation of unchangeable law or by the unchangeability itself.').

omega_variable(
    oracular_independence_and_capture,
    'Do the oracular priesthood''s pronouncements certifying Lycurgan divine origin reflect independent access to divine will, or represent institutional capture by Spartan leadership?',
    'Historical analysis of Delphic pronouncements on Spartan matters; examination of whether oracle statements shifted when Spartan interests shifted; comparison of Delphic outputs across poleis to detect bias patterns.',
    'If the oracle is captured, the priesthood is a beneficiary-faction agent using divine authority instrumentally. If independent, its pronouncements carry weight for the constraint''s naturality claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracular_independence_and_capture, empirical, 'Whether the oracular priesthood certifies the constraint independently or under institutional pressure.').

omega_variable(
    beneficiary_faction_genuine_belief,
    'Does the traditionist faction genuinely believe in the sacral immutability of the laws, or do they use the framing as a cover story to preserve power?',
    'Examination of private correspondence and institutional behavior during crisis periods; willingness to discuss revision options in non-public settings; archaeological evidence of whether revision occurred but was masked as fidelity.',
    'If the faction genuinely believes, the constraint is more likely to be a true mountain. If the faction instrumentalizes while privately acknowledging revisability, the constraint becomes a false summit (beneficiary naturalization of a constructed system).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_faction_genuine_belief, empirical, 'Whether the sacral framing reflects genuine epistemological commitment or strategic naturalization by beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(lycu_tr_t0, observed).
narrative_ontology:measurement(lycu_tr_t5, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(lycu_tr_t5, observed).
narrative_ontology:measurement(lycu_tr_t10, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(lycu_tr_t10, observed).
narrative_ontology:measurement(lycu_tr_t15, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(lycu_tr_t15, observed).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(lycu_tr_t20, observed).
narrative_ontology:measurement(lycu_tr_t25, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(lycu_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(lycu_be_t0, observed).
narrative_ontology:measurement(lycu_be_t5, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement_basis(lycu_be_t5, observed).
narrative_ontology:measurement(lycu_be_t10, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(lycu_be_t10, observed).
narrative_ontology:measurement(lycu_be_t15, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement_basis(lycu_be_t15, observed).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(lycu_be_t20, observed).
narrative_ontology:measurement(lycu_be_t25, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement_basis(lycu_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(lycu_su_t0, observed).
narrative_ontology:measurement(lycu_su_t5, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 5, 0.07).
narrative_ontology:measurement_basis(lycu_su_t5, observed).
narrative_ontology:measurement(lycu_su_t10, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement_basis(lycu_su_t10, observed).
narrative_ontology:measurement(lycu_su_t15, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement_basis(lycu_su_t15, observed).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(lycu_su_t20, observed).
narrative_ontology:measurement(lycu_su_t25, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement_basis(lycu_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__sacral_fidelity_reading, 0.08).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The Lycurgan-laws kernel decomposes into three structurally distinct constraint readings: sacral_fidelity_reading (this story) treats the laws as genuinely immutable and naturally ordered; adaptive_fiction_reading treats sacral immutability as a noble lie masking incremental revision; demographic_trap_reading treats unrevisability as a causal factor in Spartan decline. All three readings interpret the same historical record but from different epistemological commitments. The ε values differ substantially: sacral_fidelity minimizes extraction (natural law); adaptive_fiction treats extraction as moderate to high (beneficiaries using deception); demographic_trap treats extraction as moderate (constraint persisting despite misfit). Each reading has distinct beneficiaries and victims, distinct stakeholder structures, and distinct classifications. They are linked as sibling readings of one kernel, not as perspectives on a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
