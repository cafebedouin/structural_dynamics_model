% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Blood-Feud as Sin: The Christianized Pacification Reading of the Feud Kernel
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This story instantiates the christianized_pacification_reading of the
 *   feud_obligation_kernel: the claim, advanced by ecclesiastical and allied
 *   royal authorities across the early and high medieval period, that
 *   blood-feud violence violates divine law reserving vengeance to God, and
 *   that legitimate coercive force can only be exercised through delegated
 *   ecclesiastical or royal channels. Under this reading every feud
 *   participant — vengeance-taker and composition-seeker alike — is placed in
 *   spiritual peril, since even honor-driven violence usurps a prerogative
 *   that belongs to God alone. The Church (and its royal allies) is the
 *   structural beneficiary: it acquires interpretive monopoly over what
 *   counts as legitimate violence, expanded jurisdiction over disputes, and
 *   the machinery of penance, excommunication, and fee-bearing adjudication.
 *   The reading seeks not partial reform of feud practice but its complete
 *   suppression through penitential discipline — a stronger ambition than the
 *   extraction_cycle_reading's economic critique or the
 *   stateless_coordination_reading's recognition of feud's internal deterrent
 *   logic. This story authors ONLY the christianized_pacification_reading;
 *   the sibling readings are separate constraints
 *   (stateless_coordination_reading, extraction_cycle_reading) linked via
 *   network.affects_constraints, each with its own epsilon and stakeholder
 *   structure.
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: primary agenda-setter and beneficiary (institutional/arbitrage) — defines the sin, administers penance, gains jurisdiction
 *   - allied_royal_authority: co-beneficiary (institutional/arbitrage) — gains monopoly claim on lawful force
 *   - feuding_kindreds: primary target (organized/constrained) — traditional practice reclassified as mortal sin
 *   - warrior_aristocracy: secondary target (powerful/constrained) — status derived from feud capacity now delegitimized
 *   - wronged_families_seeking_composition: bears displaced remedy costs (moderate/trapped)
 *   - parish_clergy: local agenda-setter and beneficiary (moderate/constrained) — administers doctrine at ground level
 *   - customary_law_specialists: excluded voice (moderate/trapped) — entire adjudicative tradition delegitimized without consultation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.61).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.79).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Blood-Feud as Sin: The Christianized Pacification Reading of the Feud Kernel").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '44bad332-f4aa-4516-bcc3-e019d964b290').
narrative_ontology:cs_kernel_codification('44bad332-f4aa-4516-bcc3-e019d964b290', fixed_text).
narrative_ontology:cs_authority_grounding('44bad332-f4aa-4516-bcc3-e019d964b290', lineage).
narrative_ontology:cs_interpretation_layer_present('44bad332-f4aa-4516-bcc3-e019d964b290').
narrative_ontology:cs_reading_relation('44bad332-f4aa-4516-bcc3-e019d964b290', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('44bad332-f4aa-4516-bcc3-e019d964b290', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('44bad332-f4aa-4516-bcc3-e019d964b290', foundational, vengeance_reserved_to_divine_authority).
narrative_ontology:cs_axiom_status(vengeance_reserved_to_divine_authority, holdable).
narrative_ontology:cs_axiom_grounding('44bad332-f4aa-4516-bcc3-e019d964b290', vengeance_reserved_to_divine_authority, theological).
narrative_ontology:cs_axiom('44bad332-f4aa-4516-bcc3-e019d964b290', foundational, legitimate_violence_requires_delegated_institutional_sanction).
narrative_ontology:cs_axiom_status(legitimate_violence_requires_delegated_institutional_sanction, holdable).
narrative_ontology:cs_axiom_grounding('44bad332-f4aa-4516-bcc3-e019d964b290', legitimate_violence_requires_delegated_institutional_sanction, conventional).
narrative_ontology:cs_reference_frame('44bad332-f4aa-4516-bcc3-e019d964b290', apostolic_prohibition_on_private_vengeance).
narrative_ontology:cs_drift_state('44bad332-f4aa-4516-bcc3-e019d964b290', high_medieval_penitential_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44bad332-f4aa-4516-bcc3-e019d964b290', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, allied_royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feuding_kindreds).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, warrior_aristocracy).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, wronged_families_seeking_composition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, parish_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines vengeance-taking as mortal sin usurping God's exclusive prerogative over retribution ('vengeance is mine, saith the Lord'), issues penitentials assigning years of fasting and exile for feud killings, and excommunicates unrepentant feuders. Positions itself and its royal allies as the sole legitimate channel through which violence may be authorized — via ordeal, judicial combat under ecclesiastical witness, or royal ban. Gains jurisdiction over disputes previously settled kin-to-kin, along with fees, land grants, and confession-derived intelligence about local power structures.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy, beneficiary).

% Partners with the Church to declare feud violence illegitimate self-help, redirecting disputes toward royal courts and the king's peace. Gains a monopoly claim on lawful force that the feud system had previously denied it, at the cost of sharing legitimacy-granting authority with ecclesiastical courts.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, allied_royal_authority, beneficiary,
    institutional, generational, arbitrage, national).

% Practice vengeance and composition (wergild) as their traditional means of restoring honor and balance after killings. Under this reading, every feud act — even one their own custom regards as obligatory and honorable — is redefined as sin placing the kinsman's soul in peril, subject to penance, exile, or damnation. Their exit from the feud logic is blocked twice over: socially, by kin obligation, and now spiritually, by the doctrine that even refusing vengeance without ecclesiastical mediation may itself be inadequate.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feuding_kindreds, payer,
    organized, generational, constrained, regional).

% Derive status, retinue loyalty, and political standing from capacity to prosecute feuds. The doctrine strips this capacity of legitimacy, forcing a choice between spiritual condemnation and submission to ecclesiastical/royal adjudication that diminishes their independent standing and requires payment (penance, fines, land grants to churches) to be absolved.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, warrior_aristocracy, payer,
    powerful, biographical, constrained, regional).

% Have suffered a killing or injury and would, under the older logic, be entitled to vengeance or wergild negotiated among kin. Under this reading they must instead seek remedy through ecclesiastical or royal channels, which may be slower, more distant, costlier (court fees, travel, patronage), and may not recognize the harm in terms the family finds satisfying. Declining to pursue the church-sanctioned path risks being cast as sinful vengeance-seekers themselves.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, wronged_families_seeking_composition, payer,
    moderate, biographical, trapped, local).

% Administer confession, penance, and local excommunication threats against feuding parishioners. Their livelihood and local standing are tied to the doctrine's enforcement; they mediate between the hierarchy's universal claim and the concrete feud disputes of their communities, gaining informal power as arbiters.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, parish_clergy, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, parish_clergy, beneficiary).

% The lawspeakers, elders, and kin-group arbiters who traditionally adjudicated feud settlements (composition schedules, oath-helpers, honor restoration) are structurally sidelined by a doctrine that treats their entire adjudicative framework as theologically illegitimate. They are not consulted in the construction of penitential doctrine and have no seat in ecclesiastical courts.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, customary_law_specialists, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine coordinates a transfer of dispute-resolution authority from decentralized kin networks to a unified ecclesiastical-royal framework, in principle ending self-perpetuating cycles of retaliatory killing by substituting divinely-sanctioned, centrally adjudicated remedy for private vengeance.
% TRANSFER_FUNCTION: Moves jurisdictional authority, adjudication fees, land grants, and confession-derived social leverage from kin-group elders and warrior aristocrats to bishops, abbots, and allied kings; moves the locus of legitimate violence from the feuding kindred to the ecclesiastical-royal apparatus.
% ABSENT_VOICES: Customary law specialists (lawspeakers, kin-group elders) who built and operated the feud's own internal justice logic are not party to the theological reclassification of their entire system as sin; women within feuding kindreds, who often negotiated compositions and marriage alliances that resolved feuds, have no voice in the new penitential framework, which addresses feud primarily as a matter for male kin-heads and confessors.
% DISAPPEARANCE_RATIONALE: The Church and allied crown would insist the world rearranges catastrophically — a return to 'lawless' vengeance-taking, they claim. Feuding kindreds and customary law specialists would argue their own dispute-resolution machinery (composition, oath-helping, honor restoration) continued functioning throughout and beneath the doctrine regardless, and its removal would mainly strip away an added layer of spiritual coercion and jurisdictional fee-collection rather than remove justice itself.
% FOUNDING_PROBLEM: Endemic cycles of retaliatory killing between kin groups were destabilizing to social peace, land productivity, and any project of centralized political consolidation; a mechanism to interrupt and delegitimize the feud logic was sought.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chroniclers and royal charters (produced by the beneficiary parties themselves) attest the founding problem as ongoing lawlessness requiring divine remedy. Independent corroboration is thin: legal-anthropological reconstruction of surviving customary law codes (e.g. early Icelandic and Germanic law texts) suggests feud systems already possessed internal deterrence and settlement logic prior to Christianization, and no source outside the Church/crown alliance attests that feud violence was, on its own terms, spiritually or socially disordered before being redefined as such.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, contested).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that the doctrine's suppression of feud logic is not costless pacification but a jurisdictional transfer: composition fees, penance payments, land grants, and confession-derived leverage flow toward Church and allied crown, while feuding parties lose both their traditional remedy mechanism and the option to simply continue as before without incurring spiritual and often material penalty. Suppression (0.79) is high and rises sharply over the measured interval (0.40 to 0.79) as penitential codes formalize, excommunication becomes a routinely wielded instrument, and royal law increasingly criminalizes private vengeance alongside ecclesiastical condemnation — enforcement hardens over roughly four centuries as the doctrine moves from occasional preaching to codified penitential and legal apparatus. Theater ratio is moderate-low and slowly rising (0.12 to 0.28): the doctrine has real suppressive teeth (excommunication, wergild redirection, royal ban) but an increasing share of activity — elaborate penitential taxonomies, symbolic pilgrimage penances — is performative relative to the underlying aim of actually stopping killings, which continued long after the doctrine's promulgation in many regions.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical hierarchy and allied royal authority sit at the beneficiary end: they set the terms of legitimate violence, collect the resulting fees and jurisdiction, and face essentially no risk from the doctrine's operation (arbitrage exit — they can reshape the doctrine's application as political circumstance demands). Feuding kindreds, warrior aristocracy, and wronged families sit at the target end: their traditional remedy mechanism is reclassified as sin, and their exit options are constrained or trapped — kin obligation binds them to feud roles even as the new doctrine punishes fulfilling those roles, and abandoning feud entirely offers no clean alternative remedy path for families actually wronged. Parish clergy occupy an intermediate position: locally powerful as enforcers, but personally bound to a rural community whose disputes they cannot simply walk away from.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberate: this reading does possess a genuine coordination story — interrupting cycles of retaliatory killing that could otherwise escalate indefinitely is a real problem, and *some* families do gain access to less violent remedy channels. But it requires active, escalating enforcement (excommunication, royal ban, penitential taxonomy) and systematically transfers jurisdiction and fee-income to the coordinating parties while imposing a spiritual and material penalty structure on kin groups whose own composition/vengeance logic had operated (per the sibling stateless_coordination_reading) with its own deterrent equilibrium. Calling this pure Rope would erase the extraction; calling it pure Snare would erase the real (if overstated) pacification function the Church could plausibly claim credit for in regions where feud violence had in fact become socially destabilizing. The founding_problem mismatch check applies directly here: founding_problem_status is authored 'contested' because status=dead-ish evidence (customary systems already had internal deterrence) sits against disappearance_verdict=contested — this is exactly the capture-flag pattern the engine's mismatch consumer is built to surface, not a claim this story resolves on its own authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_pacification_vs_jurisdictional_capture,
    'Did the Christianized doctrine of vengeance-as-sin genuinely reduce feud violence and social harm, or did it primarily function as a jurisdictional capture mechanism that redirected dispute-resolution authority and fee income to ecclesiastical and royal institutions while feud violence continued under different labels (judicial combat, royally-sanctioned reprisal)?',
    'Comparative regional analysis of homicide/feud-killing rates before and after intensive penitential enforcement, cross-checked against Church and royal court revenue records from wergild/composition redirection, in regions with varying enforcement intensity.',
    'If pacification effects were substantial and durable, the tangled_rope classification''s coordination component is well-supported; if violence continued largely unabated while jurisdiction and revenue shifted, the constraint tips toward snare with the pacification narrative as pure cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_pacification_vs_jurisdictional_capture, empirical, 'Whether the doctrine''s professed pacification function was real or primarily a cover for jurisdictional/fiscal capture.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the christianized_pacification_reading the historically dominant self-understanding of the feud-suppression project (i.e. how the Church and courts themselves narrated it), or is it one contested framing among genealogically co-equal readings (stateless_coordination_reading, extraction_cycle_reading) with no single reading entitled to primacy?',
    'Textual analysis of penitential literature, capitulary law, and conciliar decrees to establish which framing dominates the primary sources versus which framings are retrospective analytical impositions (legal-anthropological or economic-historical).',
    'If the theological framing dominates primary sources, this reading''s authority_grounding as lineage-based doctrine is well-founded; if it is largely a retrospective gloss on what was functionally a jurisdictional power grab, the reading''s own self-description (divine law, spiritual peril) would itself be evidence for, not against, the extraction_cycle_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether this reading reflects the historical actors'' genuine self-understanding or is analytically retrofitted primacy among co-equal kernel readings.').

omega_variable(
    spiritual_peril_as_natural_law_or_construct,
    'Is the doctrine that vengeance belongs exclusively to God a genuine theological/moral discovery binding on all Christians regardless of institutional interest, or a constructed doctrine whose content was shaped by the interpretive monopoly it granted to the institutions promulgating it?',
    'Comparative theology: examine whether structurally similar ''vengeance belongs to God'' doctrines arose independently in traditions without a parallel institutional beneficiary, and whether the doctrine''s specific content (which forms of violence are exempted for royal/ecclesiastical actors) tracks institutional interest.',
    'If the doctrine''s specific content systematically exempts violence performed by the beneficiary institutions themselves (crusade, royal war, judicial execution), that asymmetry supports reading the doctrine as constructed to the institutions'' advantage rather than as even-handed moral discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_peril_as_natural_law_or_construct, conceptual, 'Whether the vengeance-belongs-to-God doctrine is theologically independent of, or shaped by, the institutional interests it serves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(feud_tr_t160, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement(feud_tr_t240, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 240, 0.23).
narrative_ontology:measurement(feud_tr_t320, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 320, 0.26).
narrative_ontology:measurement(feud_tr_t400, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 400, 0.28).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(feud_be_t160, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 160, 0.49).
narrative_ontology:measurement(feud_be_t240, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 240, 0.55).
narrative_ontology:measurement(feud_be_t320, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 320, 0.59).
narrative_ontology:measurement(feud_be_t400, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 400, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement(feud_su_t160, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 160, 0.63).
narrative_ontology:measurement(feud_su_t240, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 240, 0.71).
narrative_ontology:measurement(feud_su_t320, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 320, 0.76).
narrative_ontology:measurement(feud_su_t400, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 400, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__christianized_pacification_reading, 0.1).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of feud_obligation_kernel. christianized_pacification_reading (this story) authors the theological/jurisdictional framing as tangled_rope with epsilon=0.61, Church/crown as beneficiaries, all feud participants as victims via spiritual peril, and complete-suppression intent. stateless_coordination_reading authors feud as a functioning coordination mechanism (expected lower epsilon, feuding kindreds partially reclassified as beneficiaries of a working deterrence system). extraction_cycle_reading authors feud as a destructive extraction cycle from a secular/economic lens (distinct beneficiary/victim structure centered on territorial consolidation rather than ecclesiastical jurisdiction). The three stories share the same underlying practice (blood-feud) but diverge in epsilon, claimed_type, and stakeholder structure because they instantiate structurally different constraints per the epsilon-invariance principle — not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
