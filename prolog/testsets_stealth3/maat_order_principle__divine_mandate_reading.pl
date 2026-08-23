% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle_divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Divine Mandate Reading of the Ma'at Order Principle
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   maat_order_principle: the divine mandate reading, in which Ma'at flows
 *   downward from the creator's cosmic order through the kingship office to
 *   society, and the reigning king embodies Ma'at such that he cannot violate
 *   it by definition. That definitional clause is the reading's structural
 *   signature: it places the apex outside the constraint set as source rather
 *   than subject, so no Ma'at claim can ever bind royal action, and every
 *   levy the crown raises is warranted as cosmic necessity. The colloquial
 *   label 'Ma'at' decomposes into three structurally distinct readings — this
 *   one, a reciprocity reading in which the king owes justice and provision,
 *   and a distributed-maintenance reading in which every station sustains
 *   order — with different epsilon values, different victim sets, and
 *   different classifications; they are separate stories linked by network
 *   edges, not one constraint viewed from multiple angles. Claimed type and
 *   metrics are authored independently: I claim tangled_rope because a real
 *   coordination substrate (basin-wide flood management) carries an
 *   asymmetric, actively enforced flow that the definitional clause shields
 *   from correction; the metrics describe that operation without being tuned
 *   to any predicted verdict. The doctrine's self-presentation as natural
 *   cosmic law is its own rhetoric, not structural fact: identifiable
 *   beneficiaries exist and the enforcement machinery is human, so mountain
 *   is rejected from the authoring seat.
 *
 * KEY AGENTS:
 *   - pharaonic_crown: Primary beneficiary and agenda-setter (institutional/identity_locked) — source of the flow, definitionally immune to Ma'at claims
 *   - priestly_establishment: Secondary beneficiary and ritual administrator (institutional/constrained) — collects endowments while performing the maintenance that keeps the structure credible
 *   - royal_administration_elite: Beneficiary-executor (powerful/constrained) — staffs the levy machinery in exchange for rank and memorial
 *   - corvee_laborers: Primary target (powerless/trapped) — bears conscripted labor with no appellate surface
 *   - grain_tax_farming_households: Target with residual relief entitlement (powerless/constrained) — pays in kind, owed return flows controlled by the collector
 *   - foreign_war_captives: Target outside any claim structure (powerless/trapped) — subjugation narrated as the king defeating chaos
 *   - intermediate_period_witness_elites: Excluded critic seat (organized/constrained) — articulates reciprocity objections without standing to indict the king's person
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.7).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.75).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Divine Mandate Reading of the Ma'at Order Principle").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f').
narrative_ontology:cs_kernel_codification('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', distributed).
narrative_ontology:cs_authority_grounding('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', extraction).
narrative_ontology:cs_interpretation_layer_present('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f').
narrative_ontology:cs_reading_relation('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', foundational, royal_embodiment_of_maat).
narrative_ontology:cs_axiom_status(royal_embodiment_of_maat, holdable).
narrative_ontology:cs_axiom_grounding('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', royal_embodiment_of_maat, theological).
narrative_ontology:cs_axiom('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', secondary, maat_transmission_unidirectional).
narrative_ontology:cs_axiom_status(maat_transmission_unidirectional, holdable).
narrative_ontology:cs_axiom_grounding('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', maat_transmission_unidirectional, theological).
narrative_ontology:cs_reference_frame('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', creator_ordained_continuous_kingship).
narrative_ontology:cs_drift_state('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', intermediate_period_succession_breaks, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('486c1e85-e7b1-4c06-8d46-e9a6ab0dc41f', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_crown).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_establishment).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, royal_administration_elite).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, corvee_laborers).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, grain_tax_farming_households).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, foreign_war_captives).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, divine_kingship_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, maat_cosmic_order_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reigning king renews the embodiment claim at coronation, Sed festivals, and daily temple offering; he commands taxation, corvee levies, and war under the warrant that his action is cosmic order taking effect. He cannot renounce the claim without unmaking the legitimacy of his own office; abdication, self-indictment, and submission to judgment are all unavailable moves from inside the role.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaonic_crown, agenda_setter,
    institutional, generational, identity_locked, national).

% Temple hierarchies perform the daily rites that keep the cosmos running, increasingly standing in for the king by proxy; they hold endowed lands, tithe exemptions, and sole authority over calendar, omen, and funerary provision. Their estates and standing depend on the cult economy the doctrine funds; walking away means surrendering income, office, and sacral rank.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priestly_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, priestly_establishment, agenda_setter).

% Scribes, nomarchs, and courtiers staff the granaries, levy quotas, and labor bureaus; promotion, tomb construction, and burial provision flow from closeness to the palace. Their tomb inscriptions profess personal uprightness while their bureaus collect the levies; declining office forfeits rank, income, and memorial.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, royal_administration_elite, beneficiary,
    powerful, biographical, constrained, national).

% Farming households owe seasonal labor quotas for canals, pyramids, and temple works, levied by district and enforced against kin; rations are minimal and flight is punishable. There is no forum in which their objection could count against a king who cannot err.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, corvee_laborers, payer,
    powerless, immediate, trapped, regional).

% Households deliver surplus grain to state granaries after the flood harvest; in famine years they are owed return flows from the same stores, but the stores answer to the palace that collected them. Moving to another district means abandoning land tenure and neighbors.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, grain_tax_farming_households, payer,
    powerless, biographical, constrained, regional).

% Captives from Nubian, Libyan, and Levantine campaigns are assigned to royal estates, mines, and building gangs; their capture is narrated as the king striking down chaos. They hold no standing in any court and the routes out of the valley cross open desert.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, foreign_war_captives, payer,
    powerless, immediate, trapped, continental).

% Provincial literati and retired officials who lived through succession breaks and low floods composed laments and prophecies diagnosing royal failure and pleading for justice toward the weak. Their genre could mourn disorder and advise the great, but could not indict the reigning king's person; they stood outside the councils where the embodiment claim is renewed.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, intermediate_period_witness_elites, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaonic_crown).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Basin-wide management of the Nile's annual flood: centralized grain collection and storage, famine buffering, canal and dike maintenance, and mobilization of labor at scales no village coalition could achieve alone.
% TRANSFER_FUNCTION: Moves surplus grain, seasonal labor, and war captives from farming households and conquered populations to the palace, temple estates, and administrative elite, under the warrant that the flow sustains cosmic order itself.
% ABSENT_VOICES: Corvee laborers and taxed farmers had no standing forum; provincial literati voiced grief over disorder but could not indict the king's person; foreign captives were wholly voiceless. The near-unanimity of the surviving record reflects who was permitted to write, not consent.
% DISAPPEARANCE_RATIONALE: Tax collection loses its warrant, labor mobilization collapses to local scale, temple economies lose their funding claim, and the kingship office itself loses its constituting story. The state as built cannot survive overnight loss of the doctrine; something functionally equivalent would have to be reconstructed before basin-wide works could resume.
% FOUNDING_PROBLEM: After unification, Upper and Lower Egypt needed a legitimacy formula that could bind two kingdoms to one center and secure basin-wide cooperation against the Nile's volatility; the doctrine answered why obey this particular center by locating obedience in cosmic structure.
% FOUNDING_PROBLEM_CORROBORATION: The underlying coordination need is attested from outside the benefiting parties: flood records, settlement archaeology, and the scale of hydraulic works confirm basin-wide management was objectively required. Wisdom-literature scribes corroborate the legitimacy problem from a critical seat. The crown's specific answer — that the king embodies order and cannot err — is attested only by its own beneficiaries; no source outside the palace-temple complex asserts the embodiment clause.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the transfer is decoupled from need: levies scale with royal project ambition, and the definitional clause removes the correction mechanism that reciprocity framings retain. Suppression (0.75) is a raw structural property, unscaled by power or scope: dissent against the king's person is definitionally alignment with chaos, so the doctrine itself manufactures the cost of criticism. Theater_ratio (0.48) reflects proxy ritual: by the later Old Kingdom the king's daily embodiment performances were routinely delegated to priests, so a growing share of maintenance activity is liturgical routine rather than royal act. Accessibility_collapse is moderate (0.45): alternatives were never fully foreclosed in practice — reciprocity language persists in official benefaction inscriptions and wisdom texts — but they could not attach to the king's person. Resistance (0.42) shows in peasant flight from levies, succession-era legitimacy contests, and the critical wisdom corpus. The measurement series share one seven-point grid (t=0..60 in dynastic-scale units spanning Old Kingdom consolidation, the First Intermediate collapse, and Middle Kingdom reconsolidation); all three tracked metrics are authored at every point. The series are cyclical, not monotonic: extraction and enforcement build under a strong center, collapse with it, and are rebuilt by a new founder who re-narrates restoration of order. The cycle is driven by hydraulic-political dynamics rather than engineered as intermittent reinforcement, though the doctrine's re-narration function is precisely what lets extraction restart after each collapse. base_properties report the T=60 recovery-phase state.
 *
 * PERSPECTIVAL GAP:
 *   The crown seat and the laboring seats should compute opposites. From the throne, the arrangement is not a constraint at all: the king is the source of the order others are bound by, his exit is identity_locked because the role constitutes him, and his directionality sits at the beneficiary pole. From the corvee and tax seats the same structure is a one-way enforced flow with no appellate surface — full-target directionality with trapped or constrained exits. The priestly seat is genuinely dual: it collects endowments and exemptions (beneficiary-side position) while performing the maintenance that keeps the structure credible (agenda-setter-side duty), so its computed position should sit nearer the beneficiary pole than its administrative labor alone would suggest. The excluded literati seat sees the full structure — including the gap between professed justice and levied burden — but has no standing to convert observation into correction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: pharaonic_crown (captures the flow and authors its warrant), priestly_establishment (endowments, exemptions, interpretive monopoly), royal_administration_elite (rank, tombs, bureau income). Victims declared: corvee_laborers, grain_tax_farming_households, foreign_war_captives. These declarations drive the engine's derivation: beneficiary seats derive low directionality (subsidized), victim seats derive high directionality amplified by trapped or constrained exits, and the crown's identity lock pushes it to the extreme beneficiary end — it cannot arbitrage away from a doctrine that is itself. Scope is national for the core seats, which modestly amplifies effective extraction for targets by raising verification difficulty. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct qualitative placement for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading the doctrine as pure snare would erase the genuine collective achievement it carried — basin-wide flood management, famine buffering, and monumental waterworks no local coalition could build; the coordination function is real and the founding problem (securing basin-wide cooperation on a volatile river) is still live. Reading it as rope or mountain would erase the definitional clause that removes the apex from accountability and converts every dispute about royal conduct into impiety. Tangled_rope holds both facts: coordination substrate plus asymmetric, actively enforced extraction. The R5 interview finds no mandatrophy: founding_problem_status is live and disappearance_verdict is world_rearranges, so no dead-mandate mismatch fires; the arrangement persists because its problem persists, not because a corpse is propped up. The dynastic-cycle measurements show the opposite of piton decay — each collapse is followed by deliberate reconstruction of the same formula by a new founder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    royal_infallibility_semantics,
    'Does ''the king cannot violate Ma''at'' assert descriptive incapacity (a rightful king is constitutionally unable to err) or a definitional shield (whatever the king does is retroactively classified as order)?',
    'Compare the tradition''s treatment of actual royal failures — succession usurpations, the Amarna rupture, low-flood famines under reigning kings: if failures are narrated as the king restoring order rather than as royal wrongdoing, the shield reading is confirmed.',
    'Under the shield reading the arrangement has no accountability surface at all and the flow is unbounded by design; under the incapacity reading a latent standard survives that later reciprocity readings could reactivate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_infallibility_semantics, conceptual, 'Whether the infallibility clause functions as incapacity or as immunization.').

omega_variable(
    kernel_flow_direction_contest,
    'This constraint is one reading of kernel maat_order_principle; would adopting a sibling reading change the structural classification?',
    'Author the sibling stories (reciprocity_reading, distributed_maintenance_reading) and compare computed classifications: under reciprocity the crown becomes a bound party with raised directionality; under distributed maintenance elite immunity dissolves and obligation diffuses across stations.',
    'The disagreement is located in flow direction and apex membership in the constraint set; resolving it redistributes victim sets and moves the crown seat from source to subject, reversing its directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_flow_direction_contest, conceptual, 'Committer structure: reading-contest within the Ma''at kernel.').

omega_variable(
    elite_compliance_mechanism,
    'Is elite compliance with the embodiment doctrine driven by structural jeopardy (career, estate, and memorial depend on the palace) or by internalized conviction (officials genuinely hold the king''s embodiment as true)?',
    'Track tone shifts in tomb autobiographies and administrative letters across succession breaks: persistence of orthodox profession when enforcement capacity collapses indicates internalization; rapid reorientation toward new centers indicates structural compliance.',
    'If largely internalized, compliance outlives its enforcement machinery and the measured suppression understates the arrangement''s grip; if structural, the arrangement falls quickly when the center does — as intermediate-period evidence suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_compliance_mechanism, empirical, 'Structural versus internalized compliance among elites.').

omega_variable(
    proxy_ritual_hollowing,
    'Does priestly proxy performance of the king''s daily rites preserve the embodiment claim (the office acts through deputies) or hollow it (embodiment quietly becomes a liturgical fiction)?',
    'Correlate intervals of heavy proxy delegation with crises: if kings resume personal performance at jubilees, famines, and succession threats, proxy is routine delegation of a live claim; if personal performance never resumes, the claim has become theatrical.',
    'Sustained hollowing would push theater_ratio past 0.5 and date a drift toward inertial maintenance; a preserved claim keeps the enforcement meaning of the arrangement intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_ritual_hollowing, empirical, 'Whether proxy ritual preserves or hollows the embodiment premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_divine_mandate_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(maat_divine_mandate_tr_t10, maat_order_principle__divine_mandate_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(maat_divine_mandate_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(maat_divine_mandate_tr_t30, maat_order_principle__divine_mandate_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(maat_divine_mandate_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(maat_divine_mandate_tr_t50, maat_order_principle__divine_mandate_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(maat_divine_mandate_tr_t60, maat_order_principle__divine_mandate_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(maat_divine_mandate_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(maat_divine_mandate_be_t10, maat_order_principle__divine_mandate_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(maat_divine_mandate_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(maat_divine_mandate_be_t30, maat_order_principle__divine_mandate_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(maat_divine_mandate_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(maat_divine_mandate_be_t50, maat_order_principle__divine_mandate_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(maat_divine_mandate_be_t60, maat_order_principle__divine_mandate_reading, base_extractiveness, 60, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(maat_divine_mandate_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(maat_divine_mandate_su_t10, maat_order_principle__divine_mandate_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(maat_divine_mandate_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(maat_divine_mandate_su_t30, maat_order_principle__divine_mandate_reading, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(maat_divine_mandate_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(maat_divine_mandate_su_t50, maat_order_principle__divine_mandate_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement(maat_divine_mandate_su_t60, maat_order_principle__divine_mandate_reading, suppression_requirement, 60, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, resource_allocation).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: 'Ma'at' as a colloquial label covers three structurally distinct arrangements. This reading carries the highest epsilon because the definitional clause deletes the accountability surface; the reciprocity sibling restores royal liability (lower epsilon, different victim set — the crown becomes a bound party); the distributed-maintenance sibling diffuses obligation across stations (lowest elite extraction, no immunized seat). Each is a separate file with its own beneficiaries, victims, and claimed type; the edges here record the family linkage, with this reading exerting upstream pressure on the other two by monopolizing legitimacy resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
