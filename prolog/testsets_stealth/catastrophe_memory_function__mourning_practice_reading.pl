% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av Memorial Obligation — Mourning-Practice Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   The annual fast of the ninth of Av obligates observing communities to
 *   mourn the destructions of the Temples and associated catastrophes: a
 *   sundown-to-nightfall fast, recitation of the lamentation liturgy, and
 *   abstention from celebratory activity on a fixed calendar date. This story
 *   instantiates ONE reading of the contested kernel
 *   catastrophe_memory_function — the mourning_practice_reading, under which
 *   the ritual preserves mourning-practice and boundary-norms (D1/D4) and
 *   nothing else: the ritual IS the boundary-maintenance, with no
 *   survival-competence transmission encoded. The sibling readings
 *   (survival_competence_reading, hybrid_transformation_reading) are separate
 *   constraints with their own epsilon values and are linked only through the
 *   network surface. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as tangled_rope (genuine identity coordination carrying an
 *   asymmetric extraction layer) while the metrics are authored descriptively
 *   — moderate extraction concentrated on marginal and dissenting seats,
 *   enforcement that decayed and partially recovered across the interval, and
 *   a theater ratio rising with heritage-mode routinization. The engine
 *   computes per-seat classifications from the structural data; the authored
 *   claim does not adjudicate them. Interval mapping: time points 0-120
 *   correspond to 1900-2020 CE.
 *
 * KEY AGENTS:
 *   - observant_community_members: primary beneficiary (organized/constrained) — bears the fast and receives the identity good; net position positive
 *   - rabbinic_institutions: agenda_setter (institutional/identity_locked) — administers the calendar and liturgy, collects interpretive deference, and is structurally fused with the custodial function it performs
 *   - assimilated_marginal_members: primary target (moderate/constrained) — bears the social cost of expected observance while receiving little of the belonging it distributes
 *   - dissenting_reframers: excluded challenger (powerful/constrained) — proposes alternative framings (sovereignty-era transformation, rival memorial days) and is resisted by the custodial machinery
 *   - traditional_community_women: dual-positioned payer-beneficiary (moderate/constrained) — carries the embodied observance in full while historically excluded from leading the liturgy or ruling on it
 *   - scholarly_observers: analytical seat (analytical/analytical) — documents the practice's transmission across centuries and polities from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av Memorial Obligation — Mourning-Practice Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'ff86fc52-9045-4a36-a556-fcf8600999dd').
narrative_ontology:cs_kernel_codification('ff86fc52-9045-4a36-a556-fcf8600999dd', fixed_text).
narrative_ontology:cs_authority_grounding('ff86fc52-9045-4a36-a556-fcf8600999dd', lineage).
narrative_ontology:cs_interpretation_layer_present('ff86fc52-9045-4a36-a556-fcf8600999dd').
narrative_ontology:cs_reading_relation('ff86fc52-9045-4a36-a556-fcf8600999dd', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff86fc52-9045-4a36-a556-fcf8600999dd', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('ff86fc52-9045-4a36-a556-fcf8600999dd', foundational, ritual_encodes_no_survival_competence).
narrative_ontology:cs_axiom_status(ritual_encodes_no_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('ff86fc52-9045-4a36-a556-fcf8600999dd', ritual_encodes_no_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('ff86fc52-9045-4a36-a556-fcf8600999dd', foundational, memorial_obligation_constitutes_group_boundary).
narrative_ontology:cs_axiom_status(memorial_obligation_constitutes_group_boundary, holdable).
narrative_ontology:cs_axiom_grounding('ff86fc52-9045-4a36-a556-fcf8600999dd', memorial_obligation_constitutes_group_boundary, deontological).
narrative_ontology:cs_reference_frame('ff86fc52-9045-4a36-a556-fcf8600999dd', perpetual_exile_lament_framework).
narrative_ontology:cs_drift_state('ff86fc52-9045-4a36-a556-fcf8600999dd', contemporary_post_sovereignty_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ff86fc52-9045-4a36-a556-fcf8600999dd', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, assimilated_marginal_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, dissenting_reframers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, traditional_community_women).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, traditional_community_women).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, exile_consciousness_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, communal_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fast from sundown to nightfall on the fixed date each year, gather to recite the lamentation liturgy, and refrain from celebratory activity. Through the shared date and shared texts they receive continuity with generations of predecessors and a yearly reaffirmation of belonging. Leaving the practice would mean stepping outside the community's rhythm of the year and, for most, outside the community itself; nearly all renew the obligation annually without experiencing it as imposed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_community_members, beneficiary,
    organized, generational, constrained, global).

% Fix the calendar date, compile and authorize the lamentation repertoire, rule on exemptions for illness and childhood, and decide which later catastrophes enter the liturgy. Members look to them for how and whether to observe, and that interpretive deference flows to them continuously. Their authority is bound up with custodianship of the memorial cycle — handing it off or retiring it would dissolve the role they occupy — and they carry the corresponding burden of answering every challenge to the practice's continuing warrant.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Sit loosely inside communities that expect observance. The annual expectation reaches them as pressure — questions from relatives, conspicuous non-participation at gatherings — while little of the belonging the practice distributes reaches back. Drifting out entirely is possible but cuts family and social ties, so most comply minimally or absorb the friction year after year.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, assimilated_marginal_members, payer,
    moderate, biographical, constrained, regional).

% Propose that the old lament no longer fits present circumstances — that sovereignty, rebuilding, or newer catastrophes call for different commemoration. Their proposals meet resistance from the custodial institutions and rarely reshape the liturgy; several built separate memorial days instead. They remain outside the decision loop over the very calendar they dispute.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, dissenting_reframers, excluded,
    powerful, generational, constrained, national).

% Carry the embodied observance in full — the fast binds them completely — while historically excluded from leading the lamentation recitation and from the councils that rule on practice. They receive the same belonging the practice distributes and bear its bodily costs; their leverage over how the practice is administered is limited, and their standing inside the community makes exit costlier than it looks from outside.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, traditional_community_women, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, traditional_community_women, beneficiary).

% Historians and sociologists of religion who study the memorial cycle across centuries and host polities. They document what the practice transmits and for whom, publish outside the communities, and hold no stake in the calendar's continuation or reform.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, rabbinic_institutions).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes mourning on a fixed annual date so that grief is carried collectively rather than individually; reproduces the community's catastrophe narrative through a shared liturgy; renders membership visible through observable practice — fasting, attendance, abstention — drawing and maintaining the boundary between insiders and outsiders.
% TRANSFER_FUNCTION: Moves bodily comfort, time, and attention from every observing member into the collective narrative; moves interpretive deference — decisions about how, whether, and which catastrophes to mourn — from members to the institutions that administer the calendar.
% ABSENT_VOICES: Secular Israelis who regard the lament as completed by sovereignty; nineteenth-century Reform communities that struck the day from their calendars outright; members whose personal losses fall on other dates and who experience the fixed schedule as foreign to their own grief. They stand outside the halakhic deliberation that governs the calendar — some in rival memorial frameworks, some outside communal life altogether — and the unanimity of the observant consensus partly reflects their absence from the room.
% DISAPPEARANCE_RATIONALE: Communal identity structures anchored to the date would loosen within a generation: the catastrophe narrative would lose its annual rehearsal, rival commemorations would compete for the vacated slot, and the institutions whose authority rests on custodianship of the cycle would forfeit a principal pillar. Diaspora communities that abandoned the practice historically assimilated faster — the rearrangement is documented, not hypothetical.
% FOUNDING_PROBLEM: After the destruction of the Temple and the dispersion that followed, a defeated and scattered community faced dissolution: nothing synchronized its grief, marked its continuity, or distinguished it from host societies. The fixed fast — already enumerated among the prophetic fasts — was instituted to bind the community to its catastrophe and to each other.
% FOUNDING_PROBLEM_CORROBORATION: Historians of diaspora communities attest the practice's continuity function across radically different host polities, and sociologists of collective memory corroborate the identity-binding effect of recurring commemoration from outside the tradition entirely; the record of communities that dropped the observance supplies the counterfactual. The custodial institutions also attest the problem's liveness, but the external historiography stands on its own. What remains disputed — and is recorded as disputed rather than resolved — is whether sovereign statehood has changed the problem's character.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.38 at interval end): the arrangement delivers a real identity good to most participants, but a distinct extraction layer runs through it — interpretive deference accrues to the administering seat, marginal members pay without proportional return, and the fixed calendar forecloses individual renegotiation of grief timing. Suppression (0.48) is a raw structural property, unscaled: communal sanction and calendar rigidity, not coercive force. Theater (0.32) requires care — in an identity ritual the performance IS part of the function, so the ratio tracks only the hollowed share: rote recitation of medieval Hebrew many attendees do not understand, attendance as affiliation signaling, livestreamed observance consumed as heritage content. Accessibility collapse is moderate (0.40): rival commemorative frameworks exist and persist (a state-sponsored Holocaust memorial day operates alongside), but within the traditional frame the calendar obligation is near-total for members. Resistance (0.48) is real and documented: nineteenth-century Reform communities struck the day from their calendars, sovereignty-era reframers sought transformation, and secular segments simply drift — none of which displaced the obligation. The temporal series share one grid (every tracked metric authored at every point 0-120). The suppression series is deliberately non-monotonic — enforcement capacity decayed through the emancipation and assimilation decades as communal sanction lost bite, then partially recovered late-century with the growth of tightly-knit traditional enclaves and digitally-visible deviation costs; the U-shape is an enforcement-capacity arc, not noise. Extractiveness rises gently throughout: outreach-era identity mobilization and liturgical consolidation (decisions over which new catastrophes enter the canon) thickened the administrative layer without changing the arrangement's basic shape.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the core member's position the arrangement is a treasured inheritance: the fast is felt as belonging, and the same structure that costs them comfort delivers continuity. From the marginal member's position the identical structure operates as unchosen obligation — the expectation arrives whether or not the belonging does. From the custodial seat the arrangement is a trust: the institution experiences itself as serving the calendar, not profiting from it, yet the deference it collects is real and its exit from the arrangement would be self-dissolution — the organization has become its custodial function, an institutional identity fusion that makes reform-from-within structurally hard. The two moderate-power seats diverge despite equal standing: the marginal member's tie to the community is thin enough that exit is conceivable, while the traditional woman's tie is thick but her leverage over administration is thin — she pays in the body what she cannot influence in council. The engine computes these divergences from the declared positions; the authored claim takes no side among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Core members are declared beneficiaries with constrained exit: derivation places them near the subsidized end — the arrangement insures their identity at a price they consent to annually. Marginal members are declared victims with constrained exit: derivation places them near the full-target end — they bear the enforcement surface of the arrangement without its dividend. Traditional community women carry both declarations (payer with secondary beneficiary): derivation should seat them mid-scale, above the core member and below the marginal. Dissenting reframers are declared victims with constrained exit and powerful standing: their exclusion from the calendar's decision loop is the enforcement object, but their capacity to build rival frameworks keeps them short of trapped. One override is declared: the institutional seat. The derivation reads the custodial institution's beneficiary declaration and drives d toward the subsidized end, but this misses two structural facts — the institution bears real custodial burdens (legitimacy defense, exemption adjudication, reframing pressure) and is identity-locked to the arrangement it administers. The override sets d to 0.18: net beneficiary, meaningfully bound, nowhere near the pure-subsidy end the raw declaration implies.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Read as pure coordination, the arrangement's extraction layer disappears: the deference that accrues to a single seat, the marginals who pay without return, and the documented resistance to reframing all vanish into a flattering story of voluntary shared grief. Read as pure extraction, the reverse error flattens what most participants demonstrably receive: a two-millennium-old identity good that communities consistently renew. The tangled-rope structure holds both truths in one frame — genuine coordination function, asymmetric extraction riding on it, active enforcement holding the joint. On obsolescence: the founding problem (post-catastrophe communal cohesion) remains live under this reading, the mandate has not outlived its function, no sunset clause exists or should, and the theater ratio sits well below the degraded-inertial range — this is not a husk maintained by habit. The genuine mandatrophy risk sits elsewhere: if sovereignty-era reframers are right that the founding problem changed character, the arrangement's warrant decays without its form decaying, and the omega on statehood frames that question rather than presuming an answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is the mourning_practice_reading of the kernel catastrophe_memory_function, asserting the memorial cycle encodes mourning-practice and boundary-norms (D1/D4) and no survival-competence transmission (D5). What would the sibling readings — survival_competence_reading (pure D5) and hybrid_transformation_reading (D1/D4+D5) — change structurally if adopted?',
    'Content analysis of the lamentation repertoire and transmitted practice for adaptive-instructional material: practical knowledge of institutional collapse, decentralized continuity techniques, or post-catastrophe reconstruction know-how encoded in the texts and their performance.',
    'If load-bearing D5 content is found, this reading collapses toward the hybrid reading and the arrangement''s profile acquires a technology-transfer component this story does not model; if none is found, the pure D1/D4 delta holds and the sibling readings remain separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the pure mourning-practice reading survives contact with the liturgical corpus the siblings read differently.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of non-observance structural (communal sanction, calendar rigidity, family pressure) or internalized (obligation experienced by observers as self-evident duty rather than imposed demand)?',
    'Post-exit trajectory study: track individuals who leave observant communities; if the sense of obligatory mourning persists after the sanctioning community is gone, a substantial share of suppression is internalized.',
    'If largely internalized, the structural suppression measure understates the constraint''s hold — targets carry the obligation with them after exit — and enforcement-capacity trends understate persistence; if largely structural, the decay-then-recovery enforcement series is the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of observance pressure.').

omega_variable(
    custody_rent_ambiguity,
    'Do the custodial institutions collect net rents from administering the memorial cycle, or do their custodial burdens (answering legitimacy challenges, absorbing reframing pressure, ruling on exemptions) offset the interpretive deference they receive?',
    'Comparative institutional accounting: resource flows to and obligations borne by the administering bodies, benchmarked against comparable non-custodial religious functions.',
    'Net rents would confirm the extraction layer''s concentration in a single seat and sharpen the capture reading; net custodial burden would redistribute measured extraction back toward the diffuse collective and soften the asymmetry this story declares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custody_rent_ambiguity, empirical, 'Whether administration of the memorial calendar is rent-collecting or burden-bearing on net.').

omega_variable(
    statehood_frame_obsolescence,
    'Does sovereign statehood answer the founding problem — preserving communal cohesion after catastrophe and dispersion — such that the perpetual-lament frame is obsolete, or does diaspora existence keep the problem live regardless?',
    'Not resolvable by data alone: it turns on whether the relevant community is defined as the diaspora, the sovereign state, or both, which is a values commitment of the disputing parties.',
    'If the founding problem is judged answered, the arrangement''s persistence becomes inertia-plus-administration and the mandate-outlived reading strengthens; if live, the arrangement retains its warrant and the contestation is merely over form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statehood_frame_obsolescence, preference, 'Whether sovereignty retires the founding problem the memorial obligation was built for.').

omega_variable(
    function_vs_inertia_persistence,
    'Is the identity-maintenance function still load-bearing for current participants, or does the practice persist mainly through institutional inertia and inherited habit with the function largely atrophied?',
    'Attendance and reported-meaning data across community segments, contrasted with communities where observance is rote; discontinuity experiments such as pandemic-year remote observance showed whether the function transfers when the embodied form breaks.',
    'If inertia dominates, the arrangement drifts toward the degraded-inertial profile and the theater series understates hollowing; if the function is live, the current classification holds and the theater rise reflects heritage-mode accretion only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(function_vs_inertia_persistence, empirical, 'Live identity function versus inertial persistence of the memorial obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(cata_tr_t80, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.29).
narrative_ontology:measurement_basis(cata_tr_t100, observed).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 120, 0.32).
narrative_ontology:measurement_basis(cata_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement_basis(cata_be_t80, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.37).
narrative_ontology:measurement_basis(cata_be_t100, observed).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement_basis(cata_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement_basis(cata_su_t80, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.47).
narrative_ontology:measurement_basis(cata_su_t100, observed).
narrative_ontology:measurement(cata_su_t120, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 120, 0.48).
narrative_ontology:measurement_basis(cata_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what the Tisha B'Av memorial cycle preserves' decomposes into three structurally distinct claims per the epsilon-invariance principle: preservation of mourning-practice and boundary-norms (this file), transmission of survival-competence (survival_competence_reading), and both jointly (hybrid_transformation_reading). Each claim carries its own epsilon, its own beneficiary/victim structure, and its own classification; measuring the cycle's liturgical content yields a different extraction profile under each. This story authors the pure D1/D4 reading: with no adaptive-instructional content attributed to the corpus, the arrangement's extraction is identity-coordination extraction plus administrative rent, and the upstream/downstream citation pattern runs from whichever reading a community's educators hold to how they teach the day. Family links route contamination analysis across the three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
