% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration as Suppressed Doctrinal Incoherence
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   In medieval and early modern Japan, simultaneous veneration of kami and
 *   buddhas became institutionalized despite resting on fundamentally
 *   contradictory ontologies. Institutional Buddhism asserted honji-suijaku
 *   (kami are manifestations of buddhas), while shrine networks maintained
 *   kami as autonomous, this-worldly beings. Rather than one reading
 *   displacing the other, the two systems coexisted through institutional
 *   silence: neither was formally refuted, both collected patronage,
 *   practitioners performed both sets of rituals. This reading instantiates
 *   'pragmatic incoherence'—the constraint is the enforced silence that
 *   permits contradictory doctrines to persist simultaneously. The referent
 *   (ε) is the standing arrangement of simultaneous veneration under this
 *   reading's lights: a high-extraction suppression of intellectual honesty
 *   masked as peaceful coexistence. Meiji shinbutsu-bunri is not an imposed
 *   rupture but a revelation—when the state forcibly separated the systems,
 *   practitioners did not resist the separation; they reorganized practice
 *   into coherence, suggesting the prior suppression was what required
 *   enforcement, not the separation.
 *
 * KEY AGENTS:
 *   - Lay practitioners: bearers of the contradiction, constrained by social cost of exit
 *   - Institutional Buddhism: enforcer of honji-suijaku reading, beneficiary of dual patronage
 *   - Shrine networks: enforcer of autonomous-kami reading, beneficiary of dual patronage
 *   - Reform-minded clerics: identity-locked holders of intellectual discomfort, silenced by institutional role
 *   - Ontological-fusion philosophers: suppressed third position that could resolve the contradiction
 *   - Meiji state: revealer of suppressed incoherence, not imposer of coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.78).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.71).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration as Suppressed Doctrinal Incoherence").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious/philosophical").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '6b0723f1-e548-4b62-97a5-b966e52691d0').
narrative_ontology:cs_kernel_codification('6b0723f1-e548-4b62-97a5-b966e52691d0', implicit).
narrative_ontology:cs_authority_grounding('6b0723f1-e548-4b62-97a5-b966e52691d0', distributed).
narrative_ontology:cs_reading_relation('6b0723f1-e548-4b62-97a5-b966e52691d0', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b0723f1-e548-4b62-97a5-b966e52691d0', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('6b0723f1-e548-4b62-97a5-b966e52691d0', foundational, contradiction_suppression_as_institutional_extraction).
narrative_ontology:cs_axiom_status(contradiction_suppression_as_institutional_extraction, holdable).
narrative_ontology:cs_axiom_grounding('6b0723f1-e548-4b62-97a5-b966e52691d0', contradiction_suppression_as_institutional_extraction, empirically_contingent).
narrative_ontology:cs_axiom('6b0723f1-e548-4b62-97a5-b966e52691d0', secondary, silence_enables_dual_patronage_without_coherence).
narrative_ontology:cs_axiom_status(silence_enables_dual_patronage_without_coherence, holdable).
narrative_ontology:cs_axiom_grounding('6b0723f1-e548-4b62-97a5-b966e52691d0', silence_enables_dual_patronage_without_coherence, instrumental).
narrative_ontology:cs_reference_frame('6b0723f1-e548-4b62-97a5-b966e52691d0', institutional_silence_permits_contradictory_doctrines).
narrative_ontology:cs_drift_state('6b0723f1-e548-4b62-97a5-b966e52691d0', pre_meiji_institutional_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6b0723f1-e548-4b62-97a5-b966e52691d0', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, reform_minded_clerics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, kami_shrine_networks).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, institutional_silence_as_coherence_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain altars housing both kami and buddha figures; perform rituals addressing contradictory ontological claims without theological resolution. Bear the cognitive burden of holding incompatible beliefs simultaneously — kami as immanent nature spirits AND kami as avatars of buddhas (or subordinate to them). Have no framework to adjudicate which system governs which phenomena. Their options are constrained: leaving either tradition is socially costly; articulating the contradiction risks accusations of impiety toward both systems.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners, payer,
    powerless, generational, constrained, national).

% Absorbs kami into honji-suijaku (original essence, manifest traces) hierarchy: kami are manifestations of buddhas, subordinate beings in the Buddhist soteriological system. Maintains doctrinal coherence by asserting the reduction. Enforces this reading through temple administration, ordination standards, and doctrinal teaching. Benefits from practitioners' simultaneous veneration because it expands Buddhist institutional reach and revenue (practitioners support both temples and shrines). Collects no direct extraction but consolidates institutional power through theological absorption.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, institutional_buddhism, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Maintain independent kami-veneration practice; operate shrines, conduct rites, train priesthood. Benefit from simultaneous veneration by receiving dual support (practitioners fund both shrine and temple activities). Enforce the kami-as-autonomous reading through shrine protocol, priestly training, and ritual practice. Their doctrinal reading (kami as distinct, this-worldly beings) contradicts institutional Buddhism's honji-suijaku absorption, but institutional silence on the contradiction permits both systems to collect patronage and authority simultaneously.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, kami_shrine_networks, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, kami_shrine_networks, agenda_setter).

% Hold philosophical or theological commitments that demand coherence — they see the doctrinal gap and want it resolved. Within their institutional roles (Buddhist clerics, shrine priests, scholars), speaking the contradiction publicly risks career penalty (accusations of heterodoxy, loss of standing). Their identity is fused with their institutional role; exit means abandoning not just employment but professional identity. They experience the suppression directly: their intellectual honesty and training in philosophy demands they articulate what practitioners are forced to live with unremarked.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, reform_minded_clerics, payer,
    moderate, biographical, identity_locked, national).

% In the mid-to-late Meiji era, officiates the shinbutsu-bunri (separation of kami and buddhas) edict: forces institutional separation, abolishes the honji-suijaku theoretical synthesis, reclassifies shrine practice as state Shinto and disestablishes Buddhist temples' shrine administration. Acts from modernization ideology (creating a rationalized, Western-comparable national religion) and political interest (consolidating state authority, weakening Buddhist institutional power). From the pragmatic-incoherence reading, the state does not IMPOSE coherence but REVEALS suppressed incoherence — the constraint's enforcement suddenly lifts and the contradiction surfaces immediately.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state, observer,
    institutional, generational, analytical, national).

% Philosophers and theologians who hold the ontological-fusion reading (kami and buddhas are metaphysically identical) are structurally excluded from public resolution of the contradiction. They have a third position that would reconcile both traditions, but institutional silence does not permit its articulation without risking both Buddhist and Shinto institutional disapproval. Their framework is suppressed not by active enforcement but by the mutual enforcement agreement between Buddhist and shrine hierarchies: neither wants coherence-through-identity because it threatens institutional distinctness and dual revenue.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, ontological_fusion_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, institutional_buddhism).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simultaneous veneration coordinates dual patronage — practitioners support both temples and shrines financially and spiritually. Institutions benefit from expanded reach without internal reformation. No coherent doctrinal framework is needed if institutional silence permits parallel practices. The 'coordination' is not solving a shared problem but freezing a problem-state: practitioners maintain contradictory commitments and neither institution demands resolution because both profit from the arrangement.
% TRANSFER_FUNCTION: Practitioners transfer attention, resources (financial contributions, ritual participation), and cognitive labor to maintain two incompatible theological systems simultaneously. Both institutional Buddhists and shrine networks collect this dual support. Reform-minded clerics transfer intellectual honesty and career autonomy in exchange for institutional belonging — they must remain silent or exit. The state transfers modernization legitimacy in exchange for doctrinal coherence (Meiji shinbutsu-bunri imposes separation and claims it as rational order).
% ABSENT_VOICES: Philosophers holding the ontological-fusion reading (a potential third framework) are suppressed from public voice by the mutual enforcement between institutional Buddhism and shrine networks — neither wants identity-based reconciliation because it threatens institutional distinctness. Lay practitioners who would articulate the cognitive burden directly (rather than living it silently) have no institutional amplifier. Meiji state actors who initiated coherence-through-separation are later; they are not absent from the pre-Meiji simultaneous-veneration period but they are absent from the constraint's own maintenance — the state reveals rather than sustains the incoherence.
% DISAPPEARANCE_RATIONALE: If simultaneous veneration's suppressive incoherence suddenly lifted — if practitioners were permitted or required to articulate which system governed which phenomena — institutional Buddhism would face either theoretical defeat (honji-suijaku revealed as false) or institutional reformation (integration into a unified system). Shrine networks would face equivalent pressure. Lay practitioners would reorganize their religious practice around a chosen, coherent framework rather than holding contradictions in suspension. The Meiji shinbutsu-bunri empirically demonstrates this: forced separation revealed the suppressed contradiction and practitioners reorganized practice rapidly into distinct domains.
% FOUNDING_PROBLEM: Early medieval Japan's religious ecology: Buddhism enters as a powerful, complete soteriological system offering afterlife salvation; kami veneration is autochthonous, governing this-worldly prosperity and agricultural fertility. No single practitioner naturally needed both systems' full ontologies — domestic ritual required kami blessing, deathbed and memorial practice required Buddhist intervention. When simultaneous veneration became institutionalized and theologized, the gap between two incompatible cosmologies became visible to trained philosophers but could be ignored by practitioners performing instrumental roles.
% FOUNDING_PROBLEM_CORROBORATION: The pragmatic division of labor (kami for this-world, buddhas for afterlife) is attested by scholars of Heian-Kamakura religious practice (e.g., Kuroda Toshio's analysis of 'exoteric-esoteric' Buddhism). Meiji reformers explicitly stated the founding problem had been solved by modernization and state rationalization. But NO pre-Meiji source attests that the contradiction is resolved or coherent — honji-suijaku theory is attested as Buddhist doctrine (attesting institutional Buddhism's reading), and kami-as-autonomous is attested in shrine ritual and some philosophical texts (attesting shrine networks' reading), but no pre-Meiji source claims coherence is achieved. This is the structural signal: if the founding problem were genuinely solved, a solving mechanism would be attested; its absence is evidence the constraint persists by suppressing the problem, not solving it.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because practitioners bear the cognitive cost of maintaining incompatible beliefs without resolution—the constraint extracts intellectual honesty. Suppression is high (0.71) because the incoherence is maintained by enforced silence from institutional hierarchies, not by logical or phenomenological evidence. Theater is substantial (0.62) because the rituals that embody the two systems appear functionally coherent—they work instrumentally (kami blessing is effective for fertility, Buddhist memorial rites are effective for afterlife transition)—but this instrumental success masks the underlying doctrinal incoherence. Accessibility collapse is low (0.48) because alternatives persistently exist: practitioners could choose exclusively Buddhist path (abandoning kami), or exclusively kami veneration (abandoning Buddhist afterlife concerns), or adopt the ontological-fusion reading (which is not available institutionally but is intellectually available). Resistance is moderate (0.52) because reform-minded clerics resist (internally, through identity-lock), but institutional mechanisms suppress their articulation; they cannot mount public resistance without career destruction. The measurement series shows extraction rising and theater rising over time (25-point interval spanning late medieval through early Edo period): as institutional Buddhism elaborates honji-suijaku theory and shrine networks entrench autonomous-kami doctrine, the suppression deepens—institutional coherence increases even as intellectual coherence does not. At interval end (time=25), extraction and suppression plateau near 0.78 and 0.71: the constraint reaches stable suppression. Historical note: the interval does not extend to Meiji separation (which occurs at approximately time=26-27 in this periodization); the measurements capture the pre-separation stability.
 *
 * PERSPECTIVAL GAP:
 *   From the lay practitioner's seat, simultaneous veneration is extraction masquerading as tradition—a requirement to maintain contradictory beliefs without resolution. From the institutional-Buddhism seat, simultaneous veneration is successful theological integration (honji-suijaku proves compatibility); from the shrine-network seat, it is practical coexistence without hierarchical subordination. From the Meiji state's observer seat, simultaneous veneration is pre-modern incoherence awaiting rational separation. These divergent readings compute to different types in per-seat classification: from the target seat (lay practitioner), the constraint is a snare (extraction, suppression, no resolution); from the beneficiary seats (institutions), it might compute as rope (genuine coordination solving the dual-patronage problem); from the reform-minded cleric's seat, it is a piton (the original coordination has atrophied into performative incoherence, sustained by institutional inertia). The divergence is the diagnostic signal: a genuine rope produces alignment across seats; divergence this wide indicates extraction is being hidden by institutional framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay practitioners sit at high directionality (d near 1.0, full target): they bear the extraction (cognitive burden), have constrained exit (social cost), and receive no direct benefit (they participate in both systems but neither institution is designed for their coherence—both institutions benefit from their dual support). Reform-minded clerics have intermediate directionality (d ≈ 0.65): they experience extraction as intellectual dishonesty but are compensated by institutional belonging and identity fusion; exit is identity-locked, not external constraint. Institutional Buddhism and shrine networks sit at low directionality (d near 0.0, beneficiaries): they collect dual patronage without solving the contradiction, and they have high exit options (they could commit to one framework, but choosing simultaneous veneration is their strategic choice). The Meiji state has analytical directionality (d = 0.5 by default, observer seat): it observes the constraint, reveals the incoherence through forced separation, but does not participate in sustaining the prior suppression. Ontological-fusion philosophers have constrained directionality (d ≈ 0.4): they could benefit from being heard (intellectual prestige), but are excluded by institutional suppression; they are partly victims of the suppression, partly potential beneficiaries if suppression lifted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dual religious systems serving different functions) was genuinely live when Buddhism first entered Japan—practitioners faced a real coordination problem (how to secure both this-worldly fertility and afterlife salvation). By the time simultaneous veneration becomes institutionalized and theorized, the founding problem is already dead: honji-suijaku provides a complete cosmology that integrates both systems; shrine networks have been absorbed into the Buddhist hierarchy as kami-temples; practitioners can and do rely on Buddhist institutions for both worldly and other-worldly goods. But the institutions do not declare the problem solved and retire the old system—they keep both active, keep both collecting patronage, and enforce silence on the contradiction. This is mandatrophy: the arrangements persist long after their original function is obsolete, sustained by extracted benefit (dual patronage) and suppression of the coherence question. The Meiji shinbutsu-bunri reveals mandatrophy by forcing the question: when the state separates the systems, practitioners do not resist and do not try to reconcile them (as they might if simultaneous veneration were genuinely solving an active coordination problem). They reorganize practice efficiently into separated domains, suggesting the prior constraint was purely extractive inertia. The six_questions.founding_problem_status = 'dead' captures this: the problem was solved (integration into single cosmology), but the institutional arrangement pretending to address it persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the silence maintained by external institutional enforcement (institutional penalties for articulating contradiction) or by internalized acceptance (practitioners genuinely believe simultaneous veneration is coherent)?',
    'Post-separation behavioral evidence: if practitioners rapidly adopt the new coherent framework without resistance, the suppression was external (institutional enforcement). If practitioners resist the separation or express genuine confusion about which system to use, the suppression was internalized (practitioners genuinely believed the contradiction was resolved). Historical post-Meiji data shows rapid adoption without resistance, suggesting external institutional enforcement.',
    'If suppression is external, the extraction is via institutional power (institutions benefit from dual patronage while suppressing questions). If suppression is internalized, the extraction is via cognitive capture (practitioners are trained not to notice the contradiction). The two mechanisms have different remedies: external suppression lifts when institutions separate (as Meiji shows); internalized suppression may persist post-separation in individual practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether silence is enforced externally or internalized cognitively').

omega_variable(
    honji_suijaku_theory_as_solution_vs_cover,
    'Does honji-suijaku theory genuinely solve the ontological contradiction (kami ARE manifestations of buddhas, so the systems are coherent), or is it a cover story that asserts solution without providing one?',
    'Textual analysis of Buddhist philosophical texts: do they provide a coherent metaphysical framework where honji-suijaku is a logical entailment, or is it asserted as a postulate without justification? Comparison with other Japanese philosophical traditions to assess whether the ''solution'' is unique to simultaneous veneration or reflects broader metaphysical commitments.',
    'If honji-suijaku is a genuine solution, the ontological-fusion reading may be correct and this reading (pragmatic incoherence) mischaracterizes the constraint. If honji-suijaku is asserted without justification, it is a cover story and extraction via suppression is structural. This directly affects the classification of all three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_theory_as_solution_vs_cover, conceptual, 'Whether honji-suijaku is a metaphysical solution or institutional posture').

omega_variable(
    alternative_readings_suppression,
    'Were other coherent readings (e.g., strict domain partition without hierarchy, or explicit incompatibility acknowledged) explicitly suppressed, or did they fail to develop institutional support and disappear naturally?',
    'Historical evidence of explicit institutional rejection (clerics penalized for proposing alternatives, texts burned or banned), vs. absence of evidence (no record of alternative frameworks being articulated or proposed). The domain_partition reading (if it were advanced clearly) would be an alternative to honji-suijaku hierarchy; evidence of its suppression would indicate institutional power enforcing one reading over others.',
    'If alternatives were explicitly suppressed, institutional enforcement is undeniable and extraction is a direct function of institutional power. If alternatives were simply never articulated, the suppression might be passive (absence of institutional support for innovation) rather than active enforcement. Passive suppression is less extractive but still constrains the possibility space.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_readings_suppression, empirical, 'Whether alternative readings faced explicit suppression or institutional neglect').

omega_variable(
    reading_incommensurability_with_sibling_readings,
    'Are the pragmatic-incoherence reading and the ontological-fusion reading logically incommensurable (each rules out the other''s core premise), or do they occupy different levels of analysis (metaphysics vs. institutional suppression)?',
    'Formal analysis of the two readings'' core premises: (pragmatic-incoherence: simultaneous veneration is MAINTAINED BY SILENCE and EXTRACTION prevents coherence-seeking) vs. (ontological-fusion: kami and buddhas ARE THE SAME and honji-suijaku EXPRESSES this truth). Do these premises logically foreclose each other, or can both be true if they describe different aspects of the arrangement (metaphysical truth and institutional sociology)?',
    'If incommensurable (forecloses relation), the three readings partition the possibility space and one must be correct. If compatible (coexists relation), multiple readings can be simultaneously true at different levels (metaphysical truth + institutional suppression could both hold). This affects how the cs_structure.reading_relations are classified and how the engine will compute drift and foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability_with_sibling_readings, conceptual, 'Whether pragmatic-incoherence and ontological-fusion readings are logically contradictory or operate at different analytical levels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(simu_tr_t0, observed).
narrative_ontology:measurement(simu_tr_t5, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement_basis(simu_tr_t5, observed).
narrative_ontology:measurement(simu_tr_t10, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement_basis(simu_tr_t10, observed).
narrative_ontology:measurement(simu_tr_t15, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 15, 0.59).
narrative_ontology:measurement_basis(simu_tr_t15, observed).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement_basis(simu_tr_t20, observed).
narrative_ontology:measurement(simu_tr_t25, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement_basis(simu_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement_basis(simu_be_t0, observed).
narrative_ontology:measurement(simu_be_t5, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(simu_be_t5, observed).
narrative_ontology:measurement(simu_be_t10, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement_basis(simu_be_t10, observed).
narrative_ontology:measurement(simu_be_t15, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(simu_be_t15, observed).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement_basis(simu_be_t20, observed).
narrative_ontology:measurement(simu_be_t25, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement_basis(simu_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(simu_su_t0, observed).
narrative_ontology:measurement(simu_su_t5, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(simu_su_t5, observed).
narrative_ontology:measurement(simu_su_t10, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(simu_su_t10, observed).
narrative_ontology:measurement(simu_su_t15, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(simu_su_t15, observed).
narrative_ontology:measurement(simu_su_t20, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(simu_su_t20, observed).
narrative_ontology:measurement(simu_su_t25, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(simu_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__pragmatic_incoherence_reading, 0.12).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, meiji_shinbutsu_bunri_state_coherence_imposition).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the simultaneous_veneration kernel: (1) pragmatic_incoherence_reading asserts the arrangement is maintained by institutional suppression of contradiction, extracting cognitive labor from practitioners; (2) ontological_fusion_reading asserts kami and buddhas are metaphysically identical and honji-suijaku expresses genuine truth; (3) domain_partition_reading asserts the arrangement is coherent specialization solving a real coordination problem. Each reading produces different ε values and different victim/beneficiary sets despite describing the same observable kernel. The three readings coexist as live positions held by different institutional actors and scholarly traditions; the pragmatic-incoherence reading emphasizes institutional sociology while the ontological-fusion reading emphasizes metaphysical truth—they are not directly contradictory but operate at different analytical levels (coexists_with relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__pragmatic_incoherence_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
