% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Self-Determination via Divine Covenant (Religious Obligation Reading)
 *   domain: political_philosophy/nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel about
 *   Jewish self-determination and territorial claims to the Levant. The
 *   religious_covenant_reading holds that Jewish people possess a divine
 *   covenant to the land, making territorial sovereignty a religious
 *   obligation independent of secular political frameworks. This reading
 *   derives from classical Jewish sources (Torah, Talmud, medieval
 *   commentaries) but has been politically intensified in modern contexts,
 *   particularly post-1967 and post-1973, among religious Zionist movements
 *   and settlement enterprises. The constraint claims mountain status (divine
 *   command as immutable natural law) but operationalizes as tangled_rope
 *   (religious authority entangled with state power, benefiting the
 *   settlement enterprise, suppressing secular territorial negotiation
 *   frameworks, excluding Palestinians from decision venues). The authored
 *   metrics describe substantial extraction (0.78), active enforcement
 *   (suppression 0.72), and rising theater ratio (0.31), indicating
 *   performative maintenance of the religious obligation narrative alongside
 *   increasingly explicit territorial expansion. This is NOT an evaluation of
 *   the reading's truth or righteousness—it is a structural description of
 *   how the reading operates as a constraint in political space.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: institutional authority; holds the covenant claim as self-evident obligation; benefits from state power; controls settlement enterprise narrative
 *   - settlement_enterprise: organized actor; expands settlements justified by covenant claim; receives state resources; suppresses Palestinian territorial claims
 *   - secular_israeli_state: agenda_setter; enforces the covenant reading via territorial policy, military control, settlement law; uses religious framework to justify boundaries
 *   - torah_interpretive_authority: distributed authority; scholars and rabbis who maintain and transmit the covenant reading; gatekeep alternative interpretations
 *   - palestinian_territorial_claimants: victims (payer); excluded from negotiation venues; bear the direct cost of territorial displacement justified by the covenant reading
 *   - diaspora_jewish_communities: victims (payer); constrained by the reading's framing (covenant ties Jewish fate to territorial state; alternatives like diaspora pluralism are foreclosed); bear reputational and political cost
 *   - secular_territorial_negotiation_framework: victim (structural); liberal nationalist and international-law approaches are suppressed as illegitimate by the covenant reading; negotiation becomes impossible within the framework
 *   - competing_religious_readings: excluded; Orthodox, Conservative, Reform, secular Jewish traditions that reject or reinterpret the covenant claim are systematically suppressed from policy venues
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.78).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.72).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Self-Determination via Divine Covenant (Religious Obligation Reading)").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, 'da00baf7-b805-449b-b669-37ad719feae3').
narrative_ontology:cs_kernel_codification('da00baf7-b805-449b-b669-37ad719feae3', formalized).
narrative_ontology:cs_authority_grounding('da00baf7-b805-449b-b669-37ad719feae3', extraction).
narrative_ontology:cs_interpretation_layer_present('da00baf7-b805-449b-b669-37ad719feae3').
narrative_ontology:cs_reading_relation('da00baf7-b805-449b-b669-37ad719feae3', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('da00baf7-b805-449b-b669-37ad719feae3', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('da00baf7-b805-449b-b669-37ad719feae3', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('da00baf7-b805-449b-b669-37ad719feae3', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('da00baf7-b805-449b-b669-37ad719feae3', foundational, divine_covenant_immutable_territorial_claim).
narrative_ontology:cs_axiom_status(divine_covenant_immutable_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('da00baf7-b805-449b-b669-37ad719feae3', divine_covenant_immutable_territorial_claim, deontological).
narrative_ontology:cs_axiom('da00baf7-b805-449b-b669-37ad719feae3', foundational, religious_obligation_supersedes_secular_negotiation).
narrative_ontology:cs_axiom_status(religious_obligation_supersedes_secular_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('da00baf7-b805-449b-b669-37ad719feae3', religious_obligation_supersedes_secular_negotiation, deontological).
narrative_ontology:cs_axiom('da00baf7-b805-449b-b669-37ad719feae3', secondary, jewish_territorial_security_requires_state_monopoly).
narrative_ontology:cs_axiom_status(jewish_territorial_security_requires_state_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('da00baf7-b805-449b-b669-37ad719feae3', jewish_territorial_security_requires_state_monopoly, instrumental).
narrative_ontology:cs_reference_frame('da00baf7-b805-449b-b669-37ad719feae3', divine_covenant_territorial_obligation).
narrative_ontology:cs_drift_state('da00baf7-b805-449b-b669-37ad719feae3', contemporary_post_1967_settlements, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da00baf7-b805-449b-b669-37ad719feae3', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, torah_interpretive_authority).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_territorial_claims).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, diaspora_jewish_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, secular_israeli_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_territorial_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and maintains the covenant reading as normative for Jewish territorial claims; controls settlement narrative and rabbinical authority; interprets scripture to justify territorial expansion; receives state resources and policy support; faces no institutional pressure to modify the reading (internal coherence is high, external criticism is framed as delegitimization). Exit would require abandoning religious identity and redefining Jewish collectivity—identity_locked.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    organized, generational, identity_locked, global).

% Expands settlements on Palestinian land justified by the covenant reading; receives state subsidies, military protection, and legal privilege; operates under the framing that territorial expansion is religious obligation rather than political choice. Exit would mean relinquishing settlements and acknowledging the reading is instrumental rather than natural law—constrained by the reading itself.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, constrained, regional).

% Enforces the covenant reading via territorial policy, military control, settlement law, and exclusion of alternative frameworks from official discourse; uses the covenant reading to justify boundaries to international audiences; collects territorial resources and strategic advantage. Could theoretically exit by adopting a secular territorial framework (liberal nationalism or international law), but the religious reading has become institutionalized; exit requires institutional and legal restructuring.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_israeli_state, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, secular_israeli_state, beneficiary).

% Maintains the covenant reading as authoritative within Jewish tradition; controls which interpretations are transmitted, taught, and recognized as legitimate; gatekeeps alternative readings (Conservative, Reform, secular interpretations that reject or reinterpret the covenant); faces internal tradition-pressure to preserve the reading and external institutional pressure to maintain religious authority. Identity-locked: rejecting the covenant reading is tradition-rupturing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, torah_interpretive_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Bear direct territorial displacement justified by the covenant reading; excluded from decision-making venues; cannot negotiate territorial arrangements because the covenant reading forecloses compromise; cannot exit without relinquishing territorial claim or leaving the territory. Trapped: the constraint operates regardless of their consent.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_territorial_claimants, payer,
    powerless, generational, trapped, regional).

% Constrained by the covenant reading's framing that Jewish collective fate is tied to territorial state; alternatives like cultural pluralism and diaspora-centered collectivity are foreclosed as incoherent or illegitimate within the reading's logic; bear reputational and political costs (diaspora Jewish communities often bear the consequences of territorial conflict while lacking decision-making power); receive some benefits (cultural/historical connection, identity affirmation) but constrained by state's military policies and territorial disputes.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diaspora_jewish_communities, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, diaspora_jewish_communities, beneficiary).

% International law, liberal nationalism, and secular political frameworks for territorial negotiation are suppressed by the covenant reading; these frameworks cannot operate within the logic of divine obligation; compromise and negotiated settlement become impossible within the framework's logic. As a non-agent entity (a framework, not an actor), it is excluded from beneficiary/victim derivation but included for narrative completeness.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework).

% Orthodox, Conservative, Reform, and secular Jewish traditions that hold alternative interpretations of the covenant (conditional covenants, non-territorial readings, spiritual rather than political obligation) are systematically excluded from official policy discourse; their interpretive authority is suppressed; they would argue for theological pluralism and alternative frameworks but lack institutional power to inject these readings into state policy. Excluded from the operative conversation, though not entirely voiceless (diaspora communities often hold these readings).
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, competing_religious_readings, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, competing_religious_readings).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds religious Jewish communities to a shared territorial framing and collective identity; provides existential security narrative (the land is inalienably ours, giving us refuge); coordinates believers around a common understanding of Jewish obligation and meaning-making in post-Holocaust context.
% TRANSFER_FUNCTION: Moves territorial control, state resources (military, legal, administrative), and collective identity from the operative space of secular-nationalist or international-law frameworks into the operative space of the covenant reading. Specifically: transfers Palestinian territorial claims, secular alternatives to religious obligation, diaspora autonomy, and international negotiation frameworks into the interpretive authority of the religious reading.
% ABSENT_VOICES: Palestinian communities and international law advocates are structurally excluded from interpretation and policy venues; they would argue that the covenant reading is constructed rather than natural and that territorial claims should be negotiated through secular frameworks, but they lack institutional power within the reading's authority structure. Diaspora Jewish communities that hold alternative readings (diasporist, secular nationalist) are also effectively excluded from policy discourse, though they maintain parallel institutional structures.
% DISAPPEARANCE_RATIONALE: Within the religious framework, if the covenant reading disappeared, Jewish self-determination would be unmoored from its theological justification—the world would rearrange around alternative framings (liberal nationalism, international law, diaspora pluralism). From the secular perspective, the constraint is constructed and its disappearance would enable territorial negotiation within international-law frameworks. From the religious perspective, the constraint cannot disappear because it expresses divine reality—the disappearance of belief would not dissolve the obligation. The verdict is contested precisely because different frameworks disagree on whether the constraint is ontologically real or politically constructed.
% FOUNDING_PROBLEM: Post-Holocaust: ensuring Jewish collective survival and creating secure territorial refuge after the Holocaust's demonstration that diaspora Jewish communities could be systematically murdered. The founding problem is both existential (Jewish survival) and spiritual (creating a land where Jewish culture and religious practice could flourish without subordination to Christian or Islamic majorities).
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionists attest the founding problem remains live: persistent antisemitism, ongoing regional threats, and diaspora vulnerability require territorial security grounded in Jewish control. Secular critics attest the founding problem is substantially solved: Israel is militarily dominant, Jewish survival is institutionally secured, and the constraint now persists as a vehicle for territorial expansion rather than security (evidenced by continued settlement growth despite Palestinian military weakness). International law scholars and human rights advocates attest the founding problem has been superseded by alternative frameworks: collective security through international law, minority rights protections through diaspora pluralism, and self-determination frameworks that don't require ethnic-territorial exclusivity. No single external authority converges on a verdict; the founding problem's status is permanently contested within the kernel.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.78) reflects that the covenant reading generates a non-negotiable territorial claim: within the framework, the land is inalienably Jewish by divine right, making compromise inherently illegitimate. This is extreme extraction from alternative frameworks (they are not merely rejected; they are ontologically incompatible). Suppression is high (0.72) because the constraint requires active state and institutional enforcement: military control of territory, settlement law, exclusion of Palestinian authority, suppression of alternative Jewish readings from policy venues, and constant narrative maintenance that the covenant is natural law rather than interpretive choice. Theater_ratio is moderate (0.31) because genuinely religious believers experience the covenant reading as self-evident, not performative, but the state's operationalization increasingly relies on ritualistic assertion (religious authority invoked in policy justifications, settlement ceremonies framed as covenant fulfillment) rather than theological argument. The temporal trajectory shows rising extractiveness (0.42 to 0.78 over 60 years) tracking post-1967 settlement expansion and post-2000s entrenchment; rising suppression tracks intensifying military/legal enforcement and exclusion of alternatives; rising theater tracks the shift from theological claim (relatively invisible in early Zionism, which was largely secular nationalist) to political assertion via state ritual. The measurement grid is shared across all three series; every time point reports every metric.
 *
 * PERSPECTIVAL GAP:
 *   The payer/victim seats and the beneficiary/agenda_setter seats compute radically different types from identical structural data. From the religious_zionist position: the constraint is Mountain—it expresses an immutable divine law grounded in covenant; accessibility_collapse is high (0.82) because the alternative (giving up the land) is theologically impossible, not politically costly; resistance is low (0.68) because believers face no resistance from within their framework, only from outside actors who reject the framework. From the Palestinian position: the constraint is Snare—it is enforced extraction, suppression of territorial claims, exclusion from negotiation; accessibility_collapse is high (0.82) because the only available 'alternative' is accepting the covenant framing or leaving; resistance is high (0.68) because Palestinians mount constant resistance to territorial displacement. From the secular_state position: the constraint is Tangled_rope—a coordination function (binding religious constituencies to territorial policy) plus extraction (monopolizing territorial claims, excluding alternatives, suppressing secular frameworks). The engine computes all three per-seat types from the shared structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious_zionist_movement and settlement_enterprise are full beneficiaries (d near 0.0): they collect the territorial expansion, receive state resources, exercise interpretive authority over the covenant claim, and face minimal exit costs (the claim is internalized as obligation). Their directionality is beneficiary: the constraint subsidizes their territorial and political project. Secular_territorial_negotiation_framework is a full target (d near 1.0): it is structurally suppressed; compromise becomes impossible; its entire operative space is foreclosed by the covenant reading. Palestinian_territorial_claimants are full targets (d = 1.0): they bear direct territorial displacement, are excluded from decision venues, have trapped exit (cannot leave the territory without relinquishing claim, cannot stay without accepting the constraint's framing). Diaspora_jewish_communities sit near target (d = 0.85): they are constrained by the covenant reading's framing (their collective fate is tied to a militarized state; alternatives like cultural pluralism are foreclosed); they bear reputational and political cost without controlling the narrative. Torah_interpretive_authority is complex: they are agenda_setters (they set and maintain the reading), but they are also locked into the framework (rejecting the covenant claim is career-ending, tradition-rupturing). From the beneficiary seat, the constraint is Mountain (immutable divine law). From the target seats, it is Tangled_rope (real coordination function—binding believers—plus extraction from non-believers and suppression of alternatives). This per-seat divergence is not a bug; it is exactly what the framework measures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing Jewish collective survival and territorial safety post-Holocaust) was live when the covenant reading was deployed in early Zionism (though early Zionism was mostly secular). The founding problem status is now contested: religious Zionists argue the problem remains live (persistent antisemitism, territorial threat); secular critics argue the founding problem is substantially solved (the state is militarily dominant, Jewish survival is secure) but the constraint persists as a vehicle for territorial expansion (mandatrophy = the founding problem has expired but the constraint remains, now serving extraction rather than coordination). The measurement series tracks this shift: extractiveness rises over 60 years as territorial expansion becomes the driver rather than security; theater rises as the constraint's operation becomes increasingly divorced from genuine security coordination and instead performed as obligation. A commitment to the founding-problem test: if Palestinian territorial threat evaporated, would the covenant claim persist? If yes, mandatrophy is present (the constraint's function has shifted from security coordination to territorial acquisition). If no, the constraint still serves the founding problem. Current evidence (continued settlement expansion despite Palestinian security weakening) suggests mandatrophy is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_covenant,
    'Is the divine covenant claim a natural law (immutable fact about reality grounded in divine will) or a constructed interpretive framework that benefits identifiable parties?',
    'Examine the historical emergence of the religious covenant reading: was it a stable constant in Jewish tradition or did it intensify under specific political pressures (European nationalism, Enlightenment secularization, post-Holocaust context, post-1967 territorial expansion)? Cross-reference with Jewish philosophical traditions that held the covenant claim yet rejected territorial sovereignty (diaspora nationalism, spiritual Zionism, non-territorial return). If the reading''s salience tracks with political contingency rather than textual discovery, the ''natural law'' status is contested.',
    'If the reading is constructed (benefits religious settlement enterprise + interprets covenant to exclude compromise), the constraint reclassifies from mountain to tangled_rope or snare: high extraction, active enforcement to exclude secular alternatives, identifiable beneficiaries. If it is genuinely immutable within the tradition, mountain status holds but requires documentation of how non-benefiting parties (diaspora Jews, Palestinians) relate to the covenant claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_covenant, empirical, 'Whether divine covenant claim is timeless tradition or politically contingent intensification.').

omega_variable(
    framework_containment_ambiguity,
    'Within a single framework (Jewish law, Christian theology, secular international law), does the religious covenant reading rule out all alternative framings, or do alternative framings remain live within different communities'' interpretive traditions?',
    'Survey Jewish philosophical and legal traditions (medieval rabbinic, modern Orthodox, Conservative, Reform, secular) and document which frames hold the covenant as binding, conditional, or superseded. Similarly document Christian and Muslim theological responses to the claim. If every intellectual tradition produces adherents who reject the religious sovereignty reading while maintaining internal coherence, the readings coexist across frameworks rather than one foreclosing another.',
    'If readings foreclose each other, the constraint is a mountain with no legitimate alternatives—the world must rearrange to accommodate it or reject it wholesale. If readings coexist across traditions, the constraint is tangled_rope: real religious obligation for believers, but one framework among several, requiring active enforcement to suppress alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framework_containment_ambiguity, conceptual, 'Whether the covenant reading is framework-internal or framework-transcendent (monopolizing the truth).').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (legal barriers to alternative territorial settlements, military enforcement of boundaries, exclusion of competing territorial claims from decision-making venues) or internalized (believers have absorbed the covenant claim as self-evident truth and suppress alternative frameworks within their own thinking)?',
    'Track enforcement apparatus: if suppression persists through explicit legal mechanisms, border control, settlement policy, and exclusion of Palestinians from territorial negotiation, suppression is structural. If suppression also persists after people leave the geographic territory or after hearing compelling alternative frameworks, measure the post-exposure persistence as evidence of internalization. For diaspora Jews and Palestinians, suppression is primarily structural (excluded from venues, constrained by enforcement). For religious Zionists, suppression appears low (internally consonant with beliefs), but measurement should track whether the covenant framework persists if alternatives were institutionally available.',
    'If suppression is primarily structural, the constraint''s effective extractiveness is what the measurement states (0.78). If suppression is partially internalized, the constraint carries its suppression apparatus with people after exit, raising effective extraction. The distinction matters for remedies: structural suppression requires institutional change; internalized suppression requires counter-narrative and frame-shifting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the covenant claim.').

omega_variable(
    kernel_reading_as_distinct_constraint,
    'Is this constraint the divine covenant claim itself (a religious proposition about reality), or is it the CLAIM plus the political enforcement structure (the entanglement of religious authority with state power that makes the claim operative in territorial policy)?',
    'Distinguish: (A) The covenant claim as a theological/philosophical statement: ''God covenanted the land to the Jewish people.'' This is a mountain if it is an immutable truth, or a constructed narrative if it is an interpretive choice. (B) The political-enforcement structure: ''Religious authority shapes territorial policy; settlement is justified by religious obligation; secular frameworks for negotiation are foreclosed by religious obligation.'' This is clearly tangled_rope: coordination (religious framework binds believers) + extraction (religious framework excludes Palestinians from decision-making, suppresses secular alternatives). The two are entangled but distinct. This story treats them as one constraint—is that the right scope, or should they be decomposed?',
    'If kept unified, the constraint''s type is contested (mountain claims but tangled_rope operation). If decomposed, each constraint gets its own ε, beneficiary/victim set, and classification. The current unified approach captures the real political salience but may obscure the structural distinction between the theological claim and its political enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_as_distinct_constraint, conceptual, 'Scope of the constraint: theological claim alone vs. claim plus political operationalization.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the religious covenant reading logically foreclose the settler_colonial_reading (the claim that Zionism is a European colonial project), or do both remain coherent within a single evidentiary framework?',
    'The two readings disagree about the causal origin of Zionism (religious obligation vs. European colonialism) and the legitimacy of territorial displacement (divine covenant vs. dispossession). Examine whether evidence of European influence on Zionist ideology, or evidence of coordinated international support, undermines the religious-obligation framing, or whether believers maintain both simultaneously: ''Yes, Zionism has European elements AND the land is divinely covenanted.'' If believers maintain both, they coexist. If accepting the colonial history requires abandoning the covenant reading, one forecloses the other.',
    'If foreclosure is real, the engine will detect contradiction and mark the pair. If coexistence is the case, each reading persists as a live position. The result shapes how the corpus models contested kernels: do they split into mutually exclusive positions, or do they admit hybrid positions that hold elements of multiple readings?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, empirical, 'Whether religious covenant and settler-colonial readings logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jewi_tr_t15, jewish_self_determination__religious_covenant_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__religious_covenant_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(jewi_tr_t45, jewish_self_determination__religious_covenant_reading, theater_ratio, 45, 0.27).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__religious_covenant_reading, theater_ratio, 60, 0.31).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jewi_be_t15, jewish_self_determination__religious_covenant_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__religious_covenant_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(jewi_be_t45, jewish_self_determination__religious_covenant_reading, base_extractiveness, 45, 0.75).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__religious_covenant_reading, base_extractiveness, 60, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(jewi_su_t15, jewish_self_determination__religious_covenant_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__religious_covenant_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(jewi_su_t45, jewish_self_determination__religious_covenant_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__religious_covenant_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the jewish_self_determination kernel. All five readings share the same domain (territorial claims and self-determination frameworks) but instantiate different constraints with different ε values, beneficiary/victim structures, and types. The religious_covenant_reading constrains the operative space of the liberal_nationalist_reading (by asserting obligation independent of nations-rights frameworks) and influences the settler_colonial_reading (by providing a legitimacy narrative for territorial claims). It coexists with diasporist and indigenous readings—different communities hold these simultaneously. See the sibling stories for the full family decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
