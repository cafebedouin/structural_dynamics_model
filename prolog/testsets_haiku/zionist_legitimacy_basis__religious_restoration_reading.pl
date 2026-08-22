% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious Zionist Legitimacy: Divine Restoration & Territorial Mandate
 *   domain: political_history/nationalism/religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates the religious Zionist reading of contested
 *   kernel zionist_legitimacy_basis. Post-1967, a coalition of rabbinical
 *   authorities and organized religious communities reinterpreted territorial
 *   gains as fulfillment of divine promise and acceleration of messianic
 *   process. The reading moves legitimacy for territorial claims from secular
 *   political and international law frameworks into theological authority,
 *   where settlement becomes religious obligation rather than political
 *   choice. This reading coexists with secular national-liberation readings
 *   (Zionism as indigenous return) and settler-colonial readings (Zionism as
 *   European settlement project) — all three read the same historical events
 *   differently. The constraint itself describes how the religious
 *   restoration reading operates as a legitimacy structure: it coordinates
 *   religious identity and territorial claim, but does so asymmetrically,
 *   extracting from Palestinian displacement and secular opposition while
 *   concentrating theological authority in religious Zionist institutions.
 *   The claim/metric gap is intentional: the constraint is claimed as
 *   tangled_rope (mixing coordination of religious obligation with extraction
 *   via territorial displacement), while the authored metrics reflect the
 *   rising extractiveness and suppression required to maintain the reading's
 *   institutional dominance over 56 years.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: Sets the theological frame; organized political power at moderate-to-institutional scale
 *   - israeli_settler_communities: Identity-locked beneficiaries with theological justification for territorial settlement
 *   - palestinian_arabs: Powerless payers bearing displacement costs reframed as theological necessity
 *   - secular_israeli_opposition: Moderate-power payers marginalized as theologically illegitimate
 *   - international_law_advocates: Powerful but structurally incompatible authority (secular law vs. divine obligation)
 *   - knesset_and_settlement_administration: State apparatus capturing secular authority for religious ends
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.72).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.68).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious Zionist Legitimacy: Divine Restoration & Territorial Mandate").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/religious_authority").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '4023a40b-51c4-4ad2-a1da-fa88e92ba9e7').
narrative_ontology:cs_kernel_codification('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', fixed_text).
narrative_ontology:cs_authority_grounding('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', lineage).
narrative_ontology:cs_interpretation_layer_present('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7').
narrative_ontology:cs_reading_relation('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', foundational, divine_covenant_binding_territorial_obligation).
narrative_ontology:cs_axiom_status(divine_covenant_binding_territorial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', divine_covenant_binding_territorial_obligation, deontological).
narrative_ontology:cs_axiom('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', foundational, messianic_acceleration_via_territorial_conquest).
narrative_ontology:cs_axiom_status(messianic_acceleration_via_territorial_conquest, holdable).
narrative_ontology:cs_axiom_grounding('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', messianic_acceleration_via_territorial_conquest, theological).
narrative_ontology:cs_reference_frame('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', divine_covenant_territorial_claim).
narrative_ontology:cs_drift_state('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', contemporary_2023, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4023a40b-51c4-4ad2-a1da-fa88e92ba9e7', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_settler_communities).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, messianic_theological_tradition).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_arabs).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_opposition).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, international_law_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, israeli_settler_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets post-1967 territorial gains as fulfillment of divine promise and signs of messianic process. Sets the theological frame that legitimizes settlement expansion and rejects territorial compromise as breach of sacred covenant. Controls interpretation of Jewish law (halakha) regarding territorial obligation and settlement permissibility. Administers settler community networks, yeshiva curricula, and rabbinical authority structures that enforce adherence to the restoration mandate.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_movement, agenda_setter,
    organized, civilizational, identity_locked, global).

% Receive theological justification for territorial settlement as divine obligation rather than political choice, reducing cognitive dissonance about displacement. Bear costs of militarized settler life, legal precarity in international law, and ongoing security threats. Their identity is fused with messianic narrative — exit from settlement means religious apostasy, not merely political disagreement. Communities are embedded in rabbinical authority structures that reinforce the theological mandate.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_settler_communities, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, israeli_settler_communities, payer).

% Bear the structural costs of territorial displacement and settlement expansion justified theologically as divine mandate rather than political negotiation. Their displacement is reframed as religious necessity, not settler colonialism, which forecloses negotiation pathways based on secular law, human rights, or historical occupation. Exit options are severely constrained: remaining under occupation or forced displacement, with no leverage over the theological frame that justifies their removal.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Oppose the religious restoration reading as theologically coercive and politically destructive, but operate within a state structure where the reading has gained institutional authority through settler community organization, rabbinical networks, and control of settlement policy. Their opposition is marginalized within Israeli political discourse as theologically illegitimate. They bear the cost of international delegitimization and internal conflict over whether Zionism is a secular national project or a religious restoration mandate.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_opposition, payer,
    moderate, biographical, constrained, national).

% Face a constraint that moves the legitimacy claim for Israeli territorial policy from international law and secular political discourse into religious authority, which sits outside the jurisdiction of international law. The religious restoration frame renders secular law-based objections (occupation law, settlement illegality under Geneva Conventions) theologically irrelevant to believers. Their tools of enforcement (treaties, courts, sanctions) are structurally incompatible with the divine-obligation framing.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_law_advocates, payer,
    powerful, biographical, analytical, global).

% Non-agent entity: the interpretive tradition grounding legitimacy in divine covenant and messianic acceleration. It is vindicated by the reading's institutional success in shaping Israeli policy, but it collects no material extraction itself. The reading's authority depends on continuous assertion that post-1967 events fulfill this tradition.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, messianic_theological_tradition, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(zionist_legitimacy_basis__religious_restoration_reading, messianic_theological_tradition).

% Would argue for alternative Jewish theological interpretations emphasizing justice, repair (tikkun olam), and coexistence — readings that view messianic obligations as spiritual transformation rather than territorial conquest. They are structurally excluded from setting Israeli policy through the institutional dominance of religious Zionist authority structures. Their theological voice is marginalized despite representing substantial Jewish communities globally.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, progressive_jewish_movements, excluded,
    organized, generational, constrained, global).

% Administers settlement policy and enforces land transfer, permit allocation, and resource distribution to settler communities. Elected coalitions dependent on religious Zionist political parties, translating theological mandate into state law and resource allocation. The constraint's enforcement machinery operates through state apparatus capturing secular political authority for religious ends.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, knesset_and_settlement_administration, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_movement).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish identity, theological obligation, and territorial claim under a unified narrative of divine restoration. Solves the theological problem of how to interpret post-1967 territorial gains within Jewish tradition — the constraint answers: as fulfillment of covenant, not contingent military outcome. Unifies religious and secular Zionists under a shared frame where territorial maximalism is obligation, not choice.
% TRANSFER_FUNCTION: Transfers territorial control, settlement legitimacy, and religious authority from Palestinians and secular international law to Israeli settler communities and religious Zionist institutions. Moves resources (land, water, permit allocation) from Palestinian administration to settler communities, justified by theological mandate. Transfers moral authority for territorial claims from secular political argumentation to religious interpretation, which places these claims outside the jurisdiction of international law and secular ethics.
% ABSENT_VOICES: Progressive Jewish movements, Palestinian civil society, international human rights bodies, and secular Zionist critics are structurally excluded from the theological-interpretation frame. They can object in political forums but cannot contest within the reading's own authority structure — the rabbinical courts and theological seminaries that set the restoration mandate. Their exclusion is maintained by restricting who counts as a legitimate interpreter of Jewish law and messianic tradition.
% DISAPPEARANCE_RATIONALE: If the religious restoration reading were to lose institutional authority and be replaced by alternative theological framings (progressive Jewish coexistence readings, secular Zionist nationalism, or international law frameworks), Israeli settlement policy would face immediate reorientation: territorial expansion would lose theological justification, settler communities would lose the religious obligation frame insulating them from law-based objections, and negotiation pathways based on secular law and human rights would reopen. The constraint's disappearance would restructure the entire legitimacy basis for ongoing settlement and territorial claims.
% FOUNDING_PROBLEM: Post-1967 territorial gains (the West Bank, Gaza, Golan Heights) required theological interpretation to resolve within Jewish tradition. The restoration reading answers this: the gains are signs of the messianic process and divine covenant restoration. The founding problem is: how do we make sense of these events within our religious tradition? What do they mean for Jewish law and obligation?
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist leaders and rabbinical authorities attest the founding problem is live and the answer clear. Secular Zionists, progressive Jewish movements, and Palestinian theologians attest the founding problem was solved differently in other readings — that the post-1967 gains require no special theological interpretation, or require a decolonization reading. International law scholars attest the founding problem was a crisis of legitimacy that the religious restoration reading addressed by moving the claim outside secular jurisdiction. The status is contested because the foundational question itself (whether post-1967 events require theological reinterpretation) is partitioned along reading lines.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (1967, post-war theological reinterpretation phase) to 0.72 (2023, institutionalized settlement regime). The trajectory reflects: (1) early ideological consolidation (1967–1980), (2) institutional embedding in state structures (1980–2005), (3) stabilization with rising international pressure (2005–2023). Theater ratio climbs from 0.18 to 0.41, indicating growing rhetorical performance relative to functional coordination — the constraint initially solved real theological problems, but increasingly performs theological obligation to justify ongoing territorial expansion. Suppression requirement rises from 0.42 to 0.68, tracking the state enforcement infrastructure required to suppress Palestinian exit options, secular opposition, and international law objections. Accessibility_collapse at individual and organizational levels reaches 0.58–0.68 by 2023 for believers within settler communities (no credible exit from identity-fused obligation) but remains lower for secular opponents and Palestinians (alternatives exist but are institutionally blocked, not logically eliminated). Resistance is consistently high (0.71 by 2023 across levels) because the constraint is actively contested: Palestinian resistance, secular Israeli opposition, international law advocates, and progressive Jewish movements all challenge the reading's legitimacy. The shared time grid ensures every metric carries the same historical accounting.
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist seat, the constraint is coordination of legitimate theological obligation — the post-1967 territorial gains DO constitute divine signs, settlement IS religious duty, and compromise would be covenant breach. The coordination function is real: it solves the theological problem of interpreting history within Jewish tradition. From the settler community seat, the constraint provides identity fusion and moral certainty that overrides secular law objections. From the Palestinian seat, the constraint is pure extraction — territorial displacement presented as theological foregone conclusion, foreclosing negotiation. From the secular Israeli seat, the constraint is coercive reframing of national identity in religious terms; exit means apostasy or exile. From the international law seat, the constraint is a legitimacy claim moved outside jurisdiction — secular law cannot adjudicate divine obligation. The engine computes these divergences from power, exit_options, and beneficiary/victim structure; they are not authored judgments but structural facts. A religious Zionist settler and a secular Israeli may hold identical power and time horizon but experience radically different d values because one is identity_locked to the theological mandate and the other is not.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist movement (organized, civilizational time horizon, identity_locked): operates as agenda_setter with directionality near 0.0 (full beneficiary — sets the frame, collects theological authority, drives policy). Israeli settler communities (moderate power, generational horizon, identity_locked): role=beneficiary/secondary_payer; d ≈ 0.15–0.25 (substantial net benefit from theological justification despite security and legal costs; identity lock means exit is experienced as apostasy, not merely political disagreement). Palestinian Arabs (powerless, generational, trapped exit): d ≈ 0.95 (full target — displacement is the constraint's direct extraction, reframed as theological necessity). Secular Israeli opposition (moderate, biographical, constrained): d ≈ 0.65–0.75 (high target — bears costs of internal conflict and international delegitimization; constrained exit because nationalist identity ties them to Israeli state even as they reject the religious reading). International law advocates (powerful, biographical, analytical exit): d ≈ 0.55 (near-symmetric: powerful globally but structurally incompatible with the theological frame; their tools are effective only against secular authority, which the reading has captured). Knesset/settlement administration (institutional, generational, constrained): d ≈ 0.35–0.45 (mixed: captures state authority but must manage competing pressures from secular constituencies, international community, and Palestinian resistance). The directionality divergences arise from power asymmetries and exit-option heterogeneity: identical-power secular and religious settlers experience opposite d values because one is identity-locked to the theological mandate and the other is not.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit classic mandatrophy (founding problem solved but mechanism persists). Instead, it exhibits 'mandate expansion': the founding problem (post-1967 theological reinterpretation) remains contested (founding_problem_status: contested), but the constraint has evolved from solving that problem into administering ongoing territorial maximalism. The founding problem itself becomes self-reinforcing: the constraint's institutional success PROVES the mandate's correctness to believers, which JUSTIFIES continued expansion, which GENERATES new theological claims requiring reinterpretation, which STRENGTHENS the authority structure. The theater_ratio rise (0.18 → 0.41) indicates performative maintenance is increasing, but this is not classic piton dynamics (inertial survival despite atrophied function). Rather, the constraint is entering a meta-stable regime where its legitimacy depends on continuous territorial expansion to prove messianic acceleration — if expansion stops, the theological mandate evaporates (the messiah has not arrived, so territorial conquest was mere colonialism). The constraint is trapped in escalation: it must keep expanding to maintain the theological interpretation that justifies its own existence. This is not mandatrophy but 'mandate drift': the original founding problem (how to interpret 1967) has become a permanent justification for territorial escalation, and the escalation itself becomes the new founding problem requiring new theological interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_obligation_vs_political_choice,
    'Is territorial expansion in post-1967 disputed territories a religious obligation binding on the Jewish people, or a political choice justified retrospectively by religious interpretation?',
    'Historical-theological analysis: (1) do pre-1967 Jewish sources contain unambiguous directives to settle the West Bank, or are these interpretations developed POST-1967 to justify the territorial gains? (2) Do messianic sources mandate territorial conquest, or do they describe end-times transformation that transcends territorial logic? (3) Are the post-1967 interpretations novel within Jewish tradition, or continuations of pre-existing minority readings?',
    'If the obligation is binding and pre-existing, the religious restoration reading is authentic theological claim. If the obligation is post-hoc interpretation developed to justify political outcomes, the constraint is a cover story for territorial maximalism under religious framing. This resolves whether the constraint is genuine coordination (solving theological problems) or extraction (using theology to suppress objections).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_obligation_vs_political_choice, empirical, 'Whether messianic territorial mandate is binding Jewish law or constructed justification').

omega_variable(
    identity_lock_mechanism_structural_or_internalized,
    'Is the identity lock binding religious Zionist settlers to settlement expansion primarily structural (economic dependency on settlement infrastructure, legal penalties for leaving, community expulsion) or internalized (belief fusion such that exit is experienced as spiritual death)?',
    'Comparative analysis: (1) Do settlers who lose belief (secular converts, doubters) face structural barriers to exit, or do they exit freely? (2) In cases where settlements were evacuated or dismantled (e.g., 2005 Gaza disengagement), did settlers exhibit primarily material resistance or spiritual crisis? (3) Are exit barriers administered externally (state sanctions, community enforcement) or self-imposed (theological compulsion)?',
    'If lock is primarily structural, exit from settlement is constrained but possible upon removal of barriers — the constraint''s suppression is high but reversible. If lock is primarily internalized, exit from settlement means psychological/identity dissolution — the constraint''s suppression is lower-dimensional (not requiring external enforcement) but more durable. This affects whether dismantling the constraint requires removing infrastructure (cheap fix) or undoing identity fusion (prohibitive fix).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_structural_or_internalized, empirical, 'Whether settler identity lock is structural or internalized').

omega_variable(
    reading_incomparability_with_secular_law,
    'Is the religious restoration reading''s claim to move legitimacy outside secular law jurisdiction a genuine categorical difference (theological claims are inherently non-falsifiable and therefore outside law), or a strategic foreclosure (using theological language to immunize political claims from legal scrutiny)?',
    'Theoretical and comparative analysis: (1) Do other religious authority systems (Catholic canon law, Islamic sharia, Hindu dharma) similarly move legitimacy outside secular law, or is this a specific function of how the religious restoration reading frames its claims? (2) Are there examples where religious obligation and secular law have been reconciled within the same framework, contradicting the claimed incomparability? (3) Do religious Zionist authorities themselves appeal to secular law when convenient (e.g., in diaspora contexts where they lack state power)?',
    'If incomparability is genuine, international law''s inability to adjudicate the constraint reflects structural limits of secular authority over theological claims — the constraint is structurally embedded in authority-system difference. If incomparability is strategic, it is a rhetorical move that could be unmade by reframing the claims in secular terms (e.g., ''the Jewish people''s historical connection to territory'' vs. ''divine promise'') — the constraint would become vulnerable to law-based objections.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_incomparability_with_secular_law, conceptual, 'Whether theological-secular incompatibility is structural or strategic').

omega_variable(
    mandate_expansion_sustainability,
    'Can the constraint sustain indefinite mandate expansion (territorial maximalism justified by messianic acceleration), or does it contain internal limits (either theological satiation — ''the messiah has arrived and boundaries are fixed'' — or practical limits — territory exhaustion or military defeat)?',
    'Theoretical trajectory analysis and theological source study: (1) Do messianic sources in Jewish tradition specify a maximum territory or endpoint at which the messiah arrives, or do they describe open-ended process? (2) Empirically, does the constraint''s theology adapt flexibly to military/political setbacks (reinterpreting defeats as tests of faith, recalibrating territory claims), or do it face theological contradiction from real-world limits? (3) Historical comparison: have other religiously-justified territorial claims achieved satiation and stopped, or do they perpetually expand?',
    'If the constraint has internal theological limits, it may face mandatrophy at territorial satiation — the founding problem (interpreting 1967 gains) could be ''solved,'' and the constraint would persist theatrically without new expansionist claims. If expansion is open-ended and theologically adaptive, the constraint could sustain indefinitely with rising theater_ratio as performance overtakes function. This affects the terminal attractor: bounded territorial state vs. escalating occupation regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_expansion_sustainability, conceptual, 'Whether messianic mandate has intrinsic limits or sustains indefinite expansion').

omega_variable(
    reading_genealogy_authenticity,
    'Is the religious restoration reading a continuity with pre-Zionist Jewish tradition (reading territorial claims as ancient Jewish law correctly recovered), or a novelty specific to post-1967 context (reading territorial conquest as messianic acceleration, a reinterpretation unavailable before territorial control existed)?',
    'Scholarly historical-theological comparison: (1) Do pre-1967 Jewish texts from diverse communities (Sephardic, Ashkenazi, Eastern European, Yemen) contain the specific claims about settlement obligation and messianic acceleration, or do these emerge as interpretation POST-1967? (2) How do religious authorities in non-Zionist Jewish communities (anti-Zionist haredim, diaspora Orthodox authorities) relate to the same sources — do they reach the same conclusions about territorial obligation? (3) What textual and hermeneutical moves were required to generate the post-1967 interpretations — are these standard within Jewish law or unprecedented?',
    'If the reading is authentic continuity, it has deep legitimacy within Jewish tradition and cannot be easily delegitimized as ''invented.'' If the reading is post-1967 novelty, it is vulnerable to counterargument that it is a convenient cover story constructed to justify a political outcome. This affects whether the constraint can be displaced by alternative theological readings (other Jewish voices offering different interpretations of the same tradition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_genealogy_authenticity, empirical, 'Whether religious restoration reading recovers or invents tradition').

omega_variable(
    kernel_coexistence_or_foreclosure,
    'Do the three readings of the legitimacy kernel (religious restoration, national liberation, settler colonial) represent genuinely coexistent interpretations of the same events, or does one reading logically foreclose the others?',
    'Structural-logical analysis: (1) Can a single framework hold all three readings simultaneously (e.g., ''Zionism is both national liberation AND religious restoration AND settler colonialism''), or do they require mutually exclusive premises? (2) Which readings share premises and which contradict? (3) How do disputes over readings arise — do they partition over theoretical axioms (what counts as legitimacy) or over empirical facts (did the events happen)?',
    'If readings are coexistent, the constraint operates in a pluralistic environment where alternative framings remain available — the religious restoration reading must maintain authority against competitors. If one reading forecloses another, the constraint''s authority depends on which reading becomes institutionally dominant (winner-take-all dynamics). This affects the constraint''s stability: coexistent readings suggest ongoing contestation and vulnerability to paradigm shifts; foreclosure dynamics suggest winner-take-all entrenchment once an institutional seat captures state apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_coexistence_or_foreclosure, conceptual, 'Whether kernel readings coexist or logically foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(zion_tr_t1980, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1993, 0.33).
narrative_ontology:measurement(zion_tr_t2005, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(zion_tr_t2015, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(zion_tr_t2023, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2023, 0.41).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.38).
narrative_ontology:measurement(zion_be_t1980, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1993, 0.61).
narrative_ontology:measurement(zion_be_t2005, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(zion_be_t2015, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(zion_be_t2023, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2023, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.42).
narrative_ontology:measurement(zion_su_t1980, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1980, 0.54).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1993, 0.61).
narrative_ontology:measurement(zion_su_t2005, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(zion_su_t2015, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2015, 0.67).
narrative_ontology:measurement(zion_su_t2023, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2023, 0.68).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1967, tn=2023
narrative_ontology:measurement(zion_grid_01, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(class), 1967, 0.38).
narrative_ontology:measurement(zion_grid_02, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(class), 2023, 0.64).
narrative_ontology:measurement(zion_grid_03, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(individual), 1967, 0.35).
narrative_ontology:measurement(zion_grid_04, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(individual), 2023, 0.58).
narrative_ontology:measurement(zion_grid_05, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(organizational), 1967, 0.42).
narrative_ontology:measurement(zion_grid_06, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(organizational), 2023, 0.68).
narrative_ontology:measurement(zion_grid_07, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(structural), 1967, 0.44).
narrative_ontology:measurement(zion_grid_08, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(structural), 2023, 0.71).
narrative_ontology:measurement(zion_grid_09, zionist_legitimacy_basis__religious_restoration_reading, resistance(class), 1967, 0.51).
narrative_ontology:measurement(zion_grid_10, zionist_legitimacy_basis__religious_restoration_reading, resistance(class), 2023, 0.72).
narrative_ontology:measurement(zion_grid_11, zionist_legitimacy_basis__religious_restoration_reading, resistance(individual), 1967, 0.52).
narrative_ontology:measurement(zion_grid_12, zionist_legitimacy_basis__religious_restoration_reading, resistance(individual), 2023, 0.68).
narrative_ontology:measurement(zion_grid_13, zionist_legitimacy_basis__religious_restoration_reading, resistance(organizational), 1967, 0.48).
narrative_ontology:measurement(zion_grid_14, zionist_legitimacy_basis__religious_restoration_reading, resistance(organizational), 2023, 0.74).
narrative_ontology:measurement(zion_grid_15, zionist_legitimacy_basis__religious_restoration_reading, resistance(structural), 1967, 0.44).
narrative_ontology:measurement(zion_grid_16, zionist_legitimacy_basis__religious_restoration_reading, resistance(structural), 2023, 0.71).
narrative_ontology:measurement(zion_grid_17, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(class), 1967, 0.38).
narrative_ontology:measurement(zion_grid_18, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(class), 2023, 0.78).
narrative_ontology:measurement(zion_grid_19, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(individual), 1967, 0.32).
narrative_ontology:measurement(zion_grid_20, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(individual), 2023, 0.73).
narrative_ontology:measurement(zion_grid_21, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(organizational), 1967, 0.41).
narrative_ontology:measurement(zion_grid_22, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(organizational), 2023, 0.81).
narrative_ontology:measurement(zion_grid_23, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(structural), 1967, 0.51).
narrative_ontology:measurement(zion_grid_24, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(structural), 2023, 0.84).
narrative_ontology:measurement(zion_grid_25, zionist_legitimacy_basis__religious_restoration_reading, suppression(class), 1967, 0.41).
narrative_ontology:measurement(zion_grid_26, zionist_legitimacy_basis__religious_restoration_reading, suppression(class), 2023, 0.71).
narrative_ontology:measurement(zion_grid_27, zionist_legitimacy_basis__religious_restoration_reading, suppression(individual), 1967, 0.28).
narrative_ontology:measurement(zion_grid_28, zionist_legitimacy_basis__religious_restoration_reading, suppression(individual), 2023, 0.62).
narrative_ontology:measurement(zion_grid_29, zionist_legitimacy_basis__religious_restoration_reading, suppression(organizational), 1967, 0.35).
narrative_ontology:measurement(zion_grid_30, zionist_legitimacy_basis__religious_restoration_reading, suppression(organizational), 2023, 0.74).
narrative_ontology:measurement(zion_grid_31, zionist_legitimacy_basis__religious_restoration_reading, suppression(structural), 1967, 0.48).
narrative_ontology:measurement(zion_grid_32, zionist_legitimacy_basis__religious_restoration_reading, suppression(structural), 2023, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_legitimacy_basis__indigenous_rights_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, international_law_settlement_illegality).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel zionist_legitimacy_basis. The kernel splits into three distinct constraint stories: (1) religious_restoration_reading (this file) — reads post-1967 events as divine fulfillment and messianic acceleration; (2) national_liberation_reading — reads events as return of indigenous persecuted people; (3) settler_colonial_reading — reads events as European settlement project. Each has distinct ε values, beneficiary/victim structures, and authority grounds. They are linked via network.affects_constraints because the dominance of one reading affects the availability of the others: religious restoration's institutional authority marginalizes national liberation narratives within Israeli policy and suppresses settler-colonial analysis in mainstream discourse. The readings do not have independent ε values that could be averaged — each reading assesses the SAME referent (post-1967 territorial control and settlement) from incommensurable authority frames (divine law vs. nationalist self-determination vs. imperial occupation). Decomposition into three separate stories respects the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
