% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Israeli Territorial Legitimacy (Zionist Refuge Reading)
 *   domain: political/territorial
 *
 * SUMMARY:
 *   Israel's territorial legitimacy in this reading is grounded in three
 *   interconnected claims: (1) Jewish historical persecution, culminating in
 *   the Holocaust, creates an existential necessity for a sovereign Jewish
 *   refuge territory; (2) Divine covenant and religious tradition establish a
 *   rightful connection between Jewish peoplehood and the land of Israel; (3)
 *   UN General Assembly partition resolution 181 (1947) provided
 *   international legal endorsement for establishing a Jewish state in
 *   Palestine. This reading frames the 1948 establishment as legitimate and
 *   uncontested; the 1967 occupation and subsequent settlement expansion as
 *   security-necessitated extensions of that legitimacy; and Palestinian
 *   displacement as the consequence of Arab state rejection of partition
 *   rather than as a primary consequence of Israeli action. The constraint
 *   operates by enforcing territorial boundaries, restricting Palestinian
 *   return and political rights, justifying settlement expansion, and
 *   suppressing Palestinian counter-claims to the same territory. The reading
 *   is contested — Palestinian autochthony and two-state compromise readings
 *   offer structurally different legitimacy grounds — but within this
 *   reading's internal logic, it is coherent and self-justifying.
 *
 * KEY AGENTS:
 *   - Jewish diaspora communities: historically persecuted, seeking refuge and self-determination; identity-locked to the legitimacy narrative of persecution and survival.
 *   - Israeli state apparatus: agenda-setter; administers territory, citizenship, settlement, and security enforcement; collects the monopoly on political authority over the territory.
 *   - Palestinian displacement bearers: powerless payers; carry the tangible cost of territorial loss, refugee status, severed livelihoods; their displacement is framed within this reading as a consequence of Arab rejection rather than primary causation.
 *   - Palestinian territorial claimants: moderately powerful but structurally excluded from the legitimacy authorization; their counter-claims are framed as superseded by partition and security necessity; their voice is absent from the constraint's founding logic.
 *   - Settlement expansion constituencies: organized beneficiaries; interpret the divine covenant and persecution narratives as extending territorial claims beyond 1948 boundaries; identity-locked to territorial expansion.
 *   - International partition authorities (UN): observer seat invoked for 1948 legitimacy; ambiguous on post-1967 boundaries and settlement; the reading treats them as having authorized the establishment but not the expansion.
 *   - Arab state actors: excluded; would contest that partition was imposed without Palestinian consent and that Palestinian displacement was a consequence of dispossession rather than Arab rejection; their refusal to endorse partition is treated as settling the matter against them within this reading.
 *   - Security apparatus beneficiaries: institutional beneficiaries; gain power and budgetary allocation from the security justification of territorial control; benefit from the threat framing that perpetuates the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Israeli Territorial Legitimacy (Zionist Refuge Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '3f5db7cc-22ec-459f-80eb-1b639b3ef929').
narrative_ontology:cs_kernel_codification('3f5db7cc-22ec-459f-80eb-1b639b3ef929', fixed_text).
narrative_ontology:cs_authority_grounding('3f5db7cc-22ec-459f-80eb-1b639b3ef929', extraction).
narrative_ontology:cs_interpretation_layer_present('3f5db7cc-22ec-459f-80eb-1b639b3ef929').
narrative_ontology:cs_reading_relation('3f5db7cc-22ec-459f-80eb-1b639b3ef929', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f5db7cc-22ec-459f-80eb-1b639b3ef929', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('3f5db7cc-22ec-459f-80eb-1b639b3ef929', foundational, jewish_persecution_necessitates_territorial_refuge).
narrative_ontology:cs_axiom_status(jewish_persecution_necessitates_territorial_refuge, holdable).
narrative_ontology:cs_axiom_grounding('3f5db7cc-22ec-459f-80eb-1b639b3ef929', jewish_persecution_necessitates_territorial_refuge, empirically_contingent).
narrative_ontology:cs_axiom('3f5db7cc-22ec-459f-80eb-1b639b3ef929', foundational, un_partition_authorizes_jewish_statehood).
narrative_ontology:cs_axiom_status(un_partition_authorizes_jewish_statehood, holdable).
narrative_ontology:cs_axiom_grounding('3f5db7cc-22ec-459f-80eb-1b639b3ef929', un_partition_authorizes_jewish_statehood, conventional).
narrative_ontology:cs_axiom('3f5db7cc-22ec-459f-80eb-1b639b3ef929', secondary, divine_covenant_ground_territorial_claim).
narrative_ontology:cs_axiom_status(divine_covenant_ground_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('3f5db7cc-22ec-459f-80eb-1b639b3ef929', divine_covenant_ground_territorial_claim, theological).
narrative_ontology:cs_axiom('3f5db7cc-22ec-459f-80eb-1b639b3ef929', secondary, security_justifies_territorial_control_expansion).
narrative_ontology:cs_axiom_status(security_justifies_territorial_control_expansion, holdable).
narrative_ontology:cs_axiom_grounding('3f5db7cc-22ec-459f-80eb-1b639b3ef929', security_justifies_territorial_control_expansion, instrumental).
narrative_ontology:cs_reference_frame('3f5db7cc-22ec-459f-80eb-1b639b3ef929', jewish_refuge_territorial_necessity).
narrative_ontology:cs_drift_state('3f5db7cc-22ec-459f-80eb-1b639b3ef929', contemporary_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f5db7cc-22ec-459f-80eb-1b639b3ef929', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_refuge_seekers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, settlement_expansion_constituencies).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_displacement_bearers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_territorial_claimants).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 at interval end) because the reading involves forcibly moving territorial control and political rights from one population to another, irrespective of the reading's internal justification. Suppression is authored higher (0.72) because the persistence of the constraint depends not on Palestinian or Arab acceptance but on active military and institutional enforcement against Palestinian return and territorial claims. Theater ratio is moderate (0.41) because the reading genuinely coordinates a real security function and a real identity-solidarity function for diaspora and settler constituencies, but a growing share of the constraint's operation defends settlement expansion and displacement permanence rather than addressing the original persecution-refuge problem. Accessibility collapse is moderate (0.62) because Palestinians theoretically have the exit option of international legal challenge, armed resistance, or migration, but in practice they are trapped by military occupation, refugee-camp confinement, and the reading's framing that delegitimizes all their counter-claims. Resistance is high (0.78) because Palestinian resistance movements, international law scholarship, and Arab states actively contest this reading and advance alternatives; the constraint persists despite substantial organized opposition. The measurement series spans 1900–2024 to show the reading's crystallization: pre-1920 it barely exists; 1920–1948 it accumulates legitimacy claims and institutional infrastructure; 1948 it reaches its initial coherence; 1967 extractiveness jumps as territorial expansion occurs; 1967–2024 it stabilizes at high extraction with modest theater and suppression drift upward. This trajectory reflects the reading's functional change: originally motivated by refuge necessity, increasingly driven by settlement expansion and security justification.
 *
 * PERSPECTIVAL GAP:
 *   The zionist_refuge_reading and the palestinian_autochthony_reading are not different observations of the same constraint; they are genuinely different constraints sharing the same territory. From the Israeli institutional seat, this reading is legitimate coordination: the state provides refuge for persecuted Jews and security for Jewish self-determination, justified by historical necessity and international endorsement. From the Palestinian seat (especially the displacement bearers), the same structure operates as enforced extraction: dispossession of land, denial of return, confinement to occupied territories or diaspora, and a legitimacy narrative that does not acknowledge their own territorial claim. The engine computes per-seat types from structural data — the Israeli agenda-setter seat should compute this as rope or weak tangled_rope (coordination + some asymmetry), while the Palestinian payer seats should compute as snare (pure extraction with a suppressed counter-claim). This divergence is NOT an error; it is the measurement the system exists to take. The authored claim is deliberately zionist_refuge_reading (the reading's own framing) while the metrics describe a substantially extractive, actively enforced arrangement — the gap documents the reading's contested status.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the Israeli state apparatus sits near 0.0 (pure beneficiary): it collects territorial control, political authority, and security guarantees without running a coordination mechanism that constrains it. Exit is arbitrage-grade — Israel can reframe the justification (security, divine covenant, historical necessity) as circumstances change and maintain territorial claims across multiple legitimacy narratives. Directionality for diaspora refuge-seekers sits around 0.2 (high beneficiary): they gain institutional safety and collective self-determination without operating the state apparatus directly, but their identity is fused with the legitimacy narrative, making exit ideologically impossible (exit = abandoning the refuge claim = abandoning Jewish survival framing). Directionality for settlement expansion constituencies sits near 0.15 (beneficiary): they gain territorial acquisition, ideological vindication, and land rights grounded in divine covenant; like diaspora beneficiaries, their identity is fused with territorial claims (exit = abandoning religious Zionism). Directionality for Palestinian displacement bearers sits near 0.95 (full target): they bear all costs (displacement, territorial loss, refugee status, military occupation) with no voice in the legitimacy authorization and no exit except emigration or acceptance of permanent statelessness. Directionality for Palestinian territorial claimants sits near 0.85 (near-target): they claim the same territory but are framed as illegitimate by the reading and subject to enforcement that delegitimizes their claims. Their exit is constrained — remaining in the territory means accepting the reading's framing of their illegitimacy, while leaving means abandoning the territorial claim. Security apparatus beneficiaries sit near 0.2 (beneficiary): they gain power and resources from the constraint without direct accountability to the displaced; their exit is mobile (they can shift to other security roles) but their institutional interest aligns with the constraint's persistence. These directionalities are derived from the beneficiary/victim declarations and exit options; no overrides are required because the structural mapping is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish persecution and genocide, existential vulnerability) is real and historically documented. The 1948 establishment (refuge + partition endorsement) addresses that problem coherently. However, the founding problem's status has shifted over the interval: by 1967, regional military threats remained (1967 war, ongoing border conflicts), but the primary threat to Jewish survival had shifted from European persecution to state-level security. By the 1990s, the founding problem's original character had substantially attenuated in historical significance, though security concerns persisted. By 2024, the founding problem as it was originally framed (Jewish diaspora vulnerability to genocide) is historical rather than live — present-day Jewish communities in most diaspora locations are secure, and internal threats within Israel have become the dominant security narrative. The constraint's justification has shifted from addressing persecution-refugee need to addressing security-territorial expansion. This is the signature of mandatrophy: the constraint was built to solve problem X; problem X is no longer live; the constraint persists by reframing its justification to address problem Y (security and settlement). The two-state compromise reading explicitly identifies this problem (founding problem dead, constraint still live) and proposes a boundary condition (1967 lines + mutual recognition). The zionist_refuge_reading does not resolve mandatrophy; instead, it preserves the original legitimacy claim while expanding the constraint's scope through supplementary justifications (security, divine covenant, settlement). The engine should flag this as potential mandatrophy: founding_problem_status='contested' (because Israel treats the security problem as functionally continuous with the original persecution problem, while critics argue the founding problem is obsolete) combined with disappearance_verdict='world_rearranges' (because if the constraint vanished, the territorial order would reorganize) and measurement trajectory showing theater ratio rising (increasing proportion of the constraint's operation devoted to defending settlement and security rather than addressing the original refugee need). This is precisely where false-summit detection and mandatrophy resolution interlock.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_persecution_justification_boundary,
    'Does the historical persecution of European Jews create a legitimate territorial claim in Palestine, or does it create a legitimate claim to refuge status and protection without necessarily justifying displacement of an indigenous population that did not perpetrate the persecution?',
    'Conceptual analysis of responsibility and redress: who bears the obligation to provide refuge for persecution victims, and can that obligation fall on third parties not responsible for the persecution? International law scholarship on collective self-determination rights vs. individual displacement claims.',
    'If persecution justifies territorial claim only (not displacement): the reading''s scope narrows to 1948 boundaries with Palestinian return rights. If displacement is justified by security (the reading''s implicit answer): the full scope including settlement expansion remains coherent. The boundary question maps the reading''s logical coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_persecution_justification_boundary, conceptual, 'Whether historical persecution can justify dispossessing third parties not responsible for the persecution.').

omega_variable(
    divine_promise_secularization,
    'When divine covenant and religious land claims are translated into secular territorial sovereignty, what remains of the divine promise claim''s legitimacy in an international legal order that rests on consent and secular ground?',
    'Historical analysis of how the Zionist movement itself navigated this transition (secular vs. religious Zionism debate); documentation of which legitimacy grounds Israel actually invokes in international forums (UN testimony, legal briefs) vs. which it emphasizes domestically to constituencies grounded in religious identity.',
    'If divine promise loses force in translation to secular law: the reading''s legitimacy foundation rests entirely on historical persecution + UN partition, narrowing its scope. If divine promise grounds are preserved as supplementary legitimacy for religious constituencies: the reading fragments into secular and religious sub-readings with different scopes (1948 vs. settlement expansion). Affects whether the constraint can hold coherently across the secular/religious divide.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_secularization, conceptual, 'Whether theological land claims translate into secular territorial rights or require separate legitimacy grounds.').

omega_variable(
    partition_endorsement_scope_creep,
    'Does UN partition resolution 181 endorse only 1947-1948 boundaries, or does it authorize territorial expansion beyond partition lines if security conditions require it?',
    'Textual analysis of UN resolution 181 and subsequent General Assembly statements; documentation of whether the UN partition plan''s legitimacy endorsement has been invoked to justify post-1967 territorial acquisition (which it has not been; all Israeli governments have treated 1967 as military conquest rather than UN-authorized expansion).',
    'If partition endorses only 1948 boundaries: post-1967 territories require separate legitimacy grounds (security, divine covenant, or historical claim). If partition can be extended: the entire reading can rest on UN authority. The evidence shows Israel''s own legal position treats partition as 1948-specific, making this omega reflect the reading''s internal boundary maintenance — 1948 legitimacy is UN-secured; 1967+ requires supplementary justification (security and covenant claims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_endorsement_scope_creep, empirical, 'Whether partition authorization extends to post-1967 territorial claims or stops at 1948 boundaries.').

omega_variable(
    arab_rejection_causal_attribution,
    'Is Palestinian displacement the result of Arab state rejection of partition (the reading''s framing), or the result of Israeli territorial expansion and military strategy during the 1948 war and subsequent occupation?',
    'Historical documentation from all sides: Israeli declassified documents (Benny Morris, Avi Shlaim), Palestinian archives, Arab state records. Analysis of causality: was displacement an inevitable consequence of partition rejection, or a strategic choice made by Israeli military and political leadership?',
    'If displacement is primarily Arab-caused: the reading''s framing holds; Palestinians bear the cost of their own rejection. If displacement is primarily Israeli-caused: the reading''s ethical foundation narrows; it cannot claim to have merely defended partition but must acknowledge territorial expansion as causally primary. Affects whether victims are framed as bearing a cost imposed by third parties (Arab states) or by the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_rejection_causal_attribution, empirical, 'Whether Palestinian displacement is caused by Arab rejection or by Israeli strategy.').

omega_variable(
    security_justification_calibration,
    'What level of Palestinian territorial control and self-determination is compatible with legitimate Israeli security concerns, and at what point does security justification become a perpetual veto on Palestinian political rights?',
    'Security studies analysis of Israeli security threats (from neighboring states, from Palestinian armed groups) and assessment of how much territorial control and Palestinian statelessness is necessary to address those threats vs. how much exceeds the proportional boundary. Comparative analysis with other security situations where territorial occupation persists (Cyprus, Kashmir, Crimea) and their resolution outcomes.',
    'If security justifies indefinite territorial control: the reading can accommodate permanent occupation and settlement expansion as security-necessitated. If security justification has limits: the reading''s scope narrows to reversible security measures (military presence, demilitarization agreements) rather than permanent territorial acquisition and settlement. Affects whether the constraint is temporally bounded or permanent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_justification_calibration, empirical, 'How much Palestinian statelessness and territorial control is necessary vs. excessive for Israeli security.').

omega_variable(
    reading_kernel_contest_location,
    'This constraint instantiates the zionist_refuge_reading of the territorial_legitimacy_dual kernel. The contested kernel is a stabilized commitment (the territorial arrangement grounded in 1948 legitimacy) that different parties read differently. What specific structural elements of the kernel do the sibling readings contest, and are those contestations located in the legitimacy authorization or in the scope of territorial claims?',
    'Comparative analysis of the three kernel readings: the zionist_refuge_reading (this constraint) grounds legitimacy in historical persecution + divine promise + UN partition; the palestinian_autochthony_reading grounds legitimacy in continuous habitation + indigenous presence + displacement trauma; the two_state_coexistence_reading grounds legitimacy in mutual recognition with 1967 boundaries as compromise. All three are readings of the SAME kernel — territorial sovereignty in Palestine — but they differ on who has the right to be there and what territory they have a right to control.',
    'The kernel contest is real; the readings are not talking past each other but genuinely disagreeing about the legitimacy grounding and territorial scope. This omega documents that the contest is structural, not rhetorical — it affects how the engine should weight cross-reading contamination analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_location, conceptual, 'The location of the kernel contest: legitimacy authorization vs. territorial scope vs. both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1900, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(terr_tr_t1920, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(terr_tr_t1940, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1940, 0.18).
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(terr_tr_t1990, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1990, 0.39).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(terr_be_t1900, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1900, 0.0).
narrative_ontology:measurement(terr_be_t1920, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(terr_be_t1940, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1940, 0.38).
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.52).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.64).
narrative_ontology:measurement(terr_be_t1990, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1900, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1900, 0.0).
narrative_ontology:measurement(terr_su_t1920, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement(terr_su_t1940, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1940, 0.48).
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.61).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.71).
narrative_ontology:measurement(terr_su_t1990, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1990, 0.71).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_right_of_return_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, israeli_security_doctrine_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, settlement_expansion_legitimacy_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three kernel readings decomposing the territorial_legitimacy_dual kernel. The zionist_refuge_reading grounds legitimacy in historical persecution, divine promise, and UN partition. The palestinian_autochthony_reading grounds legitimacy in continuous habitation, indigenous presence, and displacement trauma. The two_state_coexistence_reading treats both legitimacy claims as valid and proposes mutual recognition with 1967 boundaries as compromise framework. These are not alternative measurements of one constraint; they are genuinely different constraints grounded in incommensurable legitimacy claims about the same territory. The ε values differ substantially: zionist_refuge_reading shows moderate-to-high extractiveness (0.68) because territorial transfer and displacement are core to its operation; palestinian_autochthony_reading would show similarly high extractiveness from the opposite seat; two_state_coexistence_reading would show lower extractiveness because it attempts to distribute territorial claims symmetrically (though the actual distribution remains contested). All three stories link via network.affects_constraints to document their kinship and mutual contamination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
