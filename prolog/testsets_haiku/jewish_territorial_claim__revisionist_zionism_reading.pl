% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionism: Maximalist Territorial Claim with Military Enforcement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   The revisionist Zionism reading of the Jewish territorial claim asserts
 *   maximalist boundaries (both banks of the Jordan River), rejects Arab
 *   consent as a prerequisite for Jewish sovereignty, and treats military
 *   force (the 'Iron Wall' doctrine) as the primary mechanism to compel
 *   regional acceptance. This reading emerges from early 20th-century Zionist
 *   political thought, particularly associated with Vladimir Jabotinsky and
 *   his followers, in direct contrast to labor Zionism's gradualism,
 *   political Zionism's negotiation-based approach, and cultural Zionism's
 *   emphasis on spiritual rather than territorial claims. The constraint
 *   operates as a snare: it extracts Palestinian land and autonomy through
 *   coercive settlement and military occupation, maintains suppression
 *   through the doctrine that Arab voices are irrelevant to the outcome, and
 *   persists because the beneficiary seats (revisionist leadership, settlers,
 *   military apparatus) have fused their identity and institutional power
 *   with the territorial claim.
 *
 * KEY AGENTS:
 *   - Revisionist Zionist leadership: sets the maximalist territorial doctrine and frames military enforcement as necessity
 *   - Jewish settlers in occupied territories: occupy Palestinian land as the mechanism converting claim to fact; identity-locked to settlement presence
 *   - Israeli military apparatus: enforces occupation and settlement protection; benefits from expanded institutional power and doctrine that military force is the only viable language
 *   - Palestinian Arabs: lose land, lose self-determination, lose consent in the arrangement governing their fate; structurally trapped
 *   - Arab state actors: reject the maximalist claim but face asymmetric military pressure and fait accompli settlement
 *   - Diaspora Jewish community: receive symbolic benefit from Jewish power narrative; can exit but face identity pressure
 *   - Alternative Zionist strands (political, labor, cultural): excluded from revisionist agenda-setting by the doctrine that compromise is weakness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.88).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.91).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionism: Maximalist Territorial Claim with Military Enforcement").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '285ea16c-9987-42bd-beb4-37b3208a6085').
narrative_ontology:cs_kernel_codification('285ea16c-9987-42bd-beb4-37b3208a6085', formalized).
narrative_ontology:cs_authority_grounding('285ea16c-9987-42bd-beb4-37b3208a6085', extraction).
narrative_ontology:cs_interpretation_layer_present('285ea16c-9987-42bd-beb4-37b3208a6085').
narrative_ontology:cs_reading_relation('285ea16c-9987-42bd-beb4-37b3208a6085', jewish_territorial_claim__political_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('285ea16c-9987-42bd-beb4-37b3208a6085', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('285ea16c-9987-42bd-beb4-37b3208a6085', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('285ea16c-9987-42bd-beb4-37b3208a6085', foundational, territorial_maximalism_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_maximalism_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('285ea16c-9987-42bd-beb4-37b3208a6085', territorial_maximalism_non_negotiable, deontological).
narrative_ontology:cs_axiom('285ea16c-9987-42bd-beb4-37b3208a6085', foundational, military_force_necessity_for_acceptance).
narrative_ontology:cs_axiom_status(military_force_necessity_for_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('285ea16c-9987-42bd-beb4-37b3208a6085', military_force_necessity_for_acceptance, empirically_contingent).
narrative_ontology:cs_axiom('285ea16c-9987-42bd-beb4-37b3208a6085', foundational, arab_consent_dispensable).
narrative_ontology:cs_axiom_status(arab_consent_dispensable, holdable).
narrative_ontology:cs_axiom_grounding('285ea16c-9987-42bd-beb4-37b3208a6085', arab_consent_dispensable, deontological).
narrative_ontology:cs_reference_frame('285ea16c-9987-42bd-beb4-37b3208a6085', jewish_historical_territorial_claim_to_greater_palestine).
narrative_ontology:cs_drift_state('285ea16c-9987-42bd-beb4-37b3208a6085', post_world_war_two_state_establishment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('285ea16c-9987-42bd-beb4-37b3208a6085', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlement_enterprise).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, existing_arab_state_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_in_occupied_territories).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, israeli_military_apparatus).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, diaspora_jewish_community).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_state_actors).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, jewish_historical_claim_to_greater_palestine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, military_force_as_prerequisite_to_jewish_sovereignty).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, arab_consent_dispensable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the territorial maximalism doctrine: claim to both banks of the Jordan as the minimal acceptable boundary; frames Jewish historical right as non-negotiable. Organizes the settlement movement, justifies military force as the sole mechanism that will compel Arab acceptance of Jewish sovereignty. Their identity is fused with the territorial claim — exit from maximalism is experienced as exit from Jewishness itself in this reading. Controls the narrative of Jewish self-determination within this strand of Zionism.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership, agenda_setter,
    organized, generational, identity_locked, global).

% Occupy Palestinian land in the West Bank and East Bank under the revisionist claim. Receive military protection, subsidized land allocation, infrastructure investment, and ideological legitimation from the state. Their settlement activity is the mechanism that converts territorial claim into established fact. Exit means abandoning their homes, their community identity, and their stake in the territorial claim — identity is constituted through settlement presence.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_in_occupied_territories, beneficiary,
    organized, generational, identity_locked, regional).

% Enforces the territorial claim through occupation, settlement protection, and military operations against Palestinian resistance. Expands institutional power and budget justification through the enforcement requirement. Frames military force as the only language Arabs will understand — the doctrine of the 'Iron Wall'. Their role is simultaneously beneficiary (expanded institutional power, security budget) and agenda-setter (they interpret what 'compulsion' means operationally).
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, israeli_military_apparatus, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, israeli_military_apparatus, agenda_setter).

% Lose land, lose self-determination, lose the ability to consent to arrangements governing their territory and political future. Subjected to military occupation, settlement displacement, and the structural logic that their consent is irrelevant — the constraint operates precisely to exclude their voice from the determination of their own fate. Exit options are severely constrained: displacement within occupied territory, refugee status in neighboring states, or armed resistance met with overwhelming force.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs, payer,
    powerless, biographical, trapped, regional).

% Face the maximalist claim as a threat to regional balance and Palestinian sovereignty. Their consent is explicitly rejected by the revisionist doctrine — the constraint operates to compel their acceptance of Jewish sovereignty over territory they consider Arab, using military force and fait accompli settlement as the mechanism. Their exit options include military response (costly and asymmetric), diplomatic isolation of the Jewish state (ineffective), or resigned accommodation after the 'Iron Wall' demonstrates the cost of resistance exceeds the benefit.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_state_actors, payer,
    powerful, generational, constrained, regional).

% Receives symbolic and political benefit from the claim to Jewish sovereignty and territorial maximalism — the reading provides a narrative of Jewish power and self-determination that addresses historical powerlessness and antisemitism. They can exit (assimilate, support competing Zionist readings, dissent from revisionism) but face identity and community pressure. Their material exposure is lower than settlers or the Israeli state.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, diaspora_jewish_community, beneficiary,
    organized, generational, mobile, global).

% Monitor the territorial claim's enforcement and its stability. Some powers (historically the Soviet Union for Arab states, the United States for Israel) align with one side. Others attempt mediation or neutrality. Their observations feed into whether the 'Iron Wall' doctrine is sustainable long-term or whether the cost of maintaining it exceeds what the international system will tolerate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_powers, observer,
    institutional, generational, analytical, global).

% Would advocate for negotiated Jewish statehood with defined, defensible borders — not maximalist claim to both banks of Jordan. They are excluded from the revisionist agenda-setting by the logic that consent-based negotiation is weakness, that the territorial claim is non-negotiable, and that military force alone will produce the desired outcome. Their voice would question whether the maximalist claim is strategically sustainable or morally defensible.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, political_zionism_alternative_strand, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish national project under the revisionist reading: unifies territorial maximalism, military enforcement, and settlement as a single coherent strategy for establishing Jewish sovereignty over the claimed territory. The coordination claim is that only through unified territorial claim and iron-willed military enforcement can Jewish self-determination be secured against Arab resistance.
% TRANSFER_FUNCTION: Transfers Palestinian land, Palestinian political autonomy, and Palestinian life-prospects to the revisionist Jewish claim and the settler-colonial enterprise. Transfers regional military and political power from Arab states to the Jewish state. The transfer is enforced through military occupation, settlement displacement, and the explicit doctrine that Arab consent is irrelevant.
% ABSENT_VOICES: Palestinian leadership and Arab states are structurally excluded from consent-based deliberation by the revisionist doctrine itself — the constraint operates to rule out negotiation as a valid mechanism. Political Zionist and labor Zionist strands, which would advocate for negotiation, consent-based boundaries, and gradual integration rather than maximalism, are also excluded from the revisionist agenda-setting. International human-rights advocates, who would voice objection to the displacement and suppression mechanisms, are similarly blocked from influencing the constraint's operation.
% DISAPPEARANCE_RATIONALE: If the revisionist territorial claim and its military enforcement vanished overnight, Palestinian self-governance would reassert itself, Arab states would reclaim or negotiate the territory without the coercive exclusion of Arab consent, settlement communities would face displacement or integration into a different political order, and the Israeli state would redefine its territorial boundaries through negotiation rather than maximalist assertion. The entire regional balance would reorganize around consent-based rather than coercion-based arrangements.
% FOUNDING_PROBLEM: Jewish historical and religious connection to the land of Palestine; the Jewish Question in Europe (antisemitism, persecution, statelessness); the need for Jewish refuge and self-determination in response to European violence and precarity.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist Zionist leadership and their historian allies attest the founding problem is live and requires maximalist territorial solution. Labor Zionists, political Zionists, and cultural Zionists attest the founding problem is real but does NOT require territorial maximalism or rejection of Arab consent — alternatives are structurally viable. Palestinian historians and Arab state actors attest the founding problem does not justify the displacement and suppression of the Palestinian people who did not cause European antisemitism. International historians and Holocaust scholars corroborate that European antisemitism is a foundational grievance, but do NOT corroborate that maximalist territorial claim is the necessary or legitimate response. The status remains contested across all external corroborating voices.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88 at 1948) is high because the constraint systematically transfers Palestinian land and political autonomy to the revisionist claim without Palestinian consent — the entire operation is extractive from the Palestinian perspective, and extraction is the point. Suppression (0.91) is very high because the constraint's persistence depends on military occupation, settlement displacement, and the explicit doctrine that Arab consent is irrelevant and that resistance must be crushed. The 'Iron Wall' doctrine is suppression formalized as strategy. Theater ratio (0.22) is relatively low because the settlement activity and military occupation are functionally extractive, not performative — the machinery does what it claims to do (compel acceptance through military force), even though legitimating narratives about Jewish historical rights and security wrap the operation. The measurement series shows steady acceleration from 1880 (when revisionist ideology was nascent) through 1948 (when the state was declared and maximalist territorial claim was operationalized through military force and settlement). The coercion grid shows suppression intensifying at all levels — individual Palestinians losing freedom of movement, organizational Palestinian resistance meeting military overwhelming force, the Palestinian class losing collective political voice, and the structural arrangement tilting toward Jewish-state dominance. Resistance also rises across all levels, indicating that Palestinians, Arab states, and even alternative Jewish voices mount real opposition that the constraint must actively suppress.
 *
 * PERSPECTIVAL GAP:
 *   From the revisionist leadership seat: necessity for Jewish survival and self-determination, justified by historical claim and Arab hostility. From the Palestinian seat: dispossession, military occupation, systematic exclusion from consent. From alternative Zionist seats: strategic overreach that guarantees perpetual conflict. From Arab states: regional hegemony threat. From international powers: national interest alignment. The structural asymmetry is that the agenda-setter (revisionist leadership + military) controls what counts as a reason and imposes it through force, while the target (Palestinian Arabs) must bear the outcome without choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Revisionist Zionist leadership and the Israeli military apparatus sit near the full-beneficiary end of directionality (d near 0.0): they set the terms, control the mechanism, and collect the political and territorial gains. Jewish settlers sit between beneficiary and symmetric (d near 0.2-0.3): they gain land and identity affirmation, but live under the structural logic that military force is necessary to maintain their presence — they are not attacked because they benefit from the system, but because they ARE the system's primary mechanism. Palestinian Arabs and Arab states sit near the full-target end (d near 1.0): they lose without choice, and the constraint's operation is precisely to exclude their voice from determination of their own fate. Diaspora Jews sit near symmetric (d near 0.5): they benefit symbolically from Jewish power and self-determination, but carry little material exposure and can exit. Alternative Zionist strands sit at the excluded-but-powerful end: they have organizational capacity and voice, but are systematically blocked from agenda-setting by the revisionist doctrine that compromise is weakness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish persecution, statelessness, antisemitism in Europe) is real and corroborated by Holocaust history and European Jewish experience. The revisionist reading asserts that maximalist territorial claim and military enforcement are the necessary solution. The founding_problem_status is 'contested' because: (1) alternative Zionist readings solve the same founding problem (Jewish refuge and self-determination) without territorial maximalism or rejection of Arab consent; (2) Palestinian and Arab corroboration explicitly rejects that the European Jewish Question justifies Palestinian dispossession; (3) labor Zionism and political Zionism developed parallel solutions to the same founding problem. The constraint persists not because it is the only solution to the founding problem, but because revisionist ideology and the settlement-military coalition have locked other Israeli institutional actors into the maximalist claim. A mandatrophy evaluation would flag: the constraint solves the founding problem (Jewish self-determination and security) in one reading, but at the cost of creating a new problem (Palestinian statelessness, perpetual conflict) that the beneficiary seats treat as acceptable because Palestinians are excluded from consent. This is a case where solving one group's founding problem by imposing unsolved problems on another group — with those others explicitly excluded from decision-making — is the structure of the snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jewish_historical_claim_legitimacy,
    'Does Jewish historical and religious connection to the land of Palestine constitute a legitimate basis for territorial claim that overrides Palestinian presence and self-determination?',
    'Historical scholarship from non-partisan sources on Jewish presence in Palestine (pre-Ottoman, Ottoman, early modern periods); legal and ethical frameworks on historical claim vs. current inhabitancy; comparative analysis with other settler-colonial claims and their legitimacy status.',
    'If historical claim legitimizes displacement, revisionist maximalism is justified as recovery of ancestral homeland. If current inhabitancy and self-determination override historical claim, the constraint is pure dispossession. The disagreement is foundational to whether maximalism is justice or colonialism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jewish_historical_claim_legitimacy, conceptual, 'Whether Jewish historical presence legitimizes territorial claim overriding Palestinian self-determination.').

omega_variable(
    iron_wall_doctrine_sustainability,
    'Can military force perpetually suppress Arab resistance to the maximalist territorial claim, or does the asymmetry of military power eventually collapse under the weight of sustained resistance and international pressure?',
    'Post-1948 historical trajectory: do Arab-Israeli conflicts diminish with military domination, or escalate? Do international legal and human-rights frameworks eventually constrain the military mechanism? Do Palestinian resistance movements grow or weaken over time?',
    'If the Iron Wall is sustainable, maximalism is strategically viable and the constraint persists indefinitely. If the mechanism eventually fails, maximalism is strategically unsustainable and the constraint must be renegotiated or abandoned. This determines whether the snare is stable or eventually collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_doctrine_sustainability, empirical, 'Whether military enforcement can indefinitely sustain the maximalist claim against Arab resistance.').

omega_variable(
    consent_dispensability_in_justice_frameworks,
    'Is it legitimate to exclude the voices and consent of those most affected (Palestinian Arabs) from determination of arrangements governing their own fate, even if the excluding party justifies it as necessary for their own survival?',
    'Ethical and legal analysis of consent requirements in justice frameworks; comparison with other cases where one party excluded another''s consent on security grounds; evolution of international human-rights norms regarding self-determination and consent.',
    'If consent can be dispensed with in survival circumstances, the revisionist reading''s exclusion of Arab voices is justified. If consent is inalienable, the constraint is fundamentally unjust regardless of its security rationale. This determines whether the suppression mechanism is legitimate or criminal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_dispensability_in_justice_frameworks, preference, 'Ethical status of consent-dispensability in justice frameworks.').

omega_variable(
    revisionist_vs_alternative_zionist_kernel_foreclosure,
    'Does the revisionist reading''s core premise (territorial maximalism + military force + consent-dispensability) logically foreclose the political Zionism reading (negotiated boundaries + majority-Jewish state + international legitimacy)?',
    'Conceptual analysis of the two readings'' core premises: are they contradictory within any single framework, or do they represent different strategic choices within overlapping goal-sets? Can a Jewish state with negotiated boundaries coexist with a maximalist claim, or does accepting one entail rejecting the other?',
    'If revisionist forecloses political Zionism, the readings cannot coexist and one must eventually dominate. If they coexist (different parties holding each), the kernel exhibits genuine plural readings with no unified resolution. This determines whether the kernel is resolvable or permanently contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionist_vs_alternative_zionist_kernel_foreclosure, conceptual, 'Whether revisionist Zionism logically forecloses alternative Zionist readings or merely competes with them.').

omega_variable(
    settler_identity_fusion_mechanism,
    'Is the identity-locking of Jewish settlers to settlement presence a structural feature of the constraint itself, or a contingent political choice that could be altered through negotiation and compensation?',
    'Post-1948 cases where settlers were relocated or settlements were dismantled: what happened to settler identity and psychological integration? Do relocated settlers maintain ideological commitment to maximalism, or does identity unbind from settlement presence? Can settlement presence be separated from Jewish identity and peoplehood?',
    'If identity-fusion is structural and permanent, settlers cannot exit and the constraint is locked in by identity mechanisms. If identity-fusion is contingent, settlements could be renegotiated and the constraint could be transformed. This determines whether settler exit is possible or whether the beneficiary seat is permanently trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_identity_fusion_mechanism, empirical, 'Whether settler identity is structurally fused to settlement presence or contingently political.').

omega_variable(
    jewish_sovereignty_necessity_for_safety,
    'Is territorial maximalism and military enforcement necessary for Jewish safety, or are alternative arrangements (international guarantees, Palestinian peace, regional integration) sufficient?',
    'Comparative historical analysis of Jewish communities in diaspora and under various state arrangements; expert analysis from security scholars on defensive sufficiency of different territorial and political configurations; long-term historical trajectory of Jewish safety in different political contexts.',
    'If maximalism is necessary for safety, the constraint''s mechanism is justified by existential requirement. If alternatives are sufficient, maximalism is unnecessary extraction pursued for power rather than survival. This determines whether the snare''s beneficiaries are extracting for power or defending for survival.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jewish_sovereignty_necessity_for_safety, empirical, 'Whether territorial maximalism is necessary for Jewish safety or whether alternatives suffice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1880, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement_basis(jewi_tr_t1880, projected).
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement_basis(jewi_tr_t1900, observed).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t1920, observed).
narrative_ontology:measurement(jewi_tr_t1933, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t1933, observed).
narrative_ontology:measurement(jewi_tr_t1945, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1945, 0.19).
narrative_ontology:measurement_basis(jewi_tr_t1945, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement_basis(jewi_be_t1880, projected).
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement_basis(jewi_be_t1900, observed).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement_basis(jewi_be_t1920, observed).
narrative_ontology:measurement(jewi_be_t1933, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1933, 0.62).
narrative_ontology:measurement_basis(jewi_be_t1933, observed).
narrative_ontology:measurement(jewi_be_t1945, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1945, 0.76).
narrative_ontology:measurement_basis(jewi_be_t1945, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.88).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1880, 0.12).
narrative_ontology:measurement_basis(jewi_su_t1880, projected).
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1900, 0.24).
narrative_ontology:measurement_basis(jewi_su_t1900, observed).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1920, 0.38).
narrative_ontology:measurement_basis(jewi_su_t1920, observed).
narrative_ontology:measurement(jewi_su_t1933, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1933, 0.58).
narrative_ontology:measurement_basis(jewi_su_t1933, observed).
narrative_ontology:measurement(jewi_su_t1945, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1945, 0.74).
narrative_ontology:measurement_basis(jewi_su_t1945, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.91).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1880, tn=1948
narrative_ontology:measurement(jewi_grid_01, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(class), 1880, 0.15).
narrative_ontology:measurement(jewi_grid_02, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(class), 1948, 0.88).
narrative_ontology:measurement(jewi_grid_03, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(individual), 1880, 0.22).
narrative_ontology:measurement(jewi_grid_04, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(individual), 1948, 0.72).
narrative_ontology:measurement(jewi_grid_05, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(organizational), 1880, 0.18).
narrative_ontology:measurement(jewi_grid_06, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(organizational), 1948, 0.85).
narrative_ontology:measurement(jewi_grid_07, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(structural), 1880, 0.08).
narrative_ontology:measurement(jewi_grid_08, jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse(structural), 1948, 0.81).
narrative_ontology:measurement(jewi_grid_09, jewish_territorial_claim__revisionist_zionism_reading, resistance(class), 1880, 0.75).
narrative_ontology:measurement(jewi_grid_10, jewish_territorial_claim__revisionist_zionism_reading, resistance(class), 1948, 0.84).
narrative_ontology:measurement(jewi_grid_11, jewish_territorial_claim__revisionist_zionism_reading, resistance(individual), 1880, 0.68).
narrative_ontology:measurement(jewi_grid_12, jewish_territorial_claim__revisionist_zionism_reading, resistance(individual), 1948, 0.79).
narrative_ontology:measurement(jewi_grid_13, jewish_territorial_claim__revisionist_zionism_reading, resistance(organizational), 1880, 0.72).
narrative_ontology:measurement(jewi_grid_14, jewish_territorial_claim__revisionist_zionism_reading, resistance(organizational), 1948, 0.81).
narrative_ontology:measurement(jewi_grid_15, jewish_territorial_claim__revisionist_zionism_reading, resistance(structural), 1880, 0.62).
narrative_ontology:measurement(jewi_grid_16, jewish_territorial_claim__revisionist_zionism_reading, resistance(structural), 1948, 0.87).
narrative_ontology:measurement(jewi_grid_17, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(class), 1880, 0.12).
narrative_ontology:measurement(jewi_grid_18, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(class), 1948, 0.91).
narrative_ontology:measurement(jewi_grid_19, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(individual), 1880, 0.18).
narrative_ontology:measurement(jewi_grid_20, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(individual), 1948, 0.92).
narrative_ontology:measurement(jewi_grid_21, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(organizational), 1880, 0.15).
narrative_ontology:measurement(jewi_grid_22, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(organizational), 1948, 0.88).
narrative_ontology:measurement(jewi_grid_23, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(structural), 1880, 0.08).
narrative_ontology:measurement(jewi_grid_24, jewish_territorial_claim__revisionist_zionism_reading, stakes_inflation(structural), 1948, 0.85).
narrative_ontology:measurement(jewi_grid_25, jewish_territorial_claim__revisionist_zionism_reading, suppression(class), 1880, 0.11).
narrative_ontology:measurement(jewi_grid_26, jewish_territorial_claim__revisionist_zionism_reading, suppression(class), 1948, 0.92).
narrative_ontology:measurement(jewi_grid_27, jewish_territorial_claim__revisionist_zionism_reading, suppression(individual), 1880, 0.14).
narrative_ontology:measurement(jewi_grid_28, jewish_territorial_claim__revisionist_zionism_reading, suppression(individual), 1948, 0.89).
narrative_ontology:measurement(jewi_grid_29, jewish_territorial_claim__revisionist_zionism_reading, suppression(organizational), 1880, 0.12).
narrative_ontology:measurement(jewi_grid_30, jewish_territorial_claim__revisionist_zionism_reading, suppression(organizational), 1948, 0.93).
narrative_ontology:measurement(jewi_grid_31, jewish_territorial_claim__revisionist_zionism_reading, suppression(structural), 1880, 0.05).
narrative_ontology:measurement(jewi_grid_32, jewish_territorial_claim__revisionist_zionism_reading, suppression(structural), 1948, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__revisionist_zionism_reading, 0.18).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_national_identity_constraint).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, arab_state_regional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel 'jewish_territorial_claim'. The kernel encompasses competing answers to: What do Jews require for self-determination and safety in Palestine? What is the legitimate relationship between Jewish claim and Arab inhabitancy? How should Jewish statehood be pursued? The revisionist reading (this constraint) explicitly rejects Arab consent as prerequisite, asserts military force as primary mechanism, and claims maximalist territory as non-negotiable. Other readings (political Zionism, labor Zionism, cultural Zionism) answer these questions differently. All four readings must be linked via network.affects_constraints to document that they are alternative formulations of a single kernel, not independent constraints. The revisionist reading influences (but does not foreclose) the political Zionism reading by establishing the maximalist boundary as a starting negotiating position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
