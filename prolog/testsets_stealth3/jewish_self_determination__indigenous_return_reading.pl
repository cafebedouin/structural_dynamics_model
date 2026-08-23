% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous-Return Legitimation Frame (Decolonization Reading)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story authors the indigenous_return_reading of the
 *   jewish_self_determination kernel as a single epsilon-invariant
 *   constraint: the legitimating claim that the Jewish people are indigenous
 *   to the land with unbroken connection, and that Zionism is therefore
 *   decolonization rather than colonization, as that claim operates in the
 *   self-determination discourse. The claim's operation has two faces in one
 *   structure: it coordinates a religiously, ideologically, and
 *   geographically diverse Jewish constituency around a single
 *   non-theological, rights-based, postcolonially legible warrant for the
 *   national project, and the same structure subordinates the
 *   counter-people's continuous-presence claim, reframing it as later arrival
 *   or subordinate co-presence, and raises the cost of internal dissent. The
 *   claim's self-presentation is that of settled historical fact, a binary
 *   status that settles the legitimacy question; its actual operation is
 *   contested, actively enforced, and asymmetric. The epsilon referent is the
 *   standing contested discourse arrangement the story is about, not the
 *   frame's endorsed end-state of universal acceptance, at which epsilon
 *   would approach zero. By this reading's own lights the standing
 *   arrangement is substantially legitimate, so epsilon sits well below what
 *   an adversarial reading of the same referent would author, but the
 *   enforcement costs the frame imposes on contesting seats keep it well
 *   above coordination-floor levels. The claim/metric structure is deliberate
 *   and unreconciled: claimed_type records the structure believed true; the
 *   metrics record the operation believed descriptive.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: primary beneficiary seat (powerful/identity_locked) — the arrangement's legitimacy position is what the frame secures; also runs state-level enforcement
 *   - zionist_advocacy_institutions: agenda-setter (institutional/constrained) — administers and enforces the frame in discourse; collects relevance and resources
 *   - jewish_return_claimants: beneficiary (organized/identity_locked) — the frame is their claim's warrant
 *   - diaspora_jewish_communities: beneficiary (organized/identity_locked) — the frame supplies identification and standing without theological obligation
 *   - palestinian_indigeneity_claimants: primary target (organized/trapped) — their continuous-presence counter-claim is subordinated by the same structure
 *   - critical_scholars_of_zionism: target (moderate/constrained) — bear gatekeeping and delegitimation costs for the rival analytic
 *   - dissenting_jewish_intellectuals: target (moderate/identity_locked) — internal dissenters with no exit from peoplehood
 *   - mizrahi_jewish_refugee_communities: excluded seat (moderate/constrained) — invoked as evidence, rarely seated as autonomous voice
 *   - postcolonial_theory_field: analytical observer (institutional/analytical) — adjudicates indigeneity framings from outside the enforcement apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.62).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.58).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous-Return Legitimation Frame (Decolonization Reading)").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '0a491127-38bb-414e-97ad-c99c5313ad80').
narrative_ontology:cs_kernel_codification('0a491127-38bb-414e-97ad-c99c5313ad80', distributed).
narrative_ontology:cs_authority_grounding('0a491127-38bb-414e-97ad-c99c5313ad80', lineage).
narrative_ontology:cs_interpretation_layer_present('0a491127-38bb-414e-97ad-c99c5313ad80').
narrative_ontology:cs_reading_relation('0a491127-38bb-414e-97ad-c99c5313ad80', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('0a491127-38bb-414e-97ad-c99c5313ad80', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('0a491127-38bb-414e-97ad-c99c5313ad80', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a491127-38bb-414e-97ad-c99c5313ad80', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('0a491127-38bb-414e-97ad-c99c5313ad80', foundational, unbroken_connection_grounds_decolonization).
narrative_ontology:cs_axiom_status(unbroken_connection_grounds_decolonization, holdable).
narrative_ontology:cs_axiom_grounding('0a491127-38bb-414e-97ad-c99c5313ad80', unbroken_connection_grounds_decolonization, empirically_contingent).
narrative_ontology:cs_axiom('0a491127-38bb-414e-97ad-c99c5313ad80', foundational, indigenous_return_priority_over_later_arrival).
narrative_ontology:cs_axiom_status(indigenous_return_priority_over_later_arrival, holdable).
narrative_ontology:cs_axiom_grounding('0a491127-38bb-414e-97ad-c99c5313ad80', indigenous_return_priority_over_later_arrival, deontological).
narrative_ontology:cs_reference_frame('0a491127-38bb-414e-97ad-c99c5313ad80', continuous_indigenous_presence_baseline).
narrative_ontology:cs_drift_state('0a491127-38bb-414e-97ad-c99c5313ad80', contemporary_postcolonial_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0a491127-38bb-414e-97ad-c99c5313ad80', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, zionist_advocacy_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_return_claimants).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, palestinian_indigeneity_claimants).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, critical_scholars_of_zionism).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, dissenting_jewish_intellectuals).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, jewish_indigenous_status_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, unbroken_connection_thesis).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, decolonization_reclassification_of_zionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state's founding narrative and ongoing international standing rest substantially on the return frame: ministries, embassies, and public-diplomacy bodies present the state as the realization of an indigenous people's homecoming rather than a colonial implant. The frame converts into diplomatic legitimacy, legal defense in international fora, and domestic cohesion. The state also runs its own enforcement of the frame through diplomatic campaigns and legal advocacy. It cannot adopt a rival frame without dissolving its own legitimating narrative.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, israeli_state_institutions, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, israeli_state_institutions, agenda_setter).

% Advocacy organizations, federations, and communal defense bodies draft, disseminate, and enforce the frame: briefing materials, campus programming, anti-delegitimization campaigns, and discipline of communal deviation. Their budgets, staffing, and institutional relevance depend on the frame remaining the primary response to the colonialism charge. Pivoting to other framings would carry substantial organizational cost.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, zionist_advocacy_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, zionist_advocacy_institutions, beneficiary).

% Jews who ground their national claim in ancestral return: the frame is the warrant for their collective project, giving it a rights-based form legible to postcolonial norms. Their connection to the claim is constitutive of peoplehood; abandoning it would mean re-deriving the national claim from scratch under adversarial conditions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_return_claimants, beneficiary,
    organized, generational, identity_locked, global).

% Communities outside the land use the frame to hold identification with the collective project without theological obligation: it supplies a story of return that makes support feel like solidarity with an indigenous people rather than partisanship in a territorial dispute. Exit would mean renegotiating communal identity against strong internal pressure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% Palestinians who advance their own continuous-presence claim to the same land find that the frame pre-classifies their presence as later arrival or subordinate co-presence. They live under the arrangements the frame legitimates and cannot exit the discourse contest or the territory; their counter-evidence is heard only by conceding the frame's terms.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_indigeneity_claimants, payer,
    organized, generational, trapped, regional).

% Academics applying settler-colonial analytics to the case face funding pressure, journal and conference gatekeeping, public delegitimation campaigns, and in some jurisdictions institutional penalties. Their career investment is in the field they critique; relocation to adjacent fields is possible but costly.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, critical_scholars_of_zionism, payer,
    moderate, biographical, constrained, continental).

% Jewish writers and thinkers who contest the frame from inside the people: their dissent is read as betrayal, they face communal exclusion and charges of self-hatred, and their identity gives them no exit, since contesting the frame costs them standing in the community their identity binds them to.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, dissenting_jewish_intellectuals, payer,
    moderate, biographical, identity_locked, global).

% Jews displaced from Middle Eastern and North African countries are routinely invoked as evidence for the return frame, their expulsion cited as symmetry for the Palestinian displacement, but their autonomous experience, including differential treatment inside the state and the loss of diaspora heritage, is rarely seated in the venues where the frame is enforced. Their full testimony would complicate both the European-settler story and the simple-return story.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, mizrahi_jewish_refugee_communities, excluded,
    moderate, generational, constrained, regional).

% The scholarly field that owns the analytic vocabulary of indigeneity, settler colonialism, and decolonization adjudicates which cases fit which frame. It sits outside the enforcement apparatus, takes testimony from all seats, and its classifications feed back into the discourse contest through curricula, journals, and public intellectual life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, postcolonial_theory_field, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: it gives a religiously, ideologically, and geographically diverse Jewish population one non-theological, rights-based frame for the national claim, legible to postcolonial international norms, so that advocacy, communal solidarity, and diaspora identification can be coordinated without first resolving internal disagreements about theology or politics.
% TRANSFER_FUNCTION: Moves discourse-legitimacy and moral standing: from the rival people's counter-claimants and from scholars advancing the competing analytic, whose framings are subordinated or pre-delegitimized, toward the state, the advocacy apparatus, and the claimant constituency; it also moves communal standing, charging internal dissenters reputational costs.
% ABSENT_VOICES: Palestinian counter-claimants are present in the broad discourse but structurally subordinated inside the venues where the frame is enforced, heard only when conceding its terms. Mizrahi Jewish communities are conscripted as evidence but rarely seated as autonomous voices. Palestinian citizens of the state and refugee-diaspora Palestinians have the least access to enforcement venues; their objection would be that the frame settles by assertion what their displacement makes the central question.
% DISAPPEARANCE_RATIONALE: If the frame vanished overnight, the self-determination claim would lose its primary postcolonially legible warrant: advocacy would reorganize around the equal-rights or covenant framings, communal boundaries would shift as the frame's enforcement lost its object, and the discourse contest would restructure around whichever replacement the advocacy apparatus adopted. The arrangement's legitimacy defense is organized around this frame; its removal forces rearrangement.
% FOUNDING_PROBLEM: After 1967, and decisively from the 1990s onward, the Jewish national project faced a delegitimation problem: postcolonial norms made colonialism the master illegitimacy charge, and the project was increasingly read as a European colonial implant. The frame was built to answer that charge by reclassifying the project as decolonization, the return of an indigenous people, thereby converting the era's strongest legitimacy vocabulary from adversary to warrant.
% FOUNDING_PROBLEM_CORROBORATION: The adversarial scholarship itself corroborates the founding problem: postcolonial and Palestinian scholars document and keep live the delegitimation pressure the frame was built to answer, and diplomatic historiography written outside the advocacy apparatus attests that the frame's modern form was constructed as a deliberate response to the colonialism charge. Corroboration does not come from the beneficiary set alone; the frame's own adversaries are the primary external attestors that the problem it answers is live.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the frame transfers discourse-legitimacy asymmetrically; it asserts settlement of the legitimacy question rather than submitting it to adjudication, and its enforcement imposes real costs on the counter-people's claimants, on scholars running the rival analytic, and on internal dissenters. It stays short of ceiling because the frame also performs genuine coordination for a constituency with a documented history of dispossession, and because part of its warrant (continuous communal presence in the land's cities, liturgical and practical orientation toward the land across the diaspora arc) is evidentially robust. Suppression 0.58, authored as raw unscaled structure: substantial but partial; the rival framing circulates freely in global academia and activism, and suppression operates through funding gatekeeping, venue access, delegitimation charges, and in some jurisdictions state-level adoption, not through total exclusion. Theater_ratio 0.31: the coordination function is real, but a growing share of frame activity is performative, ceremonial invocation, selective archaeology, anniversary choreography aimed at audiences already convinced. Accessibility_collapse 0.35: understanding the frame does not collapse alternatives; the rival readings remain fully accessible and widely held, so the frame competes rather than forecloses. Resistance 0.70: the frame meets an institutionalized adversarial scholarship and movement, which is why its enforcement requirement has risen across the interval. Measurements run on one shared grid (points 0/7/14/21/28/35 of an interval mapping to approximately 1990-2025, one unit per year) with all three tracked metrics authored at every point; the window opens as the settler-colonial analytic gains academic ground and the frame's counter-delegitimation machinery builds in response, so extraction, suppression, and performative share all rise monotonically.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting and beneficiary seats the frame is truth-telling and rights-assertion: it states a historical fact and draws the ordinary conclusion, and its enforcement is defense of a people's return against a false charge. From the payer seats the same structure is a closure device: the counter-claim is pre-classified as later arrival before it is heard, the rival analytic is pre-delegitimized as hostility rather than scholarship, and internal dissent is pre-read as betrayal. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them, and the divergence is precisely the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real structural positions: the state collects the frame's legitimacy yield in diplomatic standing, legal defense, and domestic cohesion; the advocacy apparatus collects relevance and resources for running enforcement; return claimants and diaspora communities collect a usable warrant for identification and support. All four sit near the beneficiary end of directionality, and their identity-locked exit stabilizes them there, since none can re-frame without dissolving constitutive commitments. The victim declarations: Palestinian counter-claimants bear the frame's subordination of their continuous-presence claim and cannot exit the discourse or the territory it governs (trapped, near the full-target end); critical scholars bear gatekeeping and delegitimation (constrained); internal Jewish dissenters bear communal exclusion while lacking any exit from peoplehood itself (identity_locked, the highest effective extraction among the dissenting seats). Mizrahi refugee communities are excluded rather than seated: their experience is conscripted as evidence, which the structural derivation does not read as benefit. No directionality overrides were needed; the beneficiary/victim declarations plus exit options produce accurate directionalities for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, securing the national project's legitimacy under postcolonial norms that render colonialism the master illegitimacy charge, is live, so no mandatrophy resolution is declared. The tangled classification prevents two opposite mislabels: reading the frame as pure coordination would miss the asymmetric subordination riding the same structure (the coordination story is partly cover); reading it as pure extraction would miss the genuine collective-action function for a diverse, historically persecuted constituency whose internal disagreements the frame lets them bracket. The classification holds both faces in one structure and forces the analysis to price them together. The trajectory risk is atrophy: if the delegitimation contest ever ends, by victory, exhaustion, or settlement, the frame's enforcement machinery would outlive its function and drift toward ceremonial maintenance; the measurement series tracks the early slope of that drift in the rising performative share.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binary_vs_co_indigeneity,
    'Is indigeneity in the contested land a binary status, one people indigenous and the other later-arriving, or can two peoples hold co-indigenous standing with layered claims?',
    'Comparative legal anthropology of multi-peoples homelands, combined with a historical-demographic assessment of continuous presence for both peoples in the contested territory.',
    'If co-indigenous standing is sustained, the frame''s subordination of the rival people''s counter-claim loses its factual footing and the arrangement''s extraction rises sharply toward the adversarial reading''s authoring; if binary status holds, the coordination function dominates and measured extraction falls toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_vs_co_indigeneity, empirical, 'Whether the claim''s factual premise of singular indigeneity survives comparative and demographic scrutiny.').

omega_variable(
    unbroken_connection_facticity,
    'Is unbroken connection historically robust across the full arc, continuous presence together with continuous orientation of practice, liturgy, and longing toward the land, or does the record show ruptures that the word unbroken papers over?',
    'Historiographic audit distinguishing three separable facts: continuous communal presence in the land''s cities, continuous sovereignty (largely absent after antiquity), and continuous diasporic orientation toward the land; assess whether unbroken is accurate for the composite or only for a component.',
    'If unbroken is rhetorical, the frame''s self-presentation as settled fact diverges from its evidentiary base, raising performative maintenance and weakening the foundational axiom; if robust, the mountain-like presentation of the premise is defensible even though the constraint built on it remains constructed and contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbroken_connection_facticity, empirical, 'Facticity of the unbroken-connection premise underlying the frame.').

omega_variable(
    kernel_reading_contest,
    'This constraint is the indigenous_return_reading of the jewish_self_determination kernel; the settler_colonial sibling re-authors the same referent with near-ceiling extraction, treating dispossession as constitutive of the arrangement rather than incidental, which flips the family''s classification toward pure extraction. Where exactly is the disagreement located?',
    'The disagreement is located in whether the 1948 displacements and the post-1967 occupation regime are constitutive of the standing arrangement or incidental to it; archival and demographic adjudication of displacement scale and of the legal-exclusion architecture would move the dispute.',
    'On the settler sibling''s reading the same referent computes as a snare; on this reading it computes as a tangled hybrid. The corpus needs both files to measure the divergence; neither file should hedge across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one kernel, opposed readings, disagreement located at constitutive-versus-incidental treatment of displacement and occupation.').

omega_variable(
    identity_coordination_cover_risk,
    'Is the frame''s coordination function genuine, solving a real collective-action problem for a religiously and geographically diverse people, or is identity coordination framing covering extraction whose gains concentrate in the state and advocacy apparatus while costs fall on the rival people and internal dissenters?',
    'Trace enforcement-resource flows, who funds the frame''s defense, who staffs it, who is disciplined under it, and compare the concentration of those flows against the diffuseness of the declared beneficiary base.',
    'If the identity framing is cover, the identity-coordination complexity leeway is being gamed and the classification shifts toward pure extraction despite the declared coordination type; if the coordination is genuine, part of measured extraction is absorbed by the type''s coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coordination_cover_risk, conceptual, 'Whether identity coordination is the frame''s function or its cover story.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of the rival framing structural, institutional penalties, state-level adoption, funding gatekeeping, or internalized, communal members self-censoring because the frame has fused with peoplehood itself?',
    'Post-exit suppression trajectory: track dissenters who leave communal institutions; if their dissent persists undamped after exit, the suppression was structural, if it attenuates or inverts, part of it was internalized.',
    'If internalized, the frame''s effective suppression exceeds the structural measure and travels with agents after institutional exit, raising effective costs on the identity-locked dissenting seat; if structural, remedies that change institutions suffice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of the rival reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_indigenous_return_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(jsd_indigenous_return_tr_t0, observed).
narrative_ontology:measurement(jsd_indigenous_return_tr_t7, jewish_self_determination__indigenous_return_reading, theater_ratio, 7, 0.25).
narrative_ontology:measurement_basis(jsd_indigenous_return_tr_t7, observed).
narrative_ontology:measurement(jsd_indigenous_return_tr_t14, jewish_self_determination__indigenous_return_reading, theater_ratio, 14, 0.27).
narrative_ontology:measurement_basis(jsd_indigenous_return_tr_t14, observed).
narrative_ontology:measurement(jsd_indigenous_return_tr_t21, jewish_self_determination__indigenous_return_reading, theater_ratio, 21, 0.29).
narrative_ontology:measurement_basis(jsd_indigenous_return_tr_t21, observed).
narrative_ontology:measurement(jsd_indigenous_return_tr_t28, jewish_self_determination__indigenous_return_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement_basis(jsd_indigenous_return_tr_t28, observed).
narrative_ontology:measurement(jsd_indigenous_return_tr_t35, jewish_self_determination__indigenous_return_reading, theater_ratio, 35, 0.31).
narrative_ontology:measurement_basis(jsd_indigenous_return_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(jsd_indigenous_return_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(jsd_indigenous_return_be_t0, observed).
narrative_ontology:measurement(jsd_indigenous_return_be_t7, jewish_self_determination__indigenous_return_reading, base_extractiveness, 7, 0.5).
narrative_ontology:measurement_basis(jsd_indigenous_return_be_t7, observed).
narrative_ontology:measurement(jsd_indigenous_return_be_t14, jewish_self_determination__indigenous_return_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement_basis(jsd_indigenous_return_be_t14, observed).
narrative_ontology:measurement(jsd_indigenous_return_be_t21, jewish_self_determination__indigenous_return_reading, base_extractiveness, 21, 0.58).
narrative_ontology:measurement_basis(jsd_indigenous_return_be_t21, observed).
narrative_ontology:measurement(jsd_indigenous_return_be_t28, jewish_self_determination__indigenous_return_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement_basis(jsd_indigenous_return_be_t28, observed).
narrative_ontology:measurement(jsd_indigenous_return_be_t35, jewish_self_determination__indigenous_return_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(jsd_indigenous_return_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(jsd_indigenous_return_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(jsd_indigenous_return_su_t0, observed).
narrative_ontology:measurement(jsd_indigenous_return_su_t7, jewish_self_determination__indigenous_return_reading, suppression_requirement, 7, 0.47).
narrative_ontology:measurement_basis(jsd_indigenous_return_su_t7, observed).
narrative_ontology:measurement(jsd_indigenous_return_su_t14, jewish_self_determination__indigenous_return_reading, suppression_requirement, 14, 0.51).
narrative_ontology:measurement_basis(jsd_indigenous_return_su_t14, observed).
narrative_ontology:measurement(jsd_indigenous_return_su_t21, jewish_self_determination__indigenous_return_reading, suppression_requirement, 21, 0.54).
narrative_ontology:measurement_basis(jsd_indigenous_return_su_t21, observed).
narrative_ontology:measurement(jsd_indigenous_return_su_t28, jewish_self_determination__indigenous_return_reading, suppression_requirement, 28, 0.56).
narrative_ontology:measurement_basis(jsd_indigenous_return_su_t28, observed).
narrative_ontology:measurement(jsd_indigenous_return_su_t35, jewish_self_determination__indigenous_return_reading, suppression_requirement, 35, 0.58).
narrative_ontology:measurement_basis(jsd_indigenous_return_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel decomposes into five sibling constraint stories, one per reading, rather than one story with a measurement parameter: the readings author different epsilon over a shared referent (the standing contested discourse arrangement), and folding them into one file would violate epsilon-invariance. This file is the indigenous_return_reading and links to all four siblings. Within the family, the liberal_nationalist_reading is upstream (the baseline equal-rights claim that the other pro-sovereignty readings strengthen or replace), and the settler_colonial_reading is the adversarial pole that this reading's enforcement machinery exists to counter; the corpus measures the epsilon divergence across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
