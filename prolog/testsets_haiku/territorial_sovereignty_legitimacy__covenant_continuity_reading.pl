% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty via Covenant Continuity and International Recognition
 *   domain: political/territorial
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'territorial sovereignty legitimacy' — specifically, the covenant
 *   continuity reading, which grounds Israeli sovereignty claims in (1)
 *   ancient divine covenant to the Jewish people, (2) continuity of Jewish
 *   presence despite 2000-year gaps and demographic absence, and (3) modern
 *   international recognition instruments (Balfour Declaration 1917, UN
 *   Partition Plan 1947, 1948 statehood). The reading frames the 1948
 *   establishment and subsequent territorial control as restoration of a
 *   dormant but indelible claim rather than as creation of a new state or
 *   resolution of a competing self-determination dispute. The two sibling
 *   readings (existential_matrix_reading and self_determination_reading)
 *   instantiate fundamentally different legitimacy architectures from the
 *   same kernel of territorial authority in the post-Ottoman Levant. This
 *   constraint is a kernel reading: it declares ONE legitimacy claim as the
 *   referent; alternative readings' endorsed alternatives are NOT this
 *   constraint's referent and are NOT measured here. The ε (extractiveness)
 *   is authored for the covenant-continuity arrangement as the reading itself
 *   assesses it — high extraction because the framing subordinates
 *   Palestinian self-determination and modern residence claims to ancient
 *   covenant claims, creating asymmetric authority relationships that favor
 *   Jewish sovereignty claimants. The reading's own beneficiaries (diaspora,
 *   state apparatus, theology schools) experience the constraint differently
 *   from its payers (Palestinian Arabs, displaced communities); the engine
 *   computes that perspectival divergence from the structural data. The
 *   theater_ratio rises over the interval because the coordination function
 *   (resolving post-Ottoman territorial ambiguity) weakened while the
 *   enforcement machinery (military occupation, settlement administration,
 *   narrative defense) intensified — extractiveness increasingly dominates
 *   the constraint's operation.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_claimants: organized beneficiaries claiming ancient covenant right to return; identity-locked to the claim
 *   - israeli_state_apparatus: institutional agenda-setter and enforcer; collected sovereignty from the constraint
 *   - palestinian_arabs: moderate-power payers bearing occupation, displacement, constrained self-governance
 *   - displaced_communities: powerless payers; trapped in stateless diaspora by the constraint's operation
 *   - covenant_doctrine_adherents: organized beneficiaries (religious Zionists, settler theology); identity-locked through theological worldview
 *   - international_recognition_bodies: institutional beneficiary-agenda-setters validating the claim through diplomatic acts
 *   - competing_territorial_claimants: excluded actors (Palestinian national movement, Arab states, self-determination advocates) whose claims cannot coexist with covenant primacy
 *   - historical_scholarship_communities: analytical observers whose epistemic authority is contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.72).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.87).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty via Covenant Continuity and International Recognition").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political/territorial").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '5bfab325-2adb-4dbd-ac94-64334b46f34d').
narrative_ontology:cs_kernel_codification('5bfab325-2adb-4dbd-ac94-64334b46f34d', fixed_text).
narrative_ontology:cs_authority_grounding('5bfab325-2adb-4dbd-ac94-64334b46f34d', lineage).
narrative_ontology:cs_interpretation_layer_present('5bfab325-2adb-4dbd-ac94-64334b46f34d').
narrative_ontology:cs_reading_relation('5bfab325-2adb-4dbd-ac94-64334b46f34d', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_reading_relation('5bfab325-2adb-4dbd-ac94-64334b46f34d', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('5bfab325-2adb-4dbd-ac94-64334b46f34d', foundational, ancient_covenant_permanent_sovereignty).
narrative_ontology:cs_axiom_status(ancient_covenant_permanent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5bfab325-2adb-4dbd-ac94-64334b46f34d', ancient_covenant_permanent_sovereignty, deontological).
narrative_ontology:cs_axiom('5bfab325-2adb-4dbd-ac94-64334b46f34d', foundational, continuity_across_exile_maintains_claim).
narrative_ontology:cs_axiom_status(continuity_across_exile_maintains_claim, holdable).
narrative_ontology:cs_axiom_grounding('5bfab325-2adb-4dbd-ac94-64334b46f34d', continuity_across_exile_maintains_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('5bfab325-2adb-4dbd-ac94-64334b46f34d', covenant_grounded_ancient_right).
narrative_ontology:cs_drift_state('5bfab325-2adb-4dbd-ac94-64334b46f34d', contemporary_post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5bfab325-2adb-4dbd-ac94-64334b46f34d', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora_claimants).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, covenant_doctrine_adherents).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arabs).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, displaced_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diaspora communities and their institutional representatives (WZO, Jewish Agency, world Jewish organizations) claim historical-legal right to territorial return based on covenant lineage. Benefit from the legitimacy framework that treats ancient presence as creating indelible sovereignty claims. Identity fusion with claims of ancestral connection makes exit from this framing existentially difficult — abandoning the claim means renegotiating Jewish peoplehood identity.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora_claimants, beneficiary,
    organized, generational, identity_locked, global).

% The state institutions (Knesset, executive, military, settlement authorities) enforce the territorial claim and defend it via international diplomacy, legal argumentation, and military capacity. Set and enforce the rules of occupation, settlement expansion, and territorial administration. Directly collect authority and sovereignty from the territory. Trapped in the claim because state legitimacy is constitutively tied to it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, trapped, regional).

% Bear territorial displacement, occupation administration, and restrictions on self-governance and movement. The covenant continuity framing renders their modern residence and self-determination secondary to ancient covenant claims. Exit options are bounded by military occupation, legal restrictions, and regional geopolitics — cannot simply leave the territory and maintain collective identity.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arabs, payer,
    moderate, generational, constrained, regional).

% Palestinian refugees and their descendants (estimated 5-7 million) live in diaspora without right of return under international law as applied by the Israeli state. Bear the cost of territorial loss and statelessness. Trapped by legal barriers and lack of political power to enforce claims; diaspora status prevents territorial exit and collective return.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, displaced_communities, payer,
    powerless, biographical, trapped, global).

% Religious, theological, and ideological communities (settler movements, religious Zionists, Christian Zionists, covenant theology schools) whose worldview and institutional identity are constituted by the belief that the territory is divinely covenanted. Benefit from the constraint's legitimation of their theological claims. Identity-locked through religious worldview that makes covenant denial unthinkable without existential rupture.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, covenant_doctrine_adherents, beneficiary,
    organized, civilizational, identity_locked, global).

% UN institutions, major states recognizing Israel, and diplomatic systems treat the Israeli state as legitimate based partly on the historical-legal framing of covenant continuity combined with Balfour Declaration and Partition Plan. Collateral benefit from the stability and legal precedent the framing provides. Constrained by diplomatic consequences and institutional inertia in revising recognition once granted.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, beneficiary,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, agenda_setter).

% Palestinian national movement, Arab states, and international self-determination advocates are structurally excluded from the covenant continuity framing — their claims rest on different temporal baselines (modern continuous residence, self-determination principle) and cannot coexist with covenant primacy in the same legitimacy architecture. Constrained by the international legal framework that has already granted recognition to Israel.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, competing_territorial_claimants, excluded,
    moderate, generational, constrained, regional).

% Archaeologists, historians, biblical scholars, and demographers examine evidence of historical presence, continuity of occupation, and the historical accuracy of covenant narratives. Produce technical assessments independent of the legitimacy claim but whose findings feed into or challenge the framework. Neither benefiting nor bearing costs directly, but their epistemic authority is contested.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, historical_scholarship_communities, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the ambiguity of territorial authority in the aftermath of Ottoman collapse and European decolonization: establishes a coherent historical-legal narrative that designates a specific people (Jews) as having pre-existing sovereignty claims grounded in ancient covenant, surviving demographic absence, and reactivated through modern international recognition instruments (Balfour, Partition, 1948). Coordinates competing legitimacy claims by temporal prioritization (ancient covenant beats modern residence) and legal instruments (international recognition validates dormant claims).
% TRANSFER_FUNCTION: Transfers territorial control, political authority, and administrative power from Ottoman/British/Arab control to a Jewish-majority state, and transfers residential rights, movement rights, and self-governance capacity from Palestinian Arab populations to Israeli state administration. Moves narrative authority about historical justification from post-colonial arbitrariness to deep-historical legitimacy.
% ABSENT_VOICES: Palestinian Arab self-determination advocates, international law scholars who prioritize modern continuous residence over ancient covenant, Arab states whose interests were affected by partition, contemporary inhabitants whose consent was not solicited for the territorial rearrangement, and historical scholarship communities skeptical of covenant continuity narratives are structurally excluded from the legitimacy framework. Their objections are treated as external to the covenant logic rather than as competing legitimate readings of the same historical record.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity framing disappeared, the territorial claim would have to rest on alternative legitimacy bases (modern self-determination, UN Partition as creative act rather than restoration, historical accident of major-power support). Without the covenant framing, Israeli state legitimacy would shift from restoration to construction, radically altering the political space for negotiation, settlement, and territorial claims. Palestinian claims would not disappear but would gain legitimacy parity rather than subordination.
% FOUNDING_PROBLEM: After the collapse of Ottoman authority and the emergence of Jewish nationalism and Arab nationalism in the same territory, a legitimacy claim was needed to justify why Jewish sovereignty should supersede Arab self-determination claims in the same land. The covenant continuity reading provides that claim: ancient covenant creates a sovereign right that survives demographic absence and exile, making the modern state a restoration rather than a colonial implant or novel creation.
% FOUNDING_PROBLEM_CORROBORATION: Israeli state institutions, covenant theology schools, and organized diaspora communities attest the founding problem remains live — Jewish sovereignty requires deep historical legitimation to resist the demographic and modern-legal challenges to it. Palestinian national movement, international law scholars, and Arab state representatives attest the founding problem is a false framing designed to obscure the contingency of British-supported colonialism and that the actual problem was resolved by Arab-Jewish coexistence or two-state partition on modern self-determination grounds. Historical scholarship (particularly since 1980s) documents disputed evidence of ancient Jewish presence and contested interpretations of 'continuity' across 2000-year gaps — external corroboration leans toward skepticism of the continuity narrative but does not rule out covenant-theology adherents' interpretive claims.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high and rising because the covenant continuity framing subordinates competing legitimacy claims and renders Palestinian self-determination claims secondary to ancient covenant restoration. The constraint is explicitly asymmetric: one people's ancient presence claim overrides another people's modern continuous-residence claim in the same territory. Suppression (0.72) is high because maintaining the covenant-continuity reading as the sole legitimate framework requires continuous enforcement against competing narratives (historical scholarship skepticism, Palestinian counter-claims, international law scholars arguing for self-determination primacy). Theater ratio (0.58) is substantial and rising because the coordination function — resolving post-Ottoman territorial ambiguity — was genuinely contested and unsettled in 1917-1948 but is now largely ossified; the state apparatus now performs the legitimacy claim more than it produces coordination outcomes. The measurement series on a shared time grid (1880, 1917, 1948, 1967, 1995, 2024) track the constraint's lifecycle: low extraction pre-Balfour (the claim existed but lacked state enforcement), rapid rise through 1948 statehood (the claim was operationalized into territorial control), plateau-and-slight-rise post-1967 (occupation mechanics crystallized; theater increased as alternatives collapsed). The early values (1880, 1917) are projected because quantitative measurement is anachronistic before the state existed; the rest are observed from documented occupation policy, settlement administration, and demographic records.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is stark and structural. From the Israeli state-apparatus and diaspora-claimant seats, the constraint is experienced as (1) recovery of an ancient claim, (2) legitimate exercise of a dormant right, (3) international recognition of a pre-existing sovereignty. From the Palestinian-Arab and displaced-community seats, the same constraint is experienced as (1) displacement and subordination, (2) denial of modern self-determination rights, (3) enforcement of an external territorial claim against internal residents. Neither perspective is authored here as 'correct' — both are structural facts about how the constraint operates from different power positions. The engine computes the classification from each seat's power atoms, exit options, and role; the divergence IS what the per-seat computation is designed to capture. Covenant theology adherents and Israeli state apparatus experience the constraint as a coordination solution to a genuine problem (post-Ottoman authority vacuum, Jewish national need for collective self-determination). Palestinian Arabs and international self-determination advocates experience it as pure extraction: a framework constructed to justify dispossession and territorial subordination using historical narrative as cover. This is not a failure of the framework — it is the framework working as designed: the reading privileges covenant continuity over modern residence, so those whose claim rests on modern residence necessarily experience the constraint as extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) ranges from 0.0 (full beneficiary) to 1.0 (full target). Israeli state apparatus: d ≈ 0.15 (institutional beneficiary; collects sovereignty and authority directly; exit is identity-fused and state-structural, not a real option). Jewish diaspora claimants: d ≈ 0.20 (organized beneficiary; gain legitimacy for return claims; identity-locked exit makes them appear more trapped than they are, but the constraint directly benefits them). Covenant theology adherents: d ≈ 0.18 (organized beneficiary; worldview vindicated; identity-locked). Palestinian Arabs: d ≈ 0.82 (target; bear occupation, constrained self-governance, demographic subordination; moderate power but trapped by occupation mechanics; constrained exit). Displaced communities: d ≈ 0.95 (full target; powerless, trapped, no geographic or political exit; bear the cost of statelessness). International recognition bodies: d ≈ 0.25 (institutional beneficiary; collateral benefit from stability of recognition once granted; diplomatic path-dependency constrains exit). These values derive from the beneficiary/victim declarations plus the power, exit, and time-horizon data authored above. The engine computes d automatically; no override is needed because the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — the decay of the founding problem — is evident in the theater-ratio rise and the gap between founding-problem-status='contested' and disappearance-verdict='world_rearranges'. The founding problem was to resolve post-Ottoman territorial authority ambiguity when both Jewish nationalism and Arab nationalism claimed the same territory. That problem is solved in the sense that a state now exists and operates with (disputed but widely recognized) territorial authority. However, the constraint persists and intensifies because: (1) the payer populations never consented and have not lost their own self-determination claims, (2) the coordination that the constraint originally provided (resolving ambiguity about WHO has authority) is now ossified into straightforward dominance (Israeli state has authority and enforces it), (3) the constraint's legitimacy increasingly depends on defending the covenant-narrative against historical scholarship skepticism and on enforcing suppression against competing claims rather than on coordinating a genuine collective problem. The mandatrophy is NOT complete because the constraint remains actively defended and enforced — it is not yet degraded to piton status. But the theater ratio rising toward 0.6 signals that the lived function is shifting from coordination to enforcement-of-narrative. The six-questions data supports this: founding-problem-status='contested' (the parties dispute whether the original problem still exists) and the disappearance verdict 'world_rearranges' (the constraint's removal would not restore pre-1880 Ottoman authority but would instead trigger territorial renegotiation). This is a Tangled Rope displaying early mandatrophy symptoms — genuine coordination function is present but increasingly eclipsed by extractive enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_continuity_historical_evidence,
    'Does historical and archaeological evidence support the continuity narrative — that Jewish presence in the territory, though demographically thin during the exile period (diaspora era), never entirely ceased, and can be characterized as ''continuous'' in a meaningful sense?',
    'Archaeological surveys, historical demography, and scholarly consensus on settlement patterns in the Levant during the 2000-year period between Roman expulsion and modern Jewish migration. Competing scholarly schools (Israeli archaeology, Palestinian scholarship, international academic consensus) produce different assessments.',
    'If continuity cannot be established, the covenant claim rests on ancient title alone, weakening it against modern residence and self-determination claims. If continuity can be established even thinly, it supports the restoration narrative. The dispute is not about whether continuous presence occurred but about whether sparse, discontinuous presence across centuries counts as ''continuity'' sufficient to maintain a sovereignty claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenant_continuity_historical_evidence, empirical, 'Whether historical evidence supports the ''continuity'' assertion central to the covenant-continuity reading').

omega_variable(
    covenant_divine_vs_constructed,
    'Is the covenant claim a description of historical fact (a divine promise was made and transmitted) or a constructed legitimacy narrative (the covenant narrative was assembled from texts and tradition to serve modern political needs)?',
    'This is not empirically resolvable in the way historical claims are. Theological and textual scholarship can establish how the covenant narrative was transmitted and interpreted; it cannot establish whether the covenant was divinely made or constructed. Different epistemologies (faith-based, secular scholarship, traditional interpretation) produce different verdicts.',
    'If the covenant is understood as a constructed narrative, the legitimacy claim becomes dependent on the reading''s appeal to that narrative — it operates as a normative claim about which people should control the territory (because they have covenant claims) rather than as a natural fact. If understood as divine fact, it has the force of natural law within the reading''s own framework. The classification does not change, but the mechanism of legitimation shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_divine_vs_constructed, conceptual, 'Whether the covenant claim is factual or constructed — a framing issue, not a measurement issue').

omega_variable(
    suppression_structural_vs_internalized,
    'To what extent is the measured suppression (0.72) structural (military occupation, legal restrictions, demographic policies) versus internalized (Palestinian Arab acceptance of subordination, internalization of covenant-narrative framing as legitimate)?',
    'Post-occupation trajectory analysis: if Palestinian communities reproduce suppression-patterns after occupation ends (e.g., in diaspora, in Palestinian Authority-controlled areas), the suppression is partially internalized; if suppression patterns decay rapidly after enforcement is removed, the suppression is primarily structural.',
    'If suppression is primarily structural, removing enforcement machinery would reduce it substantially. If internalized, the constraint persists via normative acceptance and cultural reproduction even after enforcement is removed. The classification remains Tangled Rope either way, but the fixing cost (receipt surface) and the exit-options atom (stakeholder data) might be revised upward if internalization is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of suppression: structural enforcement versus cultural internalization').

omega_variable(
    kernel_reading_foreclosure_vs_coexistence,
    'Does the covenant-continuity reading logically foreclose the self-determination reading, or can both readings coexist as alternative legitimacy frameworks held by different parties?',
    'Structural analysis of the core premises: covenant-continuity asserts that ancient covenant claims have permanent priority; self-determination asserts that modern residence and nationalist mobilization ground sovereign claims. If a single framework can hold both (e.g., by treating the covenant as a claim within a broader self-determination framework), they coexist. If covenant priority logically demands subordination of modern self-determination, covenant-continuity forecloses self-determination.',
    'If forecloses: the three readings partition the logical space and only one can be true. If coexists: the readings represent genuinely live alternatives that different communities hold simultaneously, and the dispute is political, not logical. This affects how the engine computes cross-reading coupling and contamination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_vs_coexistence, conceptual, 'Whether the covenant-continuity and self-determination readings are logically compatible or mutually exclusive').

omega_variable(
    international_recognition_binding_precedent,
    'To what extent does the fact of international recognition of the Israeli state constitute a binding legitimacy precedent that makes the covenant-continuity framing settled law, versus remaining an instrument that can be revisited or revoked by changed international consensus?',
    'International law doctrine, state practice regarding recognition, and precedent in other post-colonial territorial disputes. If recognition is generally treated as irreversible once granted (binding), the covenant-continuity reading gains force from the accumulated institutional precedent. If recognition is treated as contingent on ongoing legitimacy, the covenant-continuity reading remains contestable despite 75+ years of statehood.',
    'If binding precedent: the constraint''s persistence is structurally stabilized by institutional inertia, and changing it requires extraordinary consensus change. If contingent: the constraint remains actively contested and vulnerable to shifts in international consensus. The theater_ratio interpretation shifts: high theater might indicate brittle performance rather than stable inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_binding_precedent, conceptual, 'Whether international recognition is binding precedent or contingent legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1880, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1880, 0.2).
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.28).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.38).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.48).
narrative_ontology:measurement(terr_tr_t1995, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1995, 0.55).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(terr_be_t1880, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.32).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.58).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.64).
narrative_ontology:measurement(terr_be_t1995, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1880, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1880, 0.25).
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.42).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.68).
narrative_ontology:measurement(terr_su_t1995, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1995, 0.71).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_right_of_return).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, settlement_expansion_legitimacy).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, occupation_administrative_authority).

% DUAL FORMULATION NOTE:
% The territorial_sovereignty_legitimacy kernel decomposes into three distinct constraint stories, each instantiating a different legitimacy reading applied to post-Ottoman Levantine territorial authority. (1) covenant_continuity_reading (this story): ancient divine covenant + continuous Jewish presence + modern recognition. (2) self_determination_reading: modern self-determination principle applied to Arab demographic majority and continuous residence. (3) existential_matrix_reading: territorial control as precondition for collective survival. The three stories share the same kernel (territorial authority ambiguity) but apply irreconcilable legitimacy architectures, producing different ε values, different beneficiary/victim structures, and different per-seat classifications. Each reading's endorsed alternative (the legitimacy framework it proposes) is NOT the referent for its own ε — the referent is the standing arrangement under contest (Israeli sovereignty as currently constituted). Values remain reading-indexed: a welfarist reading and a justice-based reading of the same standing arrangement would author different ε values but share the referent. Sibling readings are linked via network.affects_constraints so the corpus tracks which constraints structurally influence each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
