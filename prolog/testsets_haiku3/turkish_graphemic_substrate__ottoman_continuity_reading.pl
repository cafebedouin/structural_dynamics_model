% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__ottoman_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: turkish_graphemic_substrate__ottoman_continuity_reading
 *   human_readable: Ottoman Continuity Reading: Turkish Identity and Arabic Graphemic Substrate
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel of Turkish
 *   linguistic and cultural identity in the aftermath of Ottoman collapse.
 *   The Ottoman continuity reading asserts that Turkish national identity is
 *   and must remain continuous with Ottoman-Islamic civilization, with Arabic
 *   script as the legitimate graphemic substrate encoding that continuity.
 *   The reading is upheld by the Islamic religious establishment (whose
 *   institutional authority depends on Arabic-script mastery and Ottoman
 *   precedent), parts of the state apparatus committed to Islamic legitimacy,
 *   and conservative intellectuals. It opposes and is opposed by the secular
 *   nationalist reading (Turkish identity is distinct from Ottoman-Islamic
 *   past; Latin script is the modern, European-aligned path) and the gradual
 *   transition reading (both scripts can coexist during a managed
 *   transition). The constraint's persistence depends on active enforcement:
 *   maintaining Arabic-script curricula, suppressing alternative scripts, and
 *   defining legitimate Turkish identity through the continuity frame. The
 *   claim/metric gap is deliberate and analytically significant: the reading
 *   CLAIMS this is a rope (genuine coordination of inherited knowledge and
 *   religious authority) while the authored metrics describe substantially
 *   extractive, enforcement-dependent operation—the engine measures this gap
 *   to detect whether the reading is descriptively accurate or covers rents.
 *
 * KEY AGENTS:
 *   - Ottoman literate religious establishment (Islamic scholars, mullah networks, madrasas) — agenda-setter, institutional power, identity-locked; enforces the constraint by maintaining Arabic-script pedagogy
 *   - Islamic institutional authority (pan-Islamic legitimacy framework, religious law apparatus) — beneficiary, institutional, identity-locked; benefits from the constraint by remaining the sole authoritative interpreter of Turkish identity
 *   - Secular modernizing intelligentsia (intellectuals, reformers, military modernizers, state technocrats) — payer, powerful but constrained; blocked from redefining Turkish identity on secular grounds
 *   - Non-Arabic-literate populations (non-educated rural speakers, craft-tradition carriers, oral-knowledge keepers) — payer, powerless, trapped; must acquire additional script competence to participate in official life
 *   - European-aligned modernization precedent (excluded model) — the alternative script/identity pathway that the constraint structurally marginalizes
 *   - State enforcement apparatus (Ottoman/Turkish state administration) — agenda-setter and payer in tension; benefits from religious legitimacy but bears cost of maintaining dual literacy and resisting modernization pressure
 *   - Generational literacy cohort (young Turks facing the choice between Arabic-script authority access or informal modernity) — payer, identity-locked by the constraint's definition of legitimate knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, 0.68).
domain_priors:suppression_score(turkish_graphemic_substrate__ottoman_continuity_reading, 0.76).
domain_priors:theater_ratio(turkish_graphemic_substrate__ottoman_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__ottoman_continuity_reading, "Ottoman Continuity Reading: Turkish Identity and Arabic Graphemic Substrate").
narrative_ontology:topic_domain(turkish_graphemic_substrate__ottoman_continuity_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__ottoman_continuity_reading, 'f5d0239b-defe-41cd-9a4d-9fe7da01d2f0').
narrative_ontology:cs_kernel_codification('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', distributed).
narrative_ontology:cs_authority_grounding('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', extraction).
narrative_ontology:cs_interpretation_layer_present('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0').
narrative_ontology:cs_reading_relation('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', turkish_graphemic_substrate__secular_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', turkish_graphemic_substrate__gradual_transition_reading, coexists_with).
narrative_ontology:cs_axiom('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', foundational, turkish_identity_continuous_ottoman_islamic).
narrative_ontology:cs_axiom_status(turkish_identity_continuous_ottoman_islamic, holdable).
narrative_ontology:cs_axiom_grounding('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', turkish_identity_continuous_ottoman_islamic, conventional).
narrative_ontology:cs_axiom('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', foundational, arabic_script_sacred_substrate_legitimacy).
narrative_ontology:cs_axiom_status(arabic_script_sacred_substrate_legitimacy, overridden).
narrative_ontology:cs_axiom_grounding('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', arabic_script_sacred_substrate_legitimacy, theological).
narrative_ontology:cs_reference_frame('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', ottoman_civilization_as_turkish_cultural_substrate).
narrative_ontology:cs_drift_state('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', contemporary_secular_modernization_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f5d0239b-defe-41cd-9a4d-9fe7da01d2f0', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literate_religious_establishment).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, islamic_institutional_authority).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, secular_modernizing_intelligentsia).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, non_arabic_literate_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, generational_literacy_cohort).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious scholars, Islamic educational institutions (madrasas), and clerical networks whose authority, pedagogy, and access to foundational texts depend on fluency in Ottoman-Arabic script. They administer the constraint by maintaining Arabic-script curricula, training new generations of clerics, and defining Turkish linguistic identity as inseparable from Islamic civilization's graphemic traditions. Their institutional survival and interpretive authority are constituted through continued command of this script.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literate_religious_establishment, agenda_setter,
    institutional, generational, identity_locked, national).

% The pan-Islamic legitimacy framework that treats Arabic script as the sacred graphemic substrate of Islamic civilization itself. By maintaining Turkish identity's continuity with Ottoman-Islamic precedent, this constraint preserves the authority structure that grounds religious law, doctrine, and community cohesion. A shift to Latin script would rupture the claimed natural continuity and require reconstructing Islamic authority on explicitly chosen/negotiated grounds rather than inherited civilization.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, islamic_institutional_authority, beneficiary,
    institutional, civilizational, identity_locked, national).

% Intellectuals, reformers, military officers, and state-builders who see Turkish national identity as distinct from the Ottoman-Islamic past and modernization as requiring alignment with European (Latin-script) literacy standards. They bear the cost of the constraint by remaining unable to freely author a new Turkish identity narrative disconnected from Ottoman continuity; they must work within or against the inherited framework rather than transcending it. Their exit—adopting Latin script—requires overcoming institutional resistance and social fragmentation.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, secular_modernizing_intelligentsia, payer,
    powerful, biographical, constrained, national).

% Turkish speakers who lack Arabic-script literacy or whose intergenerational knowledge is carried in non-Islamic or non-clerical domains. The constraint traps them: participation in official literacy, religious learning, and institutional legitimacy requires acquiring Arabic-script competence. Their existing knowledge—oral tradition, craft practice, non-religious literature—is devalued as the constraint defines legitimate Turkish identity through the Ottoman-Islamic-Arabic axis. They cannot exit without severing themselves from both traditional and official domains.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, non_arabic_literate_populations, payer,
    powerless, biographical, trapped, national).

% The accumulated literary, legal, and theological works of Ottoman civilization, encoded in Arabic script. This corpus remains accessible and authoritative only under the constraint; a script transition would render it unreadable to a new generation unless maintained in parallel. The constraint vindicates the proposition that this corpus is the legitimate foundation of Turkish cultural identity.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literary_corpus, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literary_corpus).

% The institutional apparatus of Islamic pedagogy: madrasas, Quranic schools, clerical training networks. The constraint maintains this infrastructure by requiring Arabic-script literacy as the condition of religious authority. Without it, alternative modernized or secularized educational pathways would displace religious education's institutional monopoly on legitimacy and literacy training.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, religious_education_infrastructure, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(turkish_graphemic_substrate__ottoman_continuity_reading, religious_education_infrastructure).

% The model of script adoption and national identity reconstruction that successful European modernizers followed: shift to vernacular, Latin-script literacy; align with European institutional and technological standards; establish national identity through rupture with medieval/religious precedent. This precedent is excluded from the domestic conversation by the constraint's framing of Turkish identity as rooted in Ottoman continuity. Advocates for the European modernization path must argue against the inherited legitimacy narrative rather than claim it.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, european_aligned_modernization_precedent, excluded,
    powerful, biographical, constrained, global).

% The Ottoman state administration must actively enforce the constraint by maintaining Arabic-script official channels, requiring it for state education, and suppressing competing literacy pathways. The state benefits from the religious legitimacy the constraint provides but bears the cost of maintaining dual literacy infrastructures and resisting modernizing pressure. It is positioned as an agenda-setter enforcing the Islamic establishment's reading, but with tensions: the state's administrative modernization needs may eventually conflict with the constraint's requirements.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, state_enforcement_apparatus, payer).

% Movements promoting alternative scripts (simplified Arabic, Persian influences, local Turkic variants, or imported Latin script) that would fragment Turkish literacy or enable modernization outside the Ottoman-Islamic framework. The constraint excludes them by treating the Ottoman-Arabic axis as the only legitimate continuation of Turkish identity. Their proponents are marginalized as betrayers of civilization continuity.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, competing_literacy_movements, excluded,
    moderate, biographical, trapped, national).

% Young Turks and emerging generations who face a choice between acquiring Arabic-script literacy (to access institutional authority, religious learning, Ottoman heritage, and pan-Islamic legitimacy) or operating in informal literacy domains outside official recognition. The constraint creates a literacy bottleneck: it makes access to modernized institutional pathways dependent on mastering an archaic script that does not align with the technical and administrative needs of rapid state development. Generational continuity becomes a site of extraction—the young must internalize the constraint or be excluded.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, generational_literacy_cohort, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, generational_literacy_cohort, excluded).

% External analysts observing the constraint's operation: historians of literacy, comparative political scientists, linguists tracking script transitions, Ottoman historians assessing whether continuity claims are descriptively accurate or normatively chosen. They track whether the constraint's persistence rests on genuine functional necessity or on institutional inertia and identity lock-in.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, observer_analyst, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literate_religious_establishment).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves access to Ottoman literary, legal, and theological heritage; maintains institutional coherence of Islamic religious authority across generations; coordinates Turkish identity narrative with pan-Islamic civilization framework; sustains the continuity of Islamic scholarship and jurisprudence as applied to Ottoman/Turkish domains.
% TRANSFER_FUNCTION: Transfers literacy authority and cultural legitimacy from secular modernizers and non-Islamic traditions toward the Islamic institutional establishment and religious scholars. Transfers administrative burden from the state to religious institutions (which maintain Arabic-script education as the price of institutional survival). Transfers access costs to populations without inherited Arabic-script literacy: the powerless and non-religious sectors must acquire an additional script competence to participate in official life.
% ABSENT_VOICES: Secular nationalist intellectuals are partly included (powerful enough to mount pressure) but excluded from setting the legitimacy frame. Competing modernization movements (script reformers, technical modernizers, European-aligned thinkers) are structurally excluded by the constraint's definition of legitimate Turkish identity. Non-literate or orally-transmitted knowledge traditions are rendered inaudible by the constraint's definition of legitimate knowledge itself.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight—if Turkish state and society collectively abandoned the claim that Turkish identity is continuous with Ottoman-Islamic civilization and that Arabic script is its legitimate substrate—the institutional landscape would restructure radically: religious authority would lose its inherited legitimacy basis; Islamic education would require explicit negotiation of its authority rather than claiming it as civilization continuity; secular modernizers would gain institutional authority to redefine Turkish identity; literacy pathways would diversify or shift toward Latin scripts enabling technical modernization; intergenerational knowledge transfer would splinter as the Ottoman literary corpus became inaccessible to new generations without parallel translation effort. The religious establishment would either fade or reconstruct itself on explicitly chosen grounds rather than inherited ones.
% FOUNDING_PROBLEM: After Ottoman collapse and Turkish state formation, the question of what it means to be Turkish, what scripts and traditions legitimately express Turkish identity, and whether the new Turkish state should align with Islamic civilization or with European modernity. The constraint was forged as an answer: Turkish identity remains organically continuous with Ottoman-Islamic civilization; Arabic script embodies that continuity; preserving it maintains legitimacy, institutional coherence, and resistance to European cultural colonization.
% FOUNDING_PROBLEM_CORROBORATION: The Ottoman religious establishment and Islamic intellectuals attest the founding problem persists—Turkish identity IS still threatened by European cultural displacement, and maintaining Ottoman continuity remains necessary to preserve Islamic authority and Turkish civilization's dignity. Secular nationalist intellectuals and Ottoman historians attest the problem has been superseded: Turkish national identity has already begun to differentiate from the Ottoman past; the constraint now functions to prevent modernization and to extract institutional rents from the religious establishment, not to solve a legitimate problem of cultural continuity. Economic historians and literacy researchers (external parties) note that the constraint's persistence correlates more strongly with institutional inertia and identity lock-in than with demonstrated functional necessity for literacy or learning outcomes.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint imposes a literacy bottleneck: it makes access to institutional authority, religious learning, and (until modernization) even official state communication dependent on mastering an archaic script that does not serve technical modernization. The constraint's coordination function (preserving Ottoman literary heritage, maintaining Islamic pedagogical continuity, anchoring Turkish identity) is genuine but increasingly decouplable from script choice; the measured extractiveness reflects the gap between the coordination function's actual cost and the constraint's total enforcement burden. Suppression is high (0.76) because the constraint must actively exclude competing scripts and suppress alternative modernization narratives; if suppression were removed, secular modernizers and younger generations would rapidly shift to Latin scripts and redefine Turkish identity. Theater is moderate (0.42) and increasing over the measurement interval: the constraint begins with real coordination function (preserving Ottoman texts, training religious scholars) but as modernization accelerates, an increasing share of enforcement activity becomes theatrical—defending script choice against technological and administrative reality rather than solving genuine coordination problems. The measurement series shows extractiveness plateauing after year 25 (once modernization pressure peaks and state makes compromises) and theater rising throughout, a pattern consistent with constraint drift toward piton classification under continued modernization pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the religious establishment's seat, this is genuine rope: Turkish identity IS organically continuous with Ottoman-Islamic civilization, the constraint preserves real knowledge (Ottoman literary corpus, Islamic jurisprudence), and Arabic script is the rightful graphemic substrate. From this seat, the measured extractiveness is mostly coordination cost. From the secular modernizer's seat, the same structure is pure snare: the 'continuity' narrative is a retrospective construction used to block modernization, the constraint prevents Turkish identity from being redefined on secular grounds, and the script bottleneck extracts intellectual authority from modernizers. From this seat, the measured extractiveness is mostly rent-seeking. From the powerless non-Arabic-literate population's seat, it is pure extraction: no coordination benefit reaches them, only the cost of requiring additional script competence for participation. The engine computes these seat-specific classifications from the structural data (beneficiary/victim, power, exit options, institutional position); the authored claim does not adjudicate the gap. The perspectival divergence reveals that what appears as coordination from one seat is extraction from another—a signature tangled-rope structure where coordination and asymmetric extraction run through the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious establishment sits as the near-complete beneficiary (d ≈ 0.1): they set the constraint, it preserves their institutional authority, and they bear minimal cost from it—the constraint is their institutional survival strategy. Secular modernizers sit as substantial targets (d ≈ 0.75): they are blocked from redefining identity, they must operate within or against the constraint's frame, and they bear the cost of intellectual constraint. Non-Arabic-literate populations sit as complete targets (d ≈ 0.95): trapped with no exit options, they must either acquire additional literacy or remain outside official recognition—the constraint imposes pure extraction on them with no coordination benefit. The state sits in unstable middle (d ≈ 0.55): it benefits from religious legitimacy the constraint provides but bears the cost of maintaining dual literacy infrastructure and resisting modernization pressure; as modernization accelerates, the state's position drifts toward payer (d increasing). This seat divergence—agenda-setter vs. target vs. powerless—is the structural core of the constraint's extraction: the beneficiary controls the frame that defines legitimate identity, leaving all other seats either constrained or trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('what does Turkish identity mean after Ottoman collapse; how do we preserve continuity while modernizing?') is real at t0 but becomes increasingly contested and arguably dead by t=30+. The constraint persists despite the problem's death/contestation because: (1) the religious establishment's institutional survival depends on maintaining it; (2) state actors benefit from the legitimacy it provides; (3) identity lock-in among religious scholars creates path dependency (exit from Arabic script means loss of professional identity and career pathway). The theater_ratio rising from 0.28 to 0.42 signals performative maintenance increasing—more enforcement effort goes toward defending script choice against technological reality and less toward the actual coordination problem (preserving Ottoman texts, training scholars). This mandatrophy signature prevents misclassifying the constraint as pure rope: the coordination function is real but increasingly decoupled from the constraint's total enforcement burden, and the enforcement increasingly serves institutional rent preservation rather than problem-solving. The constraint is a tangled rope with rising extraction component and falling coordination component—a degradation path toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ottoman_continuity_vs_constructed_identity,
    'Is Turkish linguistic identity genuinely organic-continuous with Ottoman-Islamic civilization, or is the ''continuity'' narrative a constructed frame authored retroactively by the religious establishment to resist modernization?',
    'Historical linguistic analysis of Turkish dialects, Ottoman literary genealogy, and evidence of deliberate pedagogical choices vs. organic transmission paths. Oral history from literacy instructors and learners about whether continuity was taught as inevitable fact or as chosen institutional policy.',
    'If continuity is demonstrated as constructed/chosen, the constraint reclassifies from potential rope (genuine coordination of inherited knowledge) to snare (using heritage claim as cover for institutional rent-seeking). If continuity is demonstrated as organic, the measured extractiveness becomes less clearly separated from coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ottoman_continuity_vs_constructed_identity, empirical, 'Whether Ottoman continuity is descriptive fact or normatively chosen frame.').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For individual agents locked into Arabic-script literacy (religious scholars, clerics, families with Ottoman education), is the exit option ''identity_locked'' because the agent''s professional/relational identity is constituted through the script, or because structural barriers (institutional gatekeeping, social costs, economic dependency) prevent exit?',
    'Post-constraint counterfactual: if script transition were suddenly permitted and supported, how many identity-locked agents would actually exit? Would identity persist even after the structural barrier removes? Interviews with agents at the margin of the constraint revealing which mechanism dominates their choice calculus.',
    'If primarily structural, the constraint''s effective suppression could reduce substantially if barriers were removed. If primarily internalized/identity-constituted, suppression persists after structural barriers fall — the agent carries the constraint''s logic with them into nominally free choice. If mixed, directionality-to-extraction calculations must weight the internalization component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether identity lock is structural or internalized for religious establishment agents.').

omega_variable(
    kernel_reading_under_determination,
    'Does the ''Ottoman continuity'' framing of Turkish linguistic identity represent a genuine kernel reading (a coherent interpretation of a stabilized commitment — the Turkish state''s founding texts and Ottoman heritage), or does the kernel itself remain so under-specified that multiple incommensurable readings project different kernels onto the same ambiguous historical record?',
    'Textual analysis of Ottoman-era founding documents, Turkish state founding texts, and religious educational curricula from the period: can the same kernel be coherently read as supporting both continuity and break narratives, or do the readings require different kernels? Evidence of deliberate interpretive choices by authority figures (clerics, state officials, nationalist intellectuals) in selecting which Ottoman precedents to highlight.',
    'If the kernel is genuinely under-specified, then the ''Ottoman continuity'' reading is one possible projection, not a uniquely necessary interpretation. The constraint''s legitimacy rests on institutional power to enforce one reading, not on the reading''s inevitable fit to the source. If the kernel specifies the continuity narrative, this reading''s authority is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether the kernel itself is well-specified or radically under-determined.').

omega_variable(
    literacy_modernization_separability,
    'Is script modernization (Latin adoption) logically entangled with the break from Ottoman-Islamic identity, or can the two choices be decoupled—adopting new script while maintaining identity continuity, or preserving Ottoman identity while modernizing script?',
    'Historical comparison: cases like Japan (modernizing scripts within traditional identity) vs. Greece (adopting Latin scripts within classical/Orthodox continuity). Analysis of whether Ottoman identity requires Arabic script or merely relies on it by historical accident. Evidence of whether script is the mechanism carrying identity or merely a vehicle for separate content.',
    'If separable, the constraint''s framing of Turkish identity as requiring Arabic script is revealed as a choice, not a structural necessity — it reclassifies from coordination of inherited knowledge to extraction-via-false-necessity. If inseparable, part of the measured extractiveness represents genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_modernization_separability, conceptual, 'Whether script choice and identity continuity are logically entangled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__ottoman_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(turk_tr_t0, observed).
narrative_ontology:measurement(turk_tr_t5, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(turk_tr_t5, observed).
narrative_ontology:measurement(turk_tr_t10, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(turk_tr_t10, observed).
narrative_ontology:measurement(turk_tr_t15, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(turk_tr_t15, observed).
narrative_ontology:measurement(turk_tr_t20, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(turk_tr_t20, observed).
narrative_ontology:measurement(turk_tr_t25, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(turk_tr_t25, observed).
narrative_ontology:measurement(turk_tr_t30, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(turk_tr_t30, observed).
narrative_ontology:measurement(turk_tr_t40, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(turk_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(turk_be_t0, observed).
narrative_ontology:measurement(turk_be_t5, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(turk_be_t5, observed).
narrative_ontology:measurement(turk_be_t10, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(turk_be_t10, observed).
narrative_ontology:measurement(turk_be_t15, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(turk_be_t15, observed).
narrative_ontology:measurement(turk_be_t20, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(turk_be_t20, observed).
narrative_ontology:measurement(turk_be_t25, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(turk_be_t25, observed).
narrative_ontology:measurement(turk_be_t30, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(turk_be_t30, observed).
narrative_ontology:measurement(turk_be_t40, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(turk_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(turk_su_t0, observed).
narrative_ontology:measurement(turk_su_t5, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(turk_su_t5, observed).
narrative_ontology:measurement(turk_su_t10, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(turk_su_t10, observed).
narrative_ontology:measurement(turk_su_t15, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(turk_su_t15, observed).
narrative_ontology:measurement(turk_su_t20, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(turk_su_t20, observed).
narrative_ontology:measurement(turk_su_t25, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(turk_su_t25, observed).
narrative_ontology:measurement(turk_su_t30, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(turk_su_t30, observed).
narrative_ontology:measurement(turk_su_t40, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(turk_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__ottoman_continuity_reading, 0.12).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__gradual_transition_reading).

% DUAL FORMULATION NOTE:
% The turkish_graphemic_substrate kernel has three structurally distinct constraint readings: ottoman_continuity_reading (this story), secular_nationalist_reading, and gradual_transition_reading. Each reading instantiates a different constraint with different beneficiaries, victims, measured extractiveness, and founding-problem status, despite sharing the same ambiguous kernel. They form a constraint family linked by network.affects_constraints; each story must declare the others. The disagreement is located at the level of: (1) which is the legitimate continuation of Turkish identity; (2) whether script choice is entangled with identity or separable; (3) whether modernization requires identity break or can occur within continuity. The readings are coexisting live positions held by different institutional factions, not foreclosed alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__ottoman_continuity_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
