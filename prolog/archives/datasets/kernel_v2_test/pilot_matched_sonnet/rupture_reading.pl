% ============================================================================
% CONSTRAINT STORY: rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rupture_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rupture_reading
 *   human_readable: Script Reform as Deliberate Cultural Rupture (Rupture Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish script reform replaced Arabic script with Latin script
 *   by state decree, rendering the entire pre-reform literate population
 *   functionally illiterate overnight. This constraint story models the
 *   RUPTURE READING of the orthographic kernel: the reform was a deliberate
 *   cultural severance designed to create a new national identity by making
 *   the Ottoman/Islamic past inaccessible. The reform's coordination function
 *   (standardizing literacy, enabling mass education) was real but secondary
 *   to its extraction function: severing intergenerational knowledge
 *   transmission and consolidating state control over cultural memory. The
 *   constraint exhibits extremely high extractiveness (0.78) and suppression
 *   (0.85) because the victims — the entire pre-reform literate population,
 *   religious scholars, and cultural continuity itself — had no exit and bore
 *   the full cost of the rupture. The beneficiaries — the Kemalist state
 *   apparatus and post-reform educated elite — captured the legitimacy and
 *   political benefits of the reform. The theater_ratio is relatively low
 *   (0.35) because the reform was functionally effective at its stated goal
 *   (increasing literacy) even though that goal was cover for the extraction
 *   mechanism. This is a commitment system constraint: the Latin script is
 *   the kernel, the Kemalist state is the authority, and the reform's
 *   legitimacy claim (modernization requires rupture) is the axiom that
 *   distinguishes this reading from its siblings.
 *
 * KEY AGENTS:
 *   - Pre-Reform Literate Population: Primary victim (powerless/identity_locked) — overnight illiteracy; cultural capital rendered worthless; identity constituted through Ottoman literacy dissolved
 *   - Religious Scholars (Ulema): Primary victim (powerless/trapped) — textual foundation of religious authority severed; professional identity and institutional position script-dependent; generational transmission chain destroyed
 *   - Kemalist State Apparatus: Primary beneficiary (institutional/arbitrage) — captures political legitimacy from modernization narrative; controls reform implementation; can maintain internal Ottoman literacy while mandating Latin script publicly
 *   - Post-Reform Educated Elite: Secondary beneficiary (organized/constrained) — social position depends on new script; frames rupture as temporary necessity; expects eventual cultural reconnection
 *   - Provincial Educators: Mixed position (moderate/constrained) — experience both coordination (literacy standardization enables mass education) and extraction (must abandon own literacy and implement reform that severs students from parents' textual world)
 *   - Analytical Observer (Rupture Framing): Civilizational view (analytical/analytical) — sees coordination story as cover for state-building extraction; suppression is structural and extraction is severe
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rupture_reading, 0.78).
domain_priors:suppression_score(rupture_reading, 0.85).
domain_priors:theater_ratio(rupture_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rupture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(rupture_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rupture_reading, snare).
narrative_ontology:human_readable(rupture_reading, "Script Reform as Deliberate Cultural Rupture (Rupture Reading)").
narrative_ontology:topic_domain(rupture_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rupture_reading, '5c0ee6b5-2873-411d-b8da-1ba6e6fffa72').
narrative_ontology:cs_kernel_codification('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', formalized).
narrative_ontology:cs_authority_grounding('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', extraction).
narrative_ontology:cs_reading_relation('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', rupture_reading__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', rupture_reading__modernization_reading, coexists_with).
narrative_ontology:cs_axiom('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', foundational, cultural_rupture_prerequisite_for_modernization).
narrative_ontology:cs_axiom_status(cultural_rupture_prerequisite_for_modernization, holdable).
narrative_ontology:cs_axiom_grounding('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', cultural_rupture_prerequisite_for_modernization, instrumental).
narrative_ontology:cs_axiom('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', foundational, script_determines_civilizational_identity).
narrative_ontology:cs_axiom_status(script_determines_civilizational_identity, holdable).
narrative_ontology:cs_axiom_grounding('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', script_determines_civilizational_identity, conventional).
narrative_ontology:cs_axiom('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', secondary, ottoman_past_incompatible_with_republican_future).
narrative_ontology:cs_axiom_status(ottoman_past_incompatible_with_republican_future, holdable).
narrative_ontology:cs_axiom_grounding('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', ottoman_past_incompatible_with_republican_future, deontological).
narrative_ontology:cs_reference_frame('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', ottoman_imperial_continuity).
narrative_ontology:cs_drift_state('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', post_1928_reform, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('5c0ee6b5-2873-411d-b8da-1ba6e6fffa72', '').
narrative_ontology:cs_kernel_id(rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(rupture_reading, post_reform_educated_elite).
narrative_ontology:constraint_beneficiary(rupture_reading, secularist_coalition).
narrative_ontology:constraint_victim(rupture_reading, pre_reform_literate_population).
narrative_ontology:constraint_victim(rupture_reading, religious_scholars).
narrative_ontology:constraint_victim(rupture_reading, ottoman_cultural_continuity).
narrative_ontology:constraint_victim(rupture_reading, intergenerational_knowledge_transmission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rupture_reading, provincial_educators).
narrative_ontology:constraint_victim(rupture_reading, provincial_educators).
narrative_ontology:constraint_vindicates(rupture_reading, cultural_rupture_as_modernization_prerequisite).
narrative_ontology:constraint_vindicates(rupture_reading, script_as_identity_determinant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rendered functionally illiterate overnight by state decree. Could learn Latin script but doing so requires abandoning the identity constituted through Ottoman literacy. Cultural capital accumulated over a lifetime becomes worthless. Cannot access the textual archive that defined Ottoman intellectual life.
narrative_ontology:constraint_stakeholder(rupture_reading, pre_reform_literate_population, payer,
    powerless, biographical, identity_locked, national).

% Textual foundation of religious authority severed. Religious texts, legal precedents, and scholarly commentaries are in Arabic script. Professional identity and institutional position are script-dependent. The reform destroys the transmission chain that grounds religious authority across generations.
narrative_ontology:constraint_stakeholder(rupture_reading, religious_scholars, payer,
    powerless, generational, trapped, national).

% Controls reform implementation and captures political legitimacy from the modernization narrative. Can maintain internal Ottoman script literacy while mandating Latin script publicly. The reform serves state-building goals: severing the population from the Ottoman/Islamic past and creating a new national identity aligned with Western modernity.
narrative_ontology:constraint_stakeholder(rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, immediate, arbitrage, national).

% Social position depends on the new script. Recognize the cultural cost but frame the rupture as a temporary necessity for modernization. Expect that once modernization is achieved, some cultural reconnection will be possible through translation and scholarship.
narrative_ontology:constraint_stakeholder(rupture_reading, post_reform_educated_elite, beneficiary,
    organized, generational, constrained, national).

% Must abandon their own Ottoman literacy and retrain while implementing a reform that severs students from their parents' textual world. The reform does solve the literacy standardization problem and enables mass education, but at the cost of intergenerational knowledge transmission. Implementation varies by province.
narrative_ontology:constraint_stakeholder(rupture_reading, provincial_educators, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rupture_reading, provincial_educators, beneficiary).

% Abstract collective good representing intergenerational knowledge transmission and cultural memory. The reform severs access to the Ottoman textual archive and destroys the transmission chain. Not an agent but included for narrative completeness — the cultural continuity victim is structurally real even though it cannot organize or advocate.
narrative_ontology:constraint_stakeholder(rupture_reading, ottoman_cultural_continuity, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(rupture_reading, ottoman_cultural_continuity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizing orthography and enabling mass literacy in a population with low literacy rates and high dialectal variation.
% TRANSFER_FUNCTION: Cultural capital (Ottoman literacy, access to textual archive, religious authority) from pre-reform literate population and religious scholars to the Kemalist state apparatus and post-reform educated elite. Political legitimacy from the modernization narrative accrues to the state.
% ABSENT_VOICES: Ottoman loyalists, Islamic traditionalists, and non-Kemalist intellectuals who would have advocated for Ottoman script reform rather than replacement. These voices were excluded through political repression and the single-party state structure. The absence is structural, not incidental — the reform required foreclosing dissent.
% DISAPPEARANCE_RATIONALE: If the script reform disappeared (i.e., was reversed or never implemented), the entire post-1928 Turkish state identity would rearrange. The reform is constitutive of the Kemalist state-building project. Literacy patterns, educational institutions, cultural memory, and national identity are all organized around the Latin script. Reversal would require rebuilding these arrangements.
% FOUNDING_PROBLEM: Low literacy rates in the late Ottoman period, lack of orthographic standardization, and the Kemalist state's need to create a new national identity distinct from the Ottoman/Islamic past.
% FOUNDING_PROBLEM_CORROBORATION: The literacy problem is corroborated by Ottoman-era census data and educational records (external to beneficiaries). The state-building problem is corroborated by Kemalist state documents and speeches (internal to beneficiaries). The contestation is whether the literacy problem required script replacement or could have been solved through Ottoman script reform — this question has no neutral arbiter because the counterfactual was structurally foreclosed.
narrative_ontology:disappearance_verdict(rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(rupture_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRE-REFORM LITERATE POPULATION (SNARE) — Identity-locked rather than materially trapped: could learn Latin script but doing so requires abandoning the identity constituted through Ottoman literacy. Overnight illiteracy is not a physical barrier but an identity dissolution. The entire cultural archive becomes inaccessible not because the books burned but because the reading practice that constituted Ottoman intellectual identity was severed. Maximum extraction: cultural capital accumulated over a lifetime rendered worthless by state decree.
constraint_indexing:constraint_classification(rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS SCHOLARS (SNARE) — Trapped rather than identity-locked: material barriers compound identity barriers. Religious texts, legal precedents, and scholarly commentaries are in Arabic script. The reform severs access to the textual foundation of religious authority. Cannot exit because professional identity, institutional position, and textual expertise are all script-dependent. Generational time horizon: sees the reform as destroying the transmission chain that grounds religious authority.
constraint_indexing:constraint_classification(rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: KEMALIST STATE APPARATUS (ROPE) — Primary beneficiary with arbitrage exit. Experiences the reform as pure coordination: creating a unified national identity requires a shared script, and Latin script signals modernity and Western alignment. The state apparatus controls the reform's implementation and captures the legitimacy benefit. Arbitrage exit: the state can maintain Ottoman script literacy internally while mandating Latin script publicly, or can reverse the reform if politically expedient (though it never does). Immediate time horizon: the reform's political benefits accrue within the founding generation.
constraint_indexing:constraint_classification(rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POST-REFORM EDUCATED ELITE (SCAFFOLD) — Organized beneficiaries who see the reform as temporary rupture necessary for modernization. Constrained exit: their social position depends on the new script, but they recognize the cultural cost and frame the reform as transitional. Generational time horizon: expects that once modernization is achieved, some cultural reconnection will be possible (through translation, historical scholarship, or selective revival). The sunset is implicit: the rupture is justified as a necessary break, not a permanent state.
constraint_indexing:constraint_classification(rupture_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROVINCIAL EDUCATORS (TANGLED ROPE) — Moderate power, constrained exit. Experience both coordination (the reform does solve the literacy standardization problem and enables mass education) and extraction (must abandon their own Ottoman literacy and retrain, while implementing a reform that severs students from their parents' textual world). Biographical time horizon: the reform defines their professional lives. Regional scope: implementation varies by province, and some maintain informal Ottoman literacy instruction.
constraint_indexing:constraint_classification(rupture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / RUPTURE FRAMING (SNARE) — From the civilizational/global analytical position, this reading sees the reform as deliberate extraction: the coordination story (literacy, modernization) is cover for a state-building project that required severing the population from its own past. The suppression is structural (legal mandate, educational monopoly, cultural stigma) and the extraction is severe (intergenerational knowledge transmission destroyed). This perspective holds that the rupture was the point, not a side effect.
constraint_indexing:constraint_classification(rupture_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rupture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rupture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rupture_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The reform destroyed the cultural capital of the entire pre-reform literate population and severed intergenerational knowledge transmission. The coordination function (literacy standardization) was real but could have been achieved through Ottoman script reform without the rupture. The extraction is not total (0.95+) because some cultural reconnection has occurred through translation and academic scholarship, and the reform did deliver on its stated literacy goals. Suppression (0.85): Very high. Legal mandate (Ottoman script banned in public education and official documents), educational monopoly (no alternative script instruction permitted), and cultural stigma (Ottoman script associated with backwardness). Suppression has declined over time as legal barriers softened and some Ottoman script education became available, but internalized suppression persists — post-reform generations perceive Ottoman script as foreign. Theater_ratio (0.35): Low-moderate. The reform was functionally effective at increasing literacy rates and standardizing orthography. The theater is in the modernization narrative (the claim that Latin script was necessary for modernization rather than one path among several), not in the literacy function itself. Theater has increased slightly over time as the modernization justification has become more ritualistic.
 *
 * PERSPECTIVAL GAP:
 *   The rupture reading produces a stark perspectival gap. The Kemalist state apparatus sees pure coordination (Rope) — the reform solves the literacy standardization problem and signals modernity. The post-reform educated elite see a temporary rupture with a sunset (Scaffold) — the cultural cost is acknowledged but framed as necessary for modernization. Provincial educators see mixed coordination and extraction (Tangled Rope) — the reform both enables and constrains their work. The victim groups — pre-reform literate population and religious scholars — see pure extraction (Snare) — their cultural capital is destroyed and they have no exit. The analytical observer in the rupture framing also sees Snare — the coordination story is cover for state-building extraction. The gap is not resolvable within a single perspective because the structural positions are genuinely different: the beneficiaries experience the reform as coordination because extraction runs toward them, while the victims experience it as extraction because they bear the cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The rupture reading assigns very high directionality (d → 1.0) to the victim groups: pre-reform literate population and religious scholars are full targets of the extraction. The identity_locked exit option for the pre-reform literate population reflects that the barrier is cognitive/identity-based rather than purely material — they could learn Latin script but doing so requires abandoning the identity constituted through Ottoman literacy. The trapped exit option for religious scholars reflects material barriers (textual expertise rendered obsolete, institutional position undermined) compounding identity barriers. The Kemalist state apparatus has very low directionality (d → 0.0) as primary beneficiary with arbitrage exit — the state captures the political legitimacy benefit and controls implementation. The post-reform educated elite have low directionality (d → 0.2-0.3) as secondary beneficiaries who frame the rupture as temporary. Provincial educators have moderate directionality (d → 0.5) reflecting their mixed position — they experience both the coordination function and the extraction. The analytical observer in the rupture framing sees the constraint as primarily extractive, with high d for the victim groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading resolves mandatrophy by clearly distinguishing the coordination function (literacy standardization, mass education) from the extraction function (cultural rupture, intergenerational transmission collapse). The reform had a genuine coordination mandate — Ottoman script's complexity and lack of standardization were real barriers to mass literacy. But the mandate could have been fulfilled through Ottoman script reform (simplified orthography, vowel marking, standardization) without the cultural rupture. The Latin script reform was chosen not because it was the only path to literacy but because it served the state-building extraction function: severing the population from the Ottoman/Islamic past and creating a new national identity. The constraint is a Snare from the victim perspectives because the extraction dominates the coordination, and a Tangled Rope from the moderate perspectives because both functions are real. The mandatrophy is resolved by recognizing that the coordination function was real but instrumentalized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the 1928 script reform a deliberate cultural rupture (this reading), a modernization coordination mechanism (modernization_reading), or a natural evolution of Turkish linguistic identity (continuity_reading)?',
    'Historical analysis of reform architects'' stated intentions vs. structural outcomes; comparison with other script reforms (e.g., Soviet Central Asia, Mongolia) that achieved literacy gains without severing cultural continuity; examination of whether alternative paths (gradual transition, dual-script period, Ottoman script simplification) were seriously considered or structurally foreclosed.',
    'If rupture: ε remains high (0.78), victims correctly identified. If modernization: ε drops to ~0.35, beneficiary set expands to include general population. If continuity: ε drops to ~0.15, constraint reclassifies as rope or scaffold from most perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the orthographic kernel is structurally correct').

omega_variable(
    counterfactual_literacy_trajectory,
    'Would literacy rates have risen comparably under Ottoman script reform (simplified Arabic script, vowel marking, standardized orthography) without the cultural rupture?',
    'Comparison with Ottoman script reforms in other Turkic regions; analysis of pre-1928 Ottoman literacy initiatives; examination of Arabic script literacy trajectories in other modernizing states (Egypt, Iran) during the same period.',
    'If yes: the rupture was unnecessary for the coordination function, confirming high extraction. If no: the Latin script was structurally necessary for mass literacy, reducing extraction estimate and supporting modernization_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_literacy_trajectory, empirical, 'Whether Ottoman script reform could have achieved comparable literacy gains').

omega_variable(
    intergenerational_transmission_collapse,
    'What proportion of pre-reform cultural knowledge (literature, legal precedent, historical memory, religious scholarship) was permanently lost vs. recovered through translation and academic scholarship?',
    'Bibliometric analysis of Ottoman-era texts translated post-1928; surveys of contemporary Turkish access to pre-reform literature; comparison of religious scholarship continuity in Turkey vs. Arabic-script-maintaining regions.',
    'If >70% lost: confirms severe extraction from cultural continuity victim. If <30% lost: extraction was temporary, supporting scaffold perspective from educated elite.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_transmission_collapse, empirical, 'Magnitude of permanent cultural knowledge loss from script rupture').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.85) primarily structural (legal mandate, educational monopoly) or internalized (post-reform generations perceive Ottoman script as foreign, backward, or illegitimate)?',
    'Longitudinal surveys of Turkish attitudes toward Ottoman script across generations; analysis of whether suppression persists after legal barriers are removed (e.g., contemporary access to Ottoman script education); comparison with post-Soviet states where script reforms were partially reversed.',
    'If primarily structural: suppression could be reduced by policy change. If primarily internalized: suppression is self-sustaining and the identity_locked exit option applies to post-reform generations as well.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or has been internalized across generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rupture_reading, 0, 98).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rupture_theater_pre_reform, rupture_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(rupture_theater_reform_year, rupture_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(rupture_theater_first_generation, rupture_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(rupture_theater_second_generation, rupture_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(rupture_theater_contemporary, rupture_reading, theater_ratio, 98, 0.4).

% Extraction over time
narrative_ontology:measurement(rupture_extract_pre_reform, rupture_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rupture_extract_reform_year, rupture_reading, base_extractiveness, 1, 0.78).
narrative_ontology:measurement(rupture_extract_consolidation, rupture_reading, base_extractiveness, 3, 0.82).
narrative_ontology:measurement(rupture_extract_first_generation, rupture_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(rupture_extract_second_generation, rupture_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(rupture_extract_third_generation, rupture_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(rupture_extract_contemporary, rupture_reading, base_extractiveness, 98, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(rupture_suppress_pre_reform, rupture_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(rupture_suppress_reform_year, rupture_reading, suppression_requirement, 1, 0.85).
narrative_ontology:measurement(rupture_suppress_consolidation, rupture_reading, suppression_requirement, 3, 0.9).
narrative_ontology:measurement(rupture_suppress_first_generation, rupture_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(rupture_suppress_second_generation, rupture_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(rupture_suppress_third_generation, rupture_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(rupture_suppress_contemporary, rupture_reading, suppression_requirement, 98, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(rupture_reading, continuity_reading).
narrative_ontology:affects_constraint(rupture_reading, modernization_reading).

% DUAL FORMULATION NOTE:
% The orthographic kernel decomposes into three readings with different ε values. The rupture reading has the highest ε (0.78) because it foregrounds the extraction from cultural continuity. The modernization reading has moderate ε (~0.35) because it treats cultural rupture as a side effect of coordination. The continuity reading has low ε (~0.15) because it denies that rupture occurred. These are not the same constraint viewed from different angles — they are structurally distinct claims about what the reform was and what it did.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
