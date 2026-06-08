% ============================================================================
% CONSTRAINT STORY: orthographic_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel_flat_control, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: orthographic_kernel_flat_control
 *   human_readable: Orthographic Standard for Written Turkish
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   The orthographic standard for written Turkish represents a foundational
 *   commitment system grounding state legitimacy, bureaucratic capacity, and
 *   cultural identity. The constraint embeds both genuine coordination
 *   function (solving the practical problem of administering a linguistically
 *   diverse polity through standardized written communication) and extraction
 *   mechanism (suppressing competing scripts and dialects, particularly
 *   minority languages like Kurdish, Laz, Circassian, and Armenian). The
 *   standard emerged from deliberate state policy during the Turkish alphabet
 *   reform (1928) and subsequent institutional consolidation through
 *   education and bureaucracy. This constraint exemplifies how a commitment
 *   system can simultaneously solve real coordination problems and enforce
 *   cultural hierarchy: the state genuinely needs reliable written standards
 *   to administer territory and collect taxes; minorities genuinely lose
 *   written representation and linguistic legitimacy in the process. The
 *   theater ratio (0.38) reflects that orthographic standardization is
 *   primarily functional rather than performative — the Language Academy's
 *   debates over spelling and etymological purity are ornamental compared to
 *   the enforcement mechanisms embedded in schools, bureaucracy, and legal
 *   documentation. The extractiveness trajectory (0.35 → 0.58 over the
 *   interval) indicates rising pressure and enforcement as the standard
 *   deepens its institutional grip and as alternative scripts face increasing
 *   marginalization. The suppression trajectory (0.52 → 0.70) shows that
 *   enforcement intensity has grown, requiring more active suppression of
 *   competing scripts and more extensive barrier maintenance to prevent
 *   competing literacies from taking institutional root.
 *
 * KEY AGENTS:
 *   - State Bureaucracy: Primary beneficiary (institutional/arbitrage) — direct beneficiary from administrative standardization, perfect exit optionality (standard serves state interests)
 *   - Educational Establishment: Secondary beneficiary (organized/arbitrage) — benefits from curricular standardization, credential pathways, institutional coherence
 *   - Standardizing Elites (Language Academy, policy-makers): Tertiary beneficiary (organized/arbitrage) — maintains cultural authority through standard-setting, prestige through etymological control
 *   - Rural Dialect Speakers: Primary victim (powerless/trapped) — geographic and economic barriers prevent alternative literacy; local speech suppressed in written form
 *   - Minority Script Communities (Kurdish, Laz, Armenian, Circassian): Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with competing orthographic traditions; script erasure equals identity erasure
 *   - Illiterate Populations: Secondary victim (powerless/trapped) — barriers to literacy include script standardization; alternative pathways (oral transmission, non-standard scripts) systematically blocked
 *   - Analytical Observer: Witnesses full dual structure (analytical/analytical) — recognizes coordination function and extraction mechanism as inseparable aspects of the same constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel_flat_control, 0.55).
domain_priors:suppression_score(orthographic_kernel_flat_control, 0.68).
domain_priors:theater_ratio(orthographic_kernel_flat_control, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, extractiveness, 0.55).
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel_flat_control, "Orthographic Standard for Written Turkish").
narrative_ontology:topic_domain(orthographic_kernel_flat_control, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(orthographic_kernel_flat_control, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, standardizing_elites).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, education_establishment).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, regional_dialect_speakers).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, illiterate_populations).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, competing_script_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, educational_establishment).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, urban_middle_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, nationalist_intellectuals).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, rural_dialect_speakers).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, minority_script_communities).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, urban_middle_class).
narrative_ontology:constraint_vindicates(orthographic_kernel_flat_control, national_linguistic_unity_doctrine).
narrative_ontology:constraint_vindicates(orthographic_kernel_flat_control, literacy_as_state_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets, administers, and enforces the orthographic standard through legal documentation, official communications, and state records. Defines what counts as legitimate written form for contracts, deeds, legal proceedings, and tax documents. Can use alternative scripts but chooses not to because the standard serves bureaucratic efficiency. Perfect exit optionality — has every reason to maintain the standard and no structural reason to abandon it.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, state_bureaucracy, agenda_setter,
    institutional, immediate, arbitrage, national).

% Implements and enforces the standard through curriculum, textbooks, examination systems, and credential pathways. Teachers' expertise and professional identity are tied to the standard form. Schools serve as primary institutional mechanism for transmitting the standard to new generations. Benefits from standardized pedagogy and national credential recognition. Could theoretically teach regional scripts alongside Turkish orthography but would lose institutional coherence and credential portability.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, educational_establishment, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel_flat_control, educational_establishment, beneficiary).

% Turkish Language Association (TDK) maintains dictionaries, rules on disputed spellings, promotes etymological purity, and adjudicates standardization questions. Maintains authority through cultural prestige and formal institutional status. Theater component grows as the academy's functional role diminishes (schools and bureaucracy now carry enforcement) but prestige and legitimacy remain tied to standard-setting authority.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, language_academy, agenda_setter,
    organized, civilizational, arbitrage, national).

% Speak non-standard Turkish dialects (Anatolian, Black Sea, Eastern variants) with distinct phonology, vocabulary, and grammar. Geographic isolation and economic barriers prevent easy access to standard Turkish instruction. Literacy access requires learning the standardized form, which requires abandoning or de-emphasizing local speech norms. Children in rural schools learn standardized written Turkish that differs from spoken local dialect, creating cognitive friction and potential language loss. No exit from national education system; standard script is a gatekeeping requirement for economic mobility.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, rural_dialect_speakers, payer,
    powerless, biographical, trapped, local).

% Communities with historical written scripts competing with Turkish orthography: Kurdish (Kurmanji, Sorani, Zazaki varieties), Laz, Circassian, Armenian. Identity historically fused with script — written identity = ethnic identity. Standard Turkish orthography becomes gateway requirement for state access (education, employment, documentation) but acquiring it means learning to write in a script that erases community identity. Structurally could learn Turkish script (material barriers to learning are moderate) but cannot do so without internal identity cost — self-conception is constituted through the competing script. For Kurdish speakers with centuries of written tradition in Kurdish script, Turkish orthography is experienced as involuntary script replacement and cultural displacement, not as learning a neutral administrative tool.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, minority_script_communities, payer,
    powerless, biographical, identity_locked, national).

% Populations without access to standardized instruction (rural areas, extreme poverty, repeated displacement). Literacy pathways are controlled by state education system, which teaches exclusively in standardized orthography with no alternative routes (no recognized non-standard scripts, minimal oral transmission of written knowledge, minimal community-based literacy alternatives). Trapped in illiteracy by monolingual standardization requirement combined with economic/geographic barriers to accessing instruction. Cannot exit literacy deprivation; cannot access state services requiring written documentation.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, illiterate_populations, payer,
    powerless, biographical, trapped, national).

% Urban educated populations who learned standard Turkish orthography through school and daily institutional interaction. Benefits from the standard: reliable documentation, professional credentials, state service access, economic mobility, cultural participation in urban national sphere. Bears learning costs (time, cognitive effort in childhood education) but these are distributed across entire education system, making them feel 'natural.' Constrained exit: could use non-standard scripts but would sacrifice state integration and economic opportunity. Net beneficiary but not without real costs.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, urban_middle_class, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel_flat_control, urban_middle_class, payer).

% Organizations, intellectuals, and activists advocating for recognition of minority scripts or alternative standardization approaches. Excluded from official standard-setting (seat at table is denied); can advocate in civil society and academia but cannot set policy. Would argue for digraphia (dual script recognition), regional autonomy in orthographic choice, or reformed standardization that preserves minority scripts. Constrained by lack of institutional power and by state suppression of competing script movements. Not seated in decision-making but present in the accountability question of whether the constraint serves all parties fairly.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, competing_script_advocates, excluded,
    moderate, generational, constrained, national).

% Intellectuals, writers, journalists, and cultural authorities for whom orthographic standardization vindicates nationalist identity construction. The standard represents 'Turkish' as a unified written nation, erasing regional and minority variation in service of national coherence. Benefits from prestige and cultural authority tied to 'purifying' and standardizing the language. Perfect exit optionality — could advocate for multilingual recognition but have no incentive to do so because nationalist standardization serves their interests.
narrative_ontology:constraint_stakeholder(orthographic_kernel_flat_control, nationalist_intellectuals, beneficiary,
    powerful, civilizational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provide reliable standardized written communication for administration of a linguistically diverse territory; enable consistent record-keeping, legal documentation, tax collection, and bureaucratic scaling without needing to process multiple scripts or dialects; create a common written form accessible across regions to facilitate trade, education, and state service integration.
% TRANSFER_FUNCTION: Monolingual standardization concentrates literacy-based power and access toward the state bureaucracy, educators, and elites who control the standard; toward those fluent in standard Turkish; toward urban populations with access to standardized education. It transfers written recognition away from minority communities and regional dialects, which lose institutional legitimacy and economic value. Written communication becomes a flow primarily in one direction: standardized Turkish → all citizens, with reverse flows limited to those able to write in the standard form.
% ABSENT_VOICES: Communities with competing historical scripts (Kurdish, Laz, Armenian, Circassian) whose voice in orthographic policy would argue for multilingual recognition or regional autonomy. They are not at the table where the standard is maintained because the standard's enforcement mechanism excludes them from institutional power. If they were present, they would contest the monolingual assumption and argue for recognized alternatives.
% DISAPPEARANCE_RATIONALE: If the orthographic standard disappeared overnight, Turkish state administration would face immediate crisis: contracts, legal documents, tax records, and bureaucratic communication would become incomprehensible in the absence of agreed written form. The state would be forced to rapidly reconstitute some standardization mechanism (whether monolingual or multilingual). Civil institutions would reorganize around alternative communication norms — either reverting to Ottoman script (historically literate populations), adopting competing scripts (minority communities), or negotiating new pluralistic standards. The coordination function is real: removing the standard does not leave the world unchanged, it creates administrative chaos until alternatives emerge. This vindicates the Rope classification at the state level — the coordination problem is not invented.
% FOUNDING_PROBLEM: Ottoman Empire's institutional fragmentation and the early Turkish Republic's need to rapidly scale bureaucratic capacity, national identity, and state control after the 1923 foundation. The Ottoman script (Arabic-derived) was deeply associated with Islamic institutions and Ottoman multilingualism; the Latin-based Turkish alphabet (adopted 1928) served both modernization symbolism and administrative efficiency goals. The founding problem was: how to create a unified written nation-state from diverse linguistic and script-using populations?
% FOUNDING_PROBLEM_CORROBORATION: Attest by: state historians (Sunay Akin, Stanford Shaw, Erik-Jan Zurcher on Turkish state formation); comparative bureaucratic studies (Myron Weiner on India's multilingual administration); institutional analysis showing that orthographic standardization intensity does not correlate with bureaucratic effectiveness across countries. The corroboration comes from outside the beneficiary set (outside the state establishment and education system defending the standard) — it is grounded in independent scholarly assessment of state formation requirements.
narrative_ontology:disappearance_verdict(orthographic_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL DIALECT SPEAKER (SNARE) — Cannot exit the orthographic requirement; literacy access depends on learning the standardized form, which suppresses local speech norms. Trapped by geographic and economic barriers; no alternative script or recognition of local written form. Full extraction: linguistic erasure without compensation or transition pathway.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY SCRIPT COMMUNITY (SNARE) — Structurally mobile (could learn new script) but identity-fused with competing orthographic tradition (Kurdish, Laz, Circassian, Armenian communities with their own historical scripts). Identity lock prevents cognitive switch even when material barriers to learning Turkish orthography are low. Experiences suppression as erasure of legitimate written identity, not merely as learning requirement. High extraction: forced assimilation mechanism.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN MIDDLE-CLASS LEARNER (TANGLED ROPE) — Coordinated by the orthographic standard: accessing state services, education, employment, and social mobility requires literacy in the standard form. Benefits from the coordination (reliable documentation, national bureaucratic integration) while bearing the cost of learning time and cognitive discipline. Constrained exit: could theoretically use regional script but would sacrifice state access. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE BUREAUCRACY (ROPE) — Direct beneficiary. Orthographic standard enables consistent record-keeping, tax collection, legal administration, and citizen identification. Experiences the constraint as pure coordination: standardized writing solves genuine administrative problems. Arbitrage exit: could use alternative scripts but has no reason to (standardization creates positive feedback). Primary beneficiary with maximum agency.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EDUCATIONAL ESTABLISHMENT (ROPE) — Secondary beneficiary (organized/arbitrage). Orthographic standard creates standardized curriculum, credential pathways, and institutional coherence across schools. Teachers' expertise is tied to the standard; implementation mechanisms (textbooks, examinations, pedagogy) derive from it. Benefits from institutional consistency and professional identity. Sees the constraint as enabling education delivery, not as extractive imposition.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LANGUAGE ACADEMY / STANDARDIZING AUTHORITY (PITON) — Once-functional orthographic design authority, now mostly theatrical. The Turkish Language Association (TDK) maintains the standard through publishing dictionaries and ruling on disputed spellings, but the actual transmission and enforcement of the standard has migrated to schools and bureaucracy. Theater ratio reflects that much academy activity (etymological purity debates, archaic term promotion) is disconnected from functional standardization. Persists through institutional inertia and cultural prestige, not essential function.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes that orthographic standardization is BOTH a genuine coordination solution to multilingualism and administrative scaling AND an extraction mechanism that suppresses dialect diversity and enforces linguistic assimilation. The same structure that enables reliable documentation (coordination) enables demographic targeting and script erasure (extraction). The perspectival gap between powerless/trapped and institutional/arbitrage agents reveals the dual character: the state experiences coordination; suppressed communities experience extraction. No single type captures the asymmetry — Tangled Rope accurately models the hybrid.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(orthographic_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orthographic_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(orthographic_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(orthographic_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high, rising over the interval (0.35 → 0.58). The standard solves real administrative coordination problems (reliable documentation, tax collection, legal administration), reducing base extractiveness. But the benefit concentrates in the state bureaucracy and educated elites while costs concentrate in dialect speakers and minority communities. The rising trajectory reflects institutionalization: as the standard deepens through education and bureaucracy, competing scripts face increasing marginalization, and the switching costs for minorities rise. At t=0 (early standardization), alternatives still had some institutional foothold (Ottoman script in older populations, minority scripts in minority regions). By t=90 (deep standardization), switching costs become prohibitive — entire educational systems require Turkish orthography; state documents accept only Turkish script; professional advancement demands standard literacy. Suppression (0.68): High and rising (0.52 → 0.70). This reflects the active enforcement machinery required to maintain monolingual standardization: school curricula that exclude minority scripts, legal requirements for state documentation in Turkish orthography, bureaucratic gatekeeping that requires standard literacy, social stigma attached to non-standard speech in written form. Suppression is not passive (the standard doesn't naturally squeeze out alternatives); it requires continuous institutional work. The rising trajectory reflects hardening of enforcement: as minority script communities become more educated and potentially organized to resist, suppression intensifies. Theater ratio (0.38): Low and slowly rising (0.28 → 0.41). The constraint is primarily functional rather than performative. Most of the work of standardization happens through routine institutional channels (schooling, bureaucracy, documentation) rather than through explicit theatrical authority displays. The Language Academy's pronouncements on spelling and etymology have real effects (dictionaries shape teaching materials) but affect a small proportion of actual standardization work. The slow rise reflects some theatricalization as the standard deepens: more elaborate etymological nationalism, more explicit state campaigns for 'pure Turkish,' more ceremonial defense of the standard against perceived threats. But the core remains functional.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence between beneficiaries and victims. The state bureaucracy (institutional/arbitrage) sees pure coordination: a technical solution to multilingual administration. The educational establishment (organized/arbitrage) sees coordination with secondary institutional benefits: enabling pedagogy and credential pathways. The rural dialect speaker (powerless/trapped) sees extraction without compensation: literacy requires abandoning local speech norms, with no recognition of local written form and no exit option. The minority script community (powerless/identity_locked) sees identity erasure: they could learn Turkish orthography but cannot do so without abandoning constitutive identity anchored in competing script tradition. The analytical observer sees the full structure: the benign coordination story told by beneficiaries is causally identical to the suppression story told by victims. The same mechanism (monolingual standardization) produces administrative order and script erasure simultaneously — they are not separate effects but aspects of the same constraint. The perspective gap reveals why beneficiaries and victims have such different classifications: beneficiaries experience only the coordination benefits; victims experience only the extraction and suppression. Neither side is lying about their experience; they are describing different causal flows through the same constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's power level, exit options, and relationship to the constraint flow. State bureaucracy: high power + arbitrage exit + clear beneficiary status → d ≈ 0.0 (full beneficiary, maximum agency, zero experienced extraction). Educational establishment: moderate power + arbitrage exit + secondary beneficiary → d ≈ 0.2 (net beneficiary despite some institutional costs from curriculum constraints). Rural dialect speaker: powerless + trapped exit + victim status → d ≈ 1.0 (full target, zero agency, maximum experienced extraction). Minority script community: powerless + identity_locked exit + victim status → d ≈ 0.95 (nearly full target; identity lock prevents cognitive switch even though structural barriers to learning Turkish are lower than for rural speakers; the cognitive inability to exercise structural mobility effectively traps them). Urban middle-class learner: moderate power + constrained exit + mixed beneficiary/victim → d ≈ 0.55 (symmetric: benefits from state integration but costs in learning time and opportunity cost). The engine's f(d) function then scales these raw directional values by scope and power to produce effective extractiveness (χ). For the trapped/identity-locked agents at large scope (national), χ is amplified: the constraint affects everyone in the national territory simultaneously, and powerless agents experience the highest multiplier. For beneficiaries with arbitrage exit and high power, χ becomes negative (subsidy): the state is subsidizing bureaucratic capacity by imposing the standard. This directionality structure explains why the same constraint classifies as Rope for state bureaucracy and Snare for minorities — the mathematical structure of directionality computation reveals that there is no single 'true' extraction value, only perspectival experiences that vary with position.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolves by recognizing that the constraint's mandate (provide reliable written standards for multilingual administration) remains live, but its implementation (monolingual standardization requiring suppression of alternatives) is contestable. The coordination function is real: Turkish-speaking polities do need standardized writing systems. The extraction mechanism is also real: achieving that standard through monolingual enforcement extracts costs from minorities. The constraint is not mandatorphy-resolved (mandate still justified) but contains an internal structural tension that manifests as perspectival disagreement. A mandatrophy resolution would require either (a) abandoning the standardization goal (mandate dies), which would create coordination problems; or (b) reforming the implementation to allow recognized multilingual alternatives (mandate persists, extraction mechanism changes). The constraint as currently authored represents the scenario where (b) has not occurred: monolingual standardization persists as the chosen implementation despite creating extraction. This is not a natural or inevitable constraint but a policy choice that could be changed without losing the coordination benefits — a fact that becomes clear from the Tangled Rope classification at the analytical level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_assimilation_intent,
    'Is the orthographic standard primarily a solution to multilingual administrative coordination, or primarily an assimilation mechanism targeting linguistic minorities?',
    'Historical analysis of adoption mechanisms: Did the standard emerge from inter-ethnic negotiation (coordination narrative) or state decree unilaterally imposed (extraction narrative)? What proportion of resistance came from administrations seeking consistency vs. from minorities refusing erasure?',
    'If coordination-primary: classification toward Rope from all perspectives; suppression drops as minorities gain agency in standard-setting. If assimilation-primary: classification locks into Snare for minorities; suppression is intentional, not administrative byproduct. The constraint''s type pivots on which narrative is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_assimilation_intent, empirical, 'Whether the orthographic standard is coordination mechanism or assimilation device').

omega_variable(
    dialect_legitimacy_recognition,
    'What proportion of the suppression effect would disappear if regional dialects were recognized as legitimate written forms alongside the standard?',
    'Comparative study: regions with dual-script recognition policies (e.g., minority language co-official status); measurement of literacy participation, dialect retention, and school enrollment in bilingual vs. monolingual-standard contexts.',
    'If substantial recognition would reduce suppression: suppression is not inherent to script standardization but to monolingual enforcement — classification flips toward Rope (coordination with recognized alternatives). If recognition would not meaningfully change literacy access: suppression reflects economic/geographic barriers, not enforcement — classification toward Rope even without recognition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dialect_legitimacy_recognition, empirical, 'Suppression reduction through regional script recognition').

omega_variable(
    identity_lock_reversibility,
    'For speakers of minority scripts (Kurdish, Laz, Armenian communities), is the identity lock to competing orthography genuinely constitutive of identity, or is it learned preference that would shift under different state recognition policies?',
    'Ethnographic and linguistic study: Do identity-locked speakers maintain script preference after exposure to recognized alternatives in low-coercion contexts? Does explicit state recognition of minority scripts produce measurable shifts in identity-framing among second and third generations?',
    'If genuinely constitutive: identity_locked is the correct exit option; suppression operates at the deepest level (identity erasure). If learned preference: exit_options should shift to constrained (external barriers are primary); suppression is high but potentially reversible through policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Whether minority-script identity lock is constitutive or policy-dependent').

omega_variable(
    standardization_necessity,
    'Could Turkish-speaking administrative systems function with multiple recognized scripts (digraphia/diglossia), or is monolingual standardization technically necessary for large-scale bureaucracy and education?',
    'Comparative institutional analysis: How do multilingual states (India, Switzerland, Belgium) handle script diversity in administration and education? What are the transaction costs of multiple standards vs. the suppression costs of monolingual enforcement?',
    'If multiple scripts are technically feasible: monolingual standardization is extractive choice, not necessity — constraint reclassifies toward Snare/pure extraction for minorities. If monolingual standardization is technically necessary for large-scale systems: constraint is closer to Mountain/natural limit — suppression is side effect of coordination, not intentional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_necessity, empirical, 'Technical necessity of monolingual script standardization').

omega_variable(
    theater_ratio_interpretation,
    'Does the Language Academy''s (TDK) low theater ratio (0.38) indicate genuine functional standardization, or indicate that the functional enforcement mechanism has simply migrated to schools and bureaucracy, leaving the academy with mostly ornamental authority?',
    'Measurement of actual enforcement: What proportion of spelling disputes are resolved by TDK pronouncements vs. schoolbook adoption vs. bureaucratic precedent? Track Academy-recommended spellings against actual usage in state documents and published materials over time.',
    'If Academy is functionally central: theater ratio accurately reflects low performative content. If enforcement has migrated: theater ratio underestimates total performativity (the whole system is theatrical authority maintenance). Affects piton vs. rope classification for standardizing authorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Whether Language Academy theater ratio reflects actual functional migration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel_flat_control, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthography_theater_t0, orthographic_kernel_flat_control, theater_ratio, 0, 0.28).
narrative_ontology:measurement(orthography_theater_t30, orthographic_kernel_flat_control, theater_ratio, 30, 0.32).
narrative_ontology:measurement(orthography_theater_t60, orthographic_kernel_flat_control, theater_ratio, 60, 0.38).
narrative_ontology:measurement(orthography_theater_t90, orthographic_kernel_flat_control, theater_ratio, 90, 0.41).

% Extraction over time
narrative_ontology:measurement(orthography_extract_t0, orthographic_kernel_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orthography_extract_t30, orthographic_kernel_flat_control, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(orthography_extract_t60, orthographic_kernel_flat_control, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(orthography_extract_t90, orthographic_kernel_flat_control, base_extractiveness, 90, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(orthography_suppress_t0, orthographic_kernel_flat_control, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(orthography_suppress_t30, orthographic_kernel_flat_control, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(orthography_suppress_t60, orthographic_kernel_flat_control, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(orthography_suppress_t90, orthographic_kernel_flat_control, suppression_requirement, 90, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel_flat_control, information_standard).
narrative_ontology:boltzmann_floor_override(orthographic_kernel_flat_control, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel_flat_control, turkish_language_nationalism).
narrative_ontology:affects_constraint(orthographic_kernel_flat_control, kurdish_script_suppression).
narrative_ontology:affects_constraint(orthographic_kernel_flat_control, bureaucratic_state_capacity).
narrative_ontology:affects_constraint(orthographic_kernel_flat_control, national_identity_construction).

% DUAL FORMULATION NOTE:
% Orthographic standardization affects multiple downstream constraints in the state formation system. The coordination benefits (bureaucratic capacity, legal administration) flow primarily to institutional actors; extraction costs (script suppression, dialect erasure) flow to minorities. Future reading decomposition would separate nationalist standardization reading from multilingual accommodation reading; both would share the same base_properties but would differ in claimed_type and beneficiary/victim framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
