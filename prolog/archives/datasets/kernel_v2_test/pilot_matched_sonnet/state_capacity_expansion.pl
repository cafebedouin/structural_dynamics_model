% ============================================================================
% CONSTRAINT STORY: state_capacity_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_capacity_expansion, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: state_capacity_expansion
 *   human_readable: Turkish Alphabet Reform as State Capacity Expansion Mechanism
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish alphabet reform replaced Arabic script with Latin
 *   alphabet as part of Kemalist modernization. Officially framed as
 *   technical improvement for literacy expansion, the reform simultaneously
 *   served as mechanism for state capacity expansion: standardized
 *   orthography enabled centralized census, taxation, and administrative
 *   control while destroying regional elites' informational advantage. The
 *   constraint exhibits tangled_rope structure because genuine coordination
 *   function (administrative standardization, literacy access for previously
 *   excluded populations) coexists with substantial extraction (cultural
 *   rupture, suppression of regional autonomy, destruction of Ottoman textual
 *   heritage access). The reform's extractiveness peaked during mandatory
 *   enforcement period (1930-1933) then declined as new generation normalized
 *   Latin literacy, but never reached zero because the cultural rupture is
 *   permanent. Theater ratio remains low (0.35-0.42) because the functional
 *   coordination is real: Latin script genuinely enabled administrative
 *   standardization and literacy expansion, even as it extracted from Ottoman
 *   cultural continuity.
 *
 * KEY AGENTS:
 *   - Central State Bureaucracy: Primary beneficiary (institutional/arbitrage) — gains administrative legibility, standardized documentation, expanded recruitment pool through literacy requirements
 *   - Ottoman Literate Class: Primary victim (powerless/trapped) — lifetime literacy investment rendered obsolete, no exit from cultural identity, maximum extraction
 *   - Regional Administrative Elites: Secondary victim (moderate/constrained) — must adopt reform to maintain positions but lose autonomy as standardization enables central oversight
 *   - Religious Educational Institutions: Secondary victim (moderate/constrained) — lose authority over textual transmission, face pressure to secularize curriculum
 *   - New Literate Class: Secondary beneficiary (moderate/mobile) — rural and working-class populations gaining literacy access for first time, no prior investment in Arabic script
 *   - Kemalist Modernization Coalition: Organized beneficiary (organized/mobile) — political actors implementing reform as transitional mechanism toward broader modernization
 *   - Pre-Reform Cultural Transmission: Abstract victim (powerless/trapped) — Ottoman textual heritage becomes inaccessible to post-reform generations without specialized training
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_capacity_expansion, 0.58).
domain_priors:suppression_score(state_capacity_expansion, 0.72).
domain_priors:theater_ratio(state_capacity_expansion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_capacity_expansion, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_capacity_expansion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_capacity_expansion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_capacity_expansion, tangled_rope).
narrative_ontology:human_readable(state_capacity_expansion, "Turkish Alphabet Reform as State Capacity Expansion Mechanism").
narrative_ontology:topic_domain(state_capacity_expansion, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(state_capacity_expansion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_capacity_expansion, '69069be9-58e2-4e04-b5fe-6a9b23e3cfe0').
narrative_ontology:cs_kernel_codification('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', formalized).
narrative_ontology:cs_authority_grounding('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', lineage).
narrative_ontology:cs_interpretation_layer_present('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0').
narrative_ontology:cs_reading_relation('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', state_capacity_expansion__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', state_capacity_expansion__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', foundational, orthographic_modernization_enables_progress).
narrative_ontology:cs_axiom_status(orthographic_modernization_enables_progress, holdable).
narrative_ontology:cs_axiom_grounding('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', orthographic_modernization_enables_progress, instrumental).
narrative_ontology:cs_axiom('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', secondary, linguistic_identity_preserved_across_scripts).
narrative_ontology:cs_axiom_status(linguistic_identity_preserved_across_scripts, holdable).
narrative_ontology:cs_axiom_grounding('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', linguistic_identity_preserved_across_scripts, conventional).
narrative_ontology:cs_reference_frame('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', kemalist_modernization_framework).
narrative_ontology:cs_drift_state('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('69069be9-58e2-4e04-b5fe-6a9b23e3cfe0', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_capacity_expansion, central_state_bureaucracy).
narrative_ontology:constraint_beneficiary(state_capacity_expansion, new_literate_class).
narrative_ontology:constraint_beneficiary(state_capacity_expansion, kemalist_modernization_coalition).
narrative_ontology:constraint_victim(state_capacity_expansion, regional_autonomy).
narrative_ontology:constraint_victim(state_capacity_expansion, ottoman_literate_class).
narrative_ontology:constraint_victim(state_capacity_expansion, religious_educational_institutions).
narrative_ontology:constraint_victim(state_capacity_expansion, pre_reform_cultural_transmission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_capacity_expansion, regional_administrative_elites).
narrative_ontology:constraint_vindicates(state_capacity_expansion, modernization_through_westernization).
narrative_ontology:constraint_vindicates(state_capacity_expansion, state_legibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the orthographic standard through legislative mandate and educational policy. Benefits from expanded administrative reach: standardized literacy requirements filter bureaucratic recruitment, unified documentation enables centralized oversight of regional administration, census and taxation data quality improves measurably. Can exit by abandoning reform (political cost but structurally possible during early implementation). Experiences reform as solving genuine coordination problem while expanding institutional power.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, central_state_bureaucracy, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_capacity_expansion, central_state_bureaucracy, beneficiary).

% Lifetime investment in Arabic script literacy rendered obsolete overnight. Cannot access new administrative positions without complete re-education in Latin script. Cannot preserve professional status as scribes, clerks, religious scholars. Cannot exit cultural identity that is constituted through Ottoman textual tradition. Bears maximum cost of reform with no coordination benefit — the standardization that benefits state administration destroys their human capital.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, ottoman_literate_class, payer,
    powerless, biographical, trapped, national).

% Must adopt Latin script to maintain administrative positions, but lose autonomy in local governance as standardization enables central oversight. Previously held informational advantage: central state could not easily audit local records or standardize reporting across regions. Reform destroys this advantage — central bureaucracy can now read local documents, compare across regions, enforce uniform procedures. Can exit by leaving administration (high career cost) but not trapped. Experiences both coordination (unified communication with central state) and extraction (loss of discretion).
narrative_ontology:constraint_stakeholder(state_capacity_expansion, regional_administrative_elites, payer,
    moderate, biographical, constrained, regional).

% Madrasas and Quranic schools face dual pressure: must teach Latin script to remain relevant for students' economic futures, but lose authority over textual transmission and religious education. Ottoman religious texts become inaccessible without specialized training. Islamic scholarly tradition depends on Arabic script for Quranic study. Can exit by closing or converting to secular curriculum, but at high cost to institutional identity and community role. Experiences coordination function (students gain literacy for employment) alongside extraction (loss of religious textual authority).
narrative_ontology:constraint_stakeholder(state_capacity_expansion, religious_educational_institutions, payer,
    moderate, generational, constrained, local).

% Rural and working-class populations gaining literacy for the first time through Latin script education. No prior investment in Arabic script means no extraction from script change itself. Gains access to administrative employment, legal documentation, national political discourse. Literacy is portable skill — can use it across regions and sectors. Experiences reform as pure coordination: the standardization that enables state administrative reach also enables their economic mobility.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, new_literate_class, beneficiary,
    moderate, biographical, mobile, national).

% Organized political actors implementing alphabet reform as part of broader modernization program. See script change as transitional mechanism: initial coercive enforcement necessary to overcome resistance, but can relax once literacy normalizes within one generation. Benefit from expanded state capacity and from political consolidation (reform weakens Ottoman-era elites who might challenge Kemalist authority). Can exit by abandoning reform during early implementation (political cost but structurally possible). Frame extraction as temporary cost of necessary transformation.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, kemalist_modernization_coalition, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_capacity_expansion, kemalist_modernization_coalition, beneficiary).

% Abstract collective good: the capacity to transmit Ottoman textual heritage across generations. Not a real-world actor but included for narrative completeness. Ottoman literature, historical documents, religious commentaries become inaccessible to post-reform generations without specialized training in Arabic script. The rupture is permanent — even if individuals later learn Arabic script, the population-level continuity is severed. This entry is marked agent=false and is excluded from beneficiary/victim derivation.
narrative_ontology:constraint_stakeholder(state_capacity_expansion, pre_reform_cultural_transmission, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(state_capacity_expansion, pre_reform_cultural_transmission).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized orthography for administrative communication, census data collection, taxation records, and civil service documentation. Solves genuine collective-action problem: pre-reform Turkey had no unified literacy standard, regional variation in Arabic script usage created administrative friction, Ottoman script's complexity limited literacy expansion.
% TRANSFER_FUNCTION: The reform transfers administrative capacity from regional elites to central state (information flows from periphery to center become standardized and auditable), transfers cultural authority from Ottoman/Islamic textual tradition to Kemalist modernization narrative (legitimacy flows from historical continuity to revolutionary transformation), and transfers literacy access from Ottoman literate class to new literate class (human capital flows from Arabic-script-trained elites to Latin-script-trained masses).
% ABSENT_VOICES: Ottoman literary and religious scholars who would object to cultural rupture but were excluded from reform deliberations. Kurdish and Arabic-speaking minorities whose languages also used Arabic script and who would object to linguistic homogenization. Rural populations who bore literacy transition costs but had no representation in urban Kemalist coalition. These groups were not in the room during 1928 parliamentary debates — the reform was imposed by Kemalist political elite without consultation of affected populations.
% DISAPPEARANCE_RATIONALE: If the alphabet reform disappeared overnight (Latin script reverted to Arabic script), the world would rearrange substantially: central state administrative capacity would degrade (census and taxation data quality would decline, inter-provincial communication would lose standardization), civil service recruitment would shift (literacy requirements would change, different populations would have access), regional autonomy would increase (central oversight would weaken as informational advantage returns to local elites), and cultural transmission would reconnect with Ottoman textual heritage (pre-reform literature would become accessible again). The arrangements of state capacity, administrative hierarchy, literacy access, and cultural authority all depend on the orthographic standard.
% FOUNDING_PROBLEM: Administrative fragmentation and low literacy rates in early Turkish Republic. Pre-reform Turkey (1923-1928) faced genuine coordination failures: no standardized census (population estimates varied by millions), taxation was inefficient (revenue collection per capita was low and regionally inconsistent), civil service recruitment was limited by low literacy rates (~10% of population), and regional administrative elites operated with substantial autonomy from central state due to informational barriers. The founding problem was real: the state could not govern effectively without administrative legibility.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between two positions: (1) DEAD — administrative technology has advanced such that orthographic standardization is no longer necessary for state capacity (digital records, multilingual interfaces, automated translation reduce need for single-script uniformity). This position is held by some linguistic scholars and minority-rights advocates. (2) LIVE — standardized literacy remains necessary for administrative efficiency and national integration, even with modern technology. This position is held by Turkish state educational apparatus and most administrative scholars. Corroboration: Historical data on census quality and taxation efficiency (from Turkish Statistical Institute archives) confirms that administrative capacity did improve post-reform, supporting that the founding problem was real. But comparative analysis of other post-Ottoman states (Egypt, Syria retained Arabic script and also achieved administrative modernization) suggests the problem could have been solved without mandatory script replacement, supporting that the founding problem is now dead or was never as severe as claimed. The contest is between 'was script standardization necessary then?' (empirical) and 'is it necessary now?' (contested).
narrative_ontology:disappearance_verdict(state_capacity_expansion, world_rearranges).
narrative_ontology:founding_problem_status(state_capacity_expansion, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OTTOMAN LITERATE CLASS (SNARE) — Lifetime investment in Arabic script literacy rendered obsolete overnight. No exit: cannot unlearn cultural identity, cannot access new system without complete re-education, cannot preserve professional status. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(state_capacity_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL ADMINISTRATIVE ELITES (TANGLED ROPE) — Must adopt Latin script to maintain administrative positions, but lose autonomy in local governance as standardization enables central oversight. Genuine coordination function (unified administrative communication) coexists with extraction (loss of regional discretion, cultural authority). High cost to exit (career loss) but not trapped.
constraint_indexing:constraint_classification(state_capacity_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL STATE BUREAUCRACY (ROPE) — Primary beneficiary. Alphabet reform solves genuine coordination problem (administrative standardization, census legibility, taxation efficiency) while expanding state reach. Experiences constraint as pure coordination: literacy requirements filter bureaucratic recruitment, standardized documentation enables centralized control. Net beneficiary with full exit options.
constraint_indexing:constraint_classification(state_capacity_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: KEMALIST MODERNIZATION COALITION (SCAFFOLD) — Organized political actors see alphabet reform as transitional mechanism toward full modernization. The script change is temporary support for broader transformation: once literacy rates rise and administrative capacity stabilizes, the coercive enforcement can relax. Sunset logic: initial suppression (mandatory adoption, criminalization of Arabic script in public documents) gives way to normalized practice within one generation.
constraint_indexing:constraint_classification(state_capacity_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RELIGIOUS EDUCATIONAL INSTITUTIONS (TANGLED ROPE) — Madrasas and Quranic schools face dual pressure: must teach Latin script to remain relevant for students' economic futures (coordination function) while losing authority over textual transmission and religious education (extraction). Can exit by closing or converting to secular curriculum, but at high cost to institutional identity and community role.
constraint_indexing:constraint_classification(state_capacity_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: NEW LITERATE CLASS (ROPE) — Rural and working-class populations gaining literacy for the first time through Latin script. Experiences reform as pure coordination: access to administrative employment, legal documentation, national discourse. No prior investment in Arabic script means no extraction from script change itself. Mobile exit options: literacy is portable skill.
constraint_indexing:constraint_classification(state_capacity_expansion, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, alphabet reform exhibits both genuine coordination function (administrative standardization, literacy expansion, economic integration) and substantial extraction (cultural rupture, suppression of regional autonomy, destruction of Ottoman textual heritage). The coordination function is real but could have been achieved through less extractive means (gradual bilingual transition, preservation of Arabic script for religious/historical texts). Analytical classification: tangled_rope, not rope, because the extraction is structural rather than incidental.
constraint_indexing:constraint_classification(state_capacity_expansion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_capacity_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_capacity_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_capacity_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_capacity_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_capacity_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. The reform extracts from Ottoman literate class (lifetime investment loss), regional autonomy (informational advantage destruction), and cultural transmission (textual heritage access severed). But extraction is not maximal because genuine coordination function exists: literacy rates did expand (from ~10% in 1928 to ~40% by 1950), administrative capacity did increase, economic integration did improve. The value reflects that roughly 40% of the constraint's operation is extractive overhead beyond coordination necessity. Measurements show extractiveness peaked at 0.72 during mandatory enforcement (1933) then declined to 0.52 by 1958 as normalization reduced active coercion, settling at 0.58 as permanent cultural rupture cost. Suppression (0.72): High. Arabic script was criminalized in public documents, Ottoman-script publications banned, civil service employment required Latin literacy, educational curriculum mandated Latin-only instruction. Regional elites could not maintain administrative positions without adopting reform. Religious institutions faced closure if they refused to teach Latin script. Suppression measurements show enforcement intensification from 0.55 (1928, initial legislation) to 0.88 (1933, peak enforcement) then gradual decay to 0.48 (1958) as the reform normalized and active coercion became unnecessary. Theater ratio (0.35): Low-moderate. The coordination function is substantially real: standardized orthography did enable census improvements (population data quality increased measurably 1928-1935), taxation efficiency (revenue collection per capita increased), administrative communication (inter-provincial correspondence standardized). Theater exists in the modernization rhetoric (claims that Arabic script was inherently backward, that Latin script was necessary for science/technology) but the functional gains are genuine. Theater ratio rises slightly over interval (0.25 to 0.42) as the revolutionary justification becomes more performative while the functional coordination persists.
 *
 * PERSPECTIVAL GAP:
 *   The Ottoman literate class sees pure extraction (snare) because they bear maximum cost with no coordination benefit — their lifetime literacy investment is destroyed and they cannot exit their cultural identity. Regional elites see tangled_rope because they experience both coordination (unified administrative communication) and extraction (loss of autonomy). Central bureaucracy sees rope because they are net beneficiaries — the reform solves their genuine coordination problem (administrative standardization) while expanding their power. New literate class sees rope because they gain literacy access without bearing cultural rupture cost. Kemalist coalition sees scaffold because they frame the coercion as temporary — necessary during transition, relaxable once literacy normalizes. Religious institutions see tangled_rope because they must participate in the new system (coordination) while losing textual authority (extraction). The analytical observer sees tangled_rope because both functions are structurally real: the coordination gains (literacy expansion, administrative capacity) are genuine and measurable, but the extraction (cultural rupture, regional autonomy suppression) is also genuine and permanent. The constraint is not a false summit — the coordination function is real — but neither is it pure coordination, because the extraction exceeds what was necessary to achieve the coordination gains.
 *
 * DIRECTIONALITY LOGIC:
 *   Central state bureaucracy is primary beneficiary with arbitrage exit options — derives d near 0.0, experiences negative or minimal effective extraction (the constraint subsidizes this agent). Ottoman literate class is primary victim with trapped exit — derives d near 1.0, experiences maximum effective extraction. Regional administrative elites are victims with constrained exit (high cost but possible) — derives d around 0.6-0.7, experiences substantial but not maximal extraction. New literate class is beneficiary with mobile exit — derives d near 0.2, experiences low extraction or net benefit. Religious institutions are victims with constrained exit — derives d around 0.65, experiences substantial extraction. Kemalist coalition is beneficiary with mobile exit — derives d near 0.15, experiences low extraction. The perspectival gap is structural: beneficiaries see coordination (rope/scaffold), victims see extraction (snare/tangled_rope), analytical observer sees both (tangled_rope). No directionality overrides needed — the beneficiary/victim declarations plus exit options produce accurate d values for all agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled_rope classification is stable and non-collapsing: the coordination function (administrative standardization, literacy expansion) is genuine and persists across the 30-year interval, while the extraction (cultural rupture, regional autonomy suppression) is also genuine and persists. The reform is neither pure coordination (rope) nor pure extraction (snare) — it is structurally both. The perspectival gap is not measurement error but reflects real differences in structural position: agents who benefit from standardization see coordination, agents who bear cultural rupture cost see extraction, and both perceptions are correct from their respective positions. The analytical classification (tangled_rope) is not a compromise or average but a recognition that the constraint has two simultaneous functions that cannot be separated: you cannot get the coordination gains without the extraction costs, because the extraction (destroying regional informational advantage, severing Ottoman textual continuity) is the mechanism that enables the coordination (centralized legibility, standardized administration). This is the defining feature of tangled_rope: the coordination and extraction are structurally inseparable, not merely co-occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_expansion_counterfactual,
    'Would literacy rates have expanded comparably under gradual bilingual reform rather than mandatory script replacement?',
    'Comparative analysis of literacy expansion in other post-Ottoman states (e.g., Egypt, Syria) that retained Arabic script; econometric modeling of literacy determinants controlling for script system',
    'If bilingual path achieves similar literacy gains: extraction was unnecessary, constraint reclassifies toward snare from more perspectives. If mandatory replacement was necessary: coordination function dominates, constraint remains tangled_rope or shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_expansion_counterfactual, empirical, 'Whether literacy expansion required mandatory script replacement').

omega_variable(
    state_legibility_necessity,
    'Was standardized orthography necessary for administrative capacity expansion, or was it sufficient to standardize administrative language while allowing script variation?',
    'Historical analysis of administrative effectiveness in multilingual/multiscript empires (Austro-Hungarian, Russian, Qing); examination of whether census/taxation quality improvements correlate with script standardization or with other administrative reforms (civil service professionalization, telegraph infrastructure, statistical bureaus)',
    'If script standardization was necessary: coordination function is genuine, tangled_rope classification holds. If administrative capacity could expand without script control: the script reform was primarily extractive (cultural control), constraint shifts toward snare from analytical perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_legibility_necessity, empirical, 'Whether administrative capacity required orthographic standardization').

omega_variable(
    cultural_rupture_intentionality,
    'Was the cultural rupture (severing access to Ottoman textual heritage) an intended feature or an unintended side effect of the reform?',
    'Archival analysis of Kemalist internal documents, parliamentary debates, educational policy directives; examination of whether preservation mechanisms (translation programs, dual-script archives) were proposed and rejected or never considered',
    'If rupture was intended: extraction was deliberate, victim set expands to include entire pre-reform cultural transmission system. If rupture was unintended: extraction was structural but not malicious, omega shifts from preference to empirical type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_rupture_intentionality, preference, 'Whether cultural rupture was intended or incidental').

omega_variable(
    regional_autonomy_suppression_mechanism,
    'Did alphabet reform suppress regional autonomy directly (by destroying local administrative capacity) or indirectly (by enabling central state surveillance)?',
    'Analysis of regional governance structures pre/post reform; examination of whether centralization occurred through direct administrative takeover or through information asymmetry reduction (central state could now read local records, audit local officials, standardize reporting)',
    'If suppression was direct: extraction mechanism is coercive displacement. If suppression was indirect: extraction mechanism is surveillance-enabled control, which has different persistence dynamics (harder to reverse once information infrastructure is built).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_autonomy_suppression_mechanism, empirical, 'Mechanism of regional autonomy suppression').

omega_variable(
    cs_framing_under_determination,
    'Is the orthographic kernel best framed as the script system itself (Arabic vs Latin alphabet) or as the legitimacy claim layered above it (Ottoman continuity vs Kemalist rupture)?',
    'Examination of which framing better predicts downstream classification divergence: if script-as-kernel, readings differ on technical efficiency and literacy access; if legitimacy-claim-as-kernel, readings differ on cultural authority and historical continuity. Test by checking which framing produces cleaner separation of beneficiary/victim sets.',
    'If script-as-kernel: the constraint is primarily about administrative technology, coordination function dominates. If legitimacy-claim-as-kernel: the constraint is about authority structure, extraction through cultural control dominates. Current authoring uses script-as-kernel; alternative framing would shift analytical classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Whether kernel is script system or legitimacy claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_capacity_expansion, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1928, state_capacity_expansion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_1933, state_capacity_expansion, theater_ratio, 5, 0.32).
narrative_ontology:measurement(theater_1938, state_capacity_expansion, theater_ratio, 10, 0.35).
narrative_ontology:measurement(theater_1948, state_capacity_expansion, theater_ratio, 20, 0.38).
narrative_ontology:measurement(theater_1958, state_capacity_expansion, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(extract_1928, state_capacity_expansion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(extract_1930, state_capacity_expansion, base_extractiveness, 2, 0.68).
narrative_ontology:measurement(extract_1933, state_capacity_expansion, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(extract_1938, state_capacity_expansion, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(extract_1948, state_capacity_expansion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(extract_1958, state_capacity_expansion, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1928, state_capacity_expansion, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(suppress_1930, state_capacity_expansion, suppression_requirement, 2, 0.85).
narrative_ontology:measurement(suppress_1933, state_capacity_expansion, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(suppress_1938, state_capacity_expansion, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(suppress_1948, state_capacity_expansion, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(suppress_1958, state_capacity_expansion, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_capacity_expansion, enforcement_mechanism).
narrative_ontology:affects_constraint(state_capacity_expansion, census_legibility_expansion).
narrative_ontology:affects_constraint(state_capacity_expansion, taxation_standardization).
narrative_ontology:affects_constraint(state_capacity_expansion, civil_service_professionalization).

% DUAL FORMULATION NOTE:
% The alphabet reform is upstream of multiple administrative capacity constraints. Census legibility, taxation standardization, and civil service professionalization all depend on standardized literacy, but each has its own extractiveness value reflecting its specific coordination/extraction balance. The alphabet reform's extractiveness (0.58) reflects the script change itself; downstream constraints have their own values reflecting their specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
