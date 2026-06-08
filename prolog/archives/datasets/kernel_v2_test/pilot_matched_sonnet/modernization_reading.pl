% ============================================================================
% CONSTRAINT STORY: modernization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_modernization_reading, []).

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
 *   constraint_id: modernization_reading
 *   human_readable: Latin Script as Modernization Instrument (Modernization Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish script reform, viewed through the modernization reading,
 *   frames the Latin alphabet adoption as a technical instrument for
 *   achieving mass literacy and scientific modernization while preserving
 *   Turkish linguistic identity distinct from Arabic and Persian influences.
 *   This reading emphasizes the coordination function: standardized
 *   orthography enabled universal primary education, simplified printing
 *   technology, facilitated adoption of European technical vocabulary, and
 *   created orthographic distance from Ottoman elite culture. The constraint
 *   exhibits declining extractiveness over the 30-year interval (0.68 → 0.42)
 *   as the initial rupture costs (destroyed literacy capital, institutional
 *   disruption) amortized across the expanding literate population.
 *   Suppression requirement similarly declined (0.85 → 0.58) as generational
 *   turnover reduced the population with Arabic-script literacy, making
 *   enforcement less necessary. Theater ratio remained low but increased
 *   modestly (0.25 → 0.35) as the reform's modernization narrative became
 *   more performative relative to its functional literacy gains — by 1958,
 *   the 'Latin script = modernity' equation was more ideological assertion
 *   than technical necessity. The modernization reading is one of three
 *   structural interpretations of the same orthographic kernel; the
 *   continuity_reading emphasizes preservation of linguistic heritage through
 *   reformed Arabic script, while the rupture_reading emphasizes the
 *   deliberate severing of Ottoman cultural continuity. All three readings
 *   describe the same 1928 reform but with different beneficiary structures,
 *   different extraction profiles, and different naturalness claims.
 *
 * KEY AGENTS:
 *   - Kemalist State Bureaucracy: Primary beneficiary (institutional/arbitrage) — captures authority over knowledge production, standardizes administrative communication, consolidates state control over education
 *   - New Literate Class: Secondary beneficiary (organized/mobile) — gains access to literacy and technical education; sees reform as temporary disruption with clear sunset
 *   - Technical Education Sector: Mixed position (institutional/constrained) — benefits from European vocabulary alignment but loses Ottoman technical corpus
 *   - Arabic-Script Literate Population: Primary victim (powerless/trapped) — overnight functional illiteracy, destroyed literacy capital, no exit from new regime
 *   - Religious Education Institutions: Secondary victim (moderate/constrained) — must adopt Latin script to maintain legitimacy while preserving religious content
 *   - Ottoman Literary Tradition: Abstract victim (powerless/trapped) — cultural corpus rendered inaccessible to new generations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(modernization_reading, 0.42).
domain_priors:suppression_score(modernization_reading, 0.58).
domain_priors:theater_ratio(modernization_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(modernization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(modernization_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(modernization_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(modernization_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(modernization_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(modernization_reading, tangled_rope).
narrative_ontology:human_readable(modernization_reading, "Latin Script as Modernization Instrument (Modernization Reading)").
narrative_ontology:topic_domain(modernization_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(modernization_reading, 'cf3e02a0-23eb-4767-8156-d07643e70216').
narrative_ontology:cs_kernel_codification('cf3e02a0-23eb-4767-8156-d07643e70216', formalized).
narrative_ontology:cs_authority_grounding('cf3e02a0-23eb-4767-8156-d07643e70216', lineage).
narrative_ontology:cs_interpretation_layer_present('cf3e02a0-23eb-4767-8156-d07643e70216').
narrative_ontology:cs_reading_relation('cf3e02a0-23eb-4767-8156-d07643e70216', modernization_reading__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('cf3e02a0-23eb-4767-8156-d07643e70216', modernization_reading__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('cf3e02a0-23eb-4767-8156-d07643e70216', foundational, script_determines_modernization_capacity).
narrative_ontology:cs_axiom_status(script_determines_modernization_capacity, holdable).
narrative_ontology:cs_axiom_grounding('cf3e02a0-23eb-4767-8156-d07643e70216', script_determines_modernization_capacity, empirically_contingent).
narrative_ontology:cs_axiom('cf3e02a0-23eb-4767-8156-d07643e70216', foundational, literacy_expansion_justifies_cultural_rupture).
narrative_ontology:cs_axiom_status(literacy_expansion_justifies_cultural_rupture, holdable).
narrative_ontology:cs_axiom_grounding('cf3e02a0-23eb-4767-8156-d07643e70216', literacy_expansion_justifies_cultural_rupture, instrumental).
narrative_ontology:cs_axiom('cf3e02a0-23eb-4767-8156-d07643e70216', secondary, orthographic_rationalization_enables_mass_education).
narrative_ontology:cs_axiom_status(orthographic_rationalization_enables_mass_education, holdable).
narrative_ontology:cs_axiom_grounding('cf3e02a0-23eb-4767-8156-d07643e70216', orthographic_rationalization_enables_mass_education, empirically_contingent).
narrative_ontology:cs_reference_frame('cf3e02a0-23eb-4767-8156-d07643e70216', ottoman_administrative_continuity).
narrative_ontology:cs_drift_state('cf3e02a0-23eb-4767-8156-d07643e70216', post_reform_generation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cf3e02a0-23eb-4767-8156-d07643e70216', '').
narrative_ontology:cs_kernel_id(modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(modernization_reading, kemalist_state_bureaucracy).
narrative_ontology:constraint_beneficiary(modernization_reading, new_literate_class).
narrative_ontology:constraint_beneficiary(modernization_reading, technical_education_sector).
narrative_ontology:constraint_victim(modernization_reading, arabic_script_literate_population).
narrative_ontology:constraint_victim(modernization_reading, religious_education_institutions).
narrative_ontology:constraint_victim(modernization_reading, ottoman_literary_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(modernization_reading, technical_education_sector).
narrative_ontology:constraint_vindicates(modernization_reading, script_determines_modernity).
narrative_ontology:constraint_vindicates(modernization_reading, orthographic_rationalization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the script reform. Captures authority over knowledge production, standardizes administrative communication, consolidates control over mass education. Experiences the reform as rationalization: solving the coordination problem of literacy expansion while building state capacity.
narrative_ontology:constraint_stakeholder(modernization_reading, kemalist_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains access to literacy and technical education through the Latin script system. Sees the reform as temporary disruption with clear endpoint: once their generation matures, the coordination function stabilizes and enforcement becomes unnecessary. Benefits from expanded educational infrastructure and career opportunities in the modernizing state.
narrative_ontology:constraint_stakeholder(modernization_reading, new_literate_class, beneficiary,
    organized, generational, mobile, national).

% Benefits from alignment with European technical vocabulary and international scientific communication, enabling faster adoption of modern technical knowledge. Simultaneously bears the cost of rupture with Ottoman technical literature — existing corpus must be retranslated or abandoned. Mixed position: coordination gains (access to Western resources) and extraction costs (loss of accumulated knowledge).
narrative_ontology:constraint_stakeholder(modernization_reading, technical_education_sector, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(modernization_reading, technical_education_sector, payer).

% Rendered functionally illiterate overnight by state decree. Existing literacy capital destroyed with no transition mechanism, no compensation, and no alternative. Cannot exit the new orthographic regime. Bears maximum extraction: loss of literacy, loss of access to cultural heritage, loss of social status tied to Ottoman literary culture.
narrative_ontology:constraint_stakeholder(modernization_reading, arabic_script_literate_population, payer,
    powerless, biographical, trapped, national).

% Constrained by state enforcement but embedded in the new educational infrastructure. Must adopt Latin script to maintain institutional legitimacy and access to state resources, while attempting to preserve religious content and pedagogical tradition. Loses Arabic-script pedagogical tradition and direct access to classical Islamic texts, but gains integration into the expanding state education system.
narrative_ontology:constraint_stakeholder(modernization_reading, religious_education_institutions, payer,
    moderate, biographical, constrained, national).

% Abstract cultural corpus rendered inaccessible to new generations. Not an agent but a collective good that bears extraction costs. The reform creates a generational rupture: post-1928 generations cannot read pre-1928 literature without specialized training, severing the transmission chain of Ottoman literary and intellectual culture.
narrative_ontology:constraint_stakeholder(modernization_reading, ottoman_literary_tradition, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(modernization_reading, ottoman_literary_tradition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reform solves the coordination problem of mass literacy expansion in a society with low baseline literacy (estimated 10-15% in 1928). Latin script simplifies printing technology, reduces character set complexity, and enables standardized orthography for universal primary education. It also coordinates adoption of European technical vocabulary and facilitates international scientific communication.
% TRANSFER_FUNCTION: The reform transfers authority over knowledge production from Ottoman-educated elites (Arabic-script literate, Persian and Arabic linguistic competence) to the Kemalist state bureaucracy and the new Latin-script educated class. It transfers literacy capital: destroys existing Arabic-script literacy while creating new Latin-script literacy. It transfers cultural access: severs new generations from Ottoman literary heritage while connecting them to European intellectual traditions.
% ABSENT_VOICES: The Arabic-script literate population — religious scholars, Ottoman-educated professionals, literary elites — would object but were systematically excluded from the 1928 decision. The Language Commission that designed the reform was dominated by Kemalist modernizers; traditionalist voices were not represented. The reform was implemented by executive decree with minimal legislative debate, foreclosing opposition. Post-1928, dissent was suppressed through legal prohibition of Arabic script in public communication and educational policy mandating Latin script exclusively.
% DISAPPEARANCE_RATIONALE: If the Latin script reform disappeared overnight, the Turkish state's administrative apparatus, educational system, and technical vocabulary would require fundamental reorganization. The new literate class's literacy capital would be disrupted. However, the rearrangement would be less severe than the original 1928 disruption because alternative literacy pathways (reformed Arabic script, parallel orthographic systems) could be implemented more gradually. The verdict is world_rearranges because substantial institutional arrangements depend on the Latin script standard, but the dependency is weaker than the modernization reading claims — other societies achieved comparable modernization with non-Latin scripts.
% FOUNDING_PROBLEM: The founding problem, as articulated by the modernization reading, was low literacy rates (10-15%) and the perceived barrier of Arabic script complexity to mass education. The Kemalist state diagnosed Ottoman elite culture's Arabic and Persian linguistic orientation as obstacle to Turkish national identity and technical modernization. The reform was framed as solving two problems simultaneously: enabling mass literacy through orthographic simplification, and creating cultural distance from Ottoman and Islamic heritage.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between readings. The modernization reading (this constraint) holds the problem is DEAD: mass literacy was achieved (90%+ by 1980s), and Latin script is now naturalized. The continuity reading holds the problem was MISDIAGNOSED: Arabic script complexity was not the barrier (other societies achieved mass literacy with Arabic script), and the reform solved a political problem (cultural rupture) while claiming to solve a technical one (literacy). The rupture reading holds the problem is LIVE but transformed: the founding problem was not literacy but political consolidation, and that problem persists in ongoing state management of linguistic and cultural identity. Corroboration sources: Kemalist state documents (1928-1938) articulate the literacy/modernization framing; Turkish Language Association historiography maintains this narrative. Critical historiography (post-1980s) challenges the technical necessity claim, citing comparative literacy data from Arabic-script societies and evidence of suppressed alternative reform proposals. No corroboration exists outside the beneficiary set (Kemalist state and its institutional descendants) for the claim that Arabic script was the primary literacy barrier.
narrative_ontology:disappearance_verdict(modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(modernization_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARABIC-SCRIPT LITERATE POPULATION (SNARE) — Overnight functional illiteracy imposed by state decree. Existing literacy capital destroyed with no transition mechanism. Cannot exit the new orthographic regime; bears full cost of the rupture. Maximum experienced extraction.
constraint_indexing:constraint_classification(modernization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS EDUCATION INSTITUTIONS (TANGLED ROPE) — Constrained by state enforcement but also embedded in the new educational infrastructure. Must adopt Latin script to maintain institutional legitimacy while preserving religious content. Mixed coordination (access to state resources) and extraction (loss of Arabic-script pedagogical tradition).
constraint_indexing:constraint_classification(modernization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KEMALIST STATE BUREAUCRACY (ROPE) — Primary beneficiary. Latin script enables standardized mass education, technical vocabulary adoption, and administrative rationalization. Experiences the constraint as pure coordination: solving the genuine problem of literacy expansion while consolidating state authority over knowledge production.
constraint_indexing:constraint_classification(modernization_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NEW LITERATE CLASS (SCAFFOLD) — Organized beneficiaries of the literacy expansion. See the orthographic transition as temporary disruption with clear endpoint: once the generation educated in Latin script matures, the coordination function stabilizes and extraction mechanisms (enforcement, suppression of alternatives) become unnecessary. Sunset logic: 20-30 years for full generational turnover.
constraint_indexing:constraint_classification(modernization_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TECHNICAL EDUCATION SECTOR (TANGLED ROPE) — Benefits from alignment with European technical vocabulary and international scientific communication, but constrained by the rupture with Ottoman technical literature and the need to retranslate existing knowledge. Mixed coordination (access to Western technical resources) and extraction (loss of accumulated Ottoman technical corpus).
constraint_indexing:constraint_classification(modernization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the Latin script reform solves a genuine coordination problem (literacy expansion, technical vocabulary standardization) while imposing substantial extraction (cultural rupture, destruction of existing literacy capital, suppression of orthographic alternatives). The modernization framing naturalizes what is actually a contingent political choice: other paths to mass literacy existed but were foreclosed by state power.
constraint_indexing:constraint_classification(modernization_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(modernization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(modernization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(modernization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(modernization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42 at T=30): Moderate. The modernization reading acknowledges substantial initial extraction (0.68 at T=0: destroyed literacy capital, institutional disruption, cultural rupture) but frames it as amortized coordination cost. As the Latin-script literate population expanded from near-zero in 1928 to majority by 1958, the per-capita extraction declined. The 0.42 endpoint reflects persistent costs: ongoing suppression of Arabic script alternatives, loss of access to Ottoman literary heritage, and the career advantage captured by early adopters of the new system. Suppression (0.58 at T=30): Moderate-high. Significant but declining enforcement: legal prohibition of Arabic script in public communication, educational policy mandating Latin script exclusively, cultural production restrictions. The declining trajectory (0.85 → 0.58) reflects generational turnover — by 1958, most of the population under 40 had no Arabic-script literacy to suppress. But suppression remained substantial: alternative orthographic systems were foreclosed, and the 'Latin script = modernity' framing suppressed even discussion of alternatives. Theater ratio (0.35 at T=30): Low-moderate. The reform had genuine functional content (literacy expansion, technical vocabulary standardization) but increasing performative overlay. By 1958, the modernization narrative ('we are modern because we use Latin script') exceeded the technical necessity — other societies achieved comparable modernization with non-Latin scripts, revealing the contingency of the Turkish path. The modest increase (0.25 → 0.35) reflects the reform's transformation from technical instrument to identity marker.
 *
 * PERSPECTIVAL GAP:
 *   The modernization reading produces maximum perspectival divergence. The Kemalist state sees pure coordination (Rope): solving the problem of mass literacy and technical modernization. The new literate class sees temporary disruption with sunset (Scaffold): once generational turnover completes, enforcement becomes unnecessary. The Arabic-script literate population sees pure extraction (Snare): overnight illiteracy with no exit. Religious institutions and technical education see mixed coordination and extraction (Tangled Rope): embedded in the new system but bearing rupture costs. The analytical observer sees tangled rope at the civilizational scale: the reform solved real coordination problems but through a path that foreclosed alternatives and imposed substantial cultural costs. The gap between the state's Rope and the victims' Snare is the modernization reading's core tension: what the beneficiaries experience as rationalization, the victims experience as dispossession.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernization reading's beneficiary structure is state-centric: the Kemalist bureaucracy and the new literate class it created are the primary beneficiaries, experiencing the constraint as coordination (low d, low or negative chi). The Arabic-script literate population and religious institutions are the primary victims, experiencing maximum extraction (high d, high chi). The technical education sector occupies a mixed position: benefits from European alignment but bears costs from corpus rupture (moderate d, moderate chi). The analytical observer sees tangled rope: genuine coordination function (literacy expansion) intertwined with substantial extraction (cultural rupture, suppression of alternatives). The declining extractiveness trajectory reflects the modernization reading's core claim: initial rupture costs are temporary, amortized across expanding literacy. If this claim is false — if extraction persisted or intensified after generational turnover — the modernization reading is revealed as false summit, naturalizing political consolidation as technical necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The modernization reading resolves mandatrophy by explicitly framing the constraint as tangled rope from the analytical perspective: genuine coordination function (literacy expansion, technical vocabulary standardization) intertwined with substantial extraction (destroyed literacy capital, cultural rupture, suppression of alternatives). The reading does not claim the reform was pure coordination (Rope) or pure extraction (Snare) — it claims both functions coexist in the same structural arrangement. The declining extractiveness trajectory is the modernization reading's empirical bet: if extraction amortizes as literacy expands, the reading is vindicated. If extraction persists or intensifies, the reading is falsified and the constraint reclassifies toward Snare. The theater ratio's modest increase (0.25 → 0.35) signals the beginning of this falsification: by 1958, the modernization narrative was becoming more performative, suggesting the coordination function was exhausted while extraction mechanisms persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthographic_determinism,
    'Does script choice causally determine modernization capacity, or does the modernization reading naturalize a political choice as technical necessity?',
    'Comparative analysis of modernization trajectories in societies that retained non-Latin scripts (Japan, China, Arabic-script nations) vs. those that adopted Latin scripts. Control for state capacity, resource endowment, and geopolitical position.',
    'If script is causal: modernization reading is structurally accurate, extraction is coordination cost. If script is non-causal: modernization reading is false summit, extraction is political consolidation disguised as technical rationalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orthographic_determinism, empirical, 'Whether script choice causally determines modernization capacity').

omega_variable(
    alternative_literacy_pathways,
    'Could mass literacy have been achieved through reformed Arabic script or parallel orthographic systems, or was Latin script adoption structurally necessary?',
    'Historical analysis of proposed Arabic script reforms (1920s Turkish Language Commission debates); comparison with successful Arabic script literacy campaigns in other contexts; assessment of technical barriers vs. political barriers to alternative pathways.',
    'If alternatives were viable: suppression metric understates the constraint''s coercive character. If Latin script was necessary: suppression reflects coordination enforcement rather than political extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_literacy_pathways, conceptual, 'Viability of alternative literacy expansion pathways').

omega_variable(
    committer_frame_kernel_reading,
    'Is this constraint one reading of the orthographic_kernel, or is the kernel itself a retrospective construction that naturalizes the modernization framing?',
    'Genealogical analysis: when did ''the script question'' become a unified kernel with competing readings? Were continuity/rupture/modernization framings live alternatives in 1928, or were they constructed retrospectively by different historiographic traditions?',
    'If the kernel is contemporaneous: the three readings are genuine structural alternatives, and the committer frame captures real 1928 decision space. If the kernel is retrospective: the committer frame itself is a historiographic construction, and the ''readings'' are later interpretive overlays rather than live alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Whether orthographic_kernel is contemporaneous or retrospective construction').

omega_variable(
    generational_sunset_realization,
    'Did the scaffold perspective''s sunset logic realize — did enforcement mechanisms actually attenuate after generational turnover — or did new extraction mechanisms emerge?',
    'Longitudinal analysis of orthographic enforcement intensity 1928-1960: prosecution rates for Arabic script usage, educational policy evolution, cultural production restrictions. Compare predicted sunset (1950s-1960s) with actual policy trajectory.',
    'If sunset realized: scaffold classification confirmed for new_literate_class perspective. If enforcement persisted or intensified: scaffold was aspirational framing, actual trajectory was tangled_rope or snare consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_sunset_realization, empirical, 'Whether predicted enforcement sunset actually occurred').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(modernization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mod_read_theater_1928, modernization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mod_read_theater_1938, modernization_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(mod_read_theater_1948, modernization_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(mod_read_theater_1958, modernization_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(mod_read_extract_1928, modernization_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(mod_read_extract_1933, modernization_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(mod_read_extract_1938, modernization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mod_read_extract_1948, modernization_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(mod_read_extract_1958, modernization_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mod_read_suppress_1928, modernization_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(mod_read_suppress_1933, modernization_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(mod_read_suppress_1938, modernization_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(mod_read_suppress_1948, modernization_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(mod_read_suppress_1958, modernization_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(modernization_reading, information_standard).
narrative_ontology:affects_constraint(modernization_reading, continuity_reading).
narrative_ontology:affects_constraint(modernization_reading, rupture_reading).

% DUAL FORMULATION NOTE:
% The orthographic_kernel decomposes into three constraint stories (modernization_reading, continuity_reading, rupture_reading) because the same 1928 reform has three structurally distinct interpretations with different epsilon values. The modernization reading has moderate extraction (0.42) reflecting amortized coordination costs. The continuity reading (expected: low extraction, high suppression) emphasizes foreclosed alternatives. The rupture reading (expected: high extraction, moderate suppression) emphasizes political consolidation. These are not the same constraint viewed from different angles — they are different structural claims about which function was primary and which costs were necessary. The epsilon-invariance principle requires separate stories; the network edges link them as alternative interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
