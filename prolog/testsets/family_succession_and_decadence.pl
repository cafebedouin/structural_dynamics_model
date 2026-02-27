% ============================================================================
% CONSTRAINT STORY: family_succession_and_decadence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_succession_and_decadence, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_succession_and_decadence
 *   human_readable: Meiji-Taisho Family Succession and the "Ie" System
 *   domain: social/familial
 *
 * SUMMARY:
 *   The Meiji-Taisho Ie (House) system represents a rigidly formalized
 *   kinship structure, legally codified in the 1898 Civil Code, that
 *   subordinated individual autonomy to household hierarchy and patrilineal
 *   primogeniture. The system enforced property consolidation, authority
 *   clarity, and demographic continuity through systematic suppression of
 *   non-heir sons, daughters, and adopted members. Tanizaki's 'Atsumono'
 *   depicts this constraint through the lived experience of family members —
 *   those benefiting from primogeniture authority and those crushed by
 *   exclusion. The structural tension emerges from the Ie system's dual
 *   nature: it solves a genuine coordination problem (unambiguous property
 *   transfer, clear succession authority) while extracting severe costs from
 *   those it excludes (non-heirs, women, adopted members forced into identity
 *   erasure). This dual structure makes it a canonical Tangled Rope:
 *   coordination function present, asymmetric extraction present, active
 *   legal enforcement present. Yet the system exhibits degradation over the
 *   interval: as urbanization and industrial capitalism fragment household
 *   economies and create alternative identity structures, the Ie system's
 *   enforcement mechanism shifts from legal/economic coercion to cultural
 *   theater. The piton perspective captures this — the state maintains the
 *   legal architecture, but practical enforcement has atrophied. The Scaffold
 *   perspective emerges as urban workers discover that factory wages and
 *   urban migration provide genuine exit from household obligations, making
 *   the system a temporary obstacle to be overcome rather than an inescapable
 *   trap. The constraint exhibits Mandatrophy risk: classifying the Ie system
 *   as pure Mountain (natural law of kinship) would conceal its contingency
 *   on specific legal and economic conditions; classifying it as pure Snare
 *   would ignore its genuine coordination function. The DR framework reveals
 *   it as a contingent institutional arrangement whose extractiveness is
 *   historically specific and declining with structural economic change.
 *
 * KEY AGENTS:
 *   - Primogeniture Heir: Primary beneficiary (institutional/arbitrage) — captures consolidated property, undivided authority, and social status; experiences the system as legitimate coordination
 *   - Non-Heir Sons: Primary victims (powerless/trapped) — excluded from inheritance despite capability, face obligation to support the house, constrained to subordinate roles; zero agency within the system
 *   - Daughters: Primary victims (powerless/trapped) — excluded from succession by gender, subordinated to father then husband, denied independent legal status; doubly trapped by Ie logic plus patrilineal marriage laws
 *   - Adopted Heirs: Secondary victims (moderate/constrained) — offered social mobility and property access but forced to suppress own lineage identity and assimilate completely into Ie structure; constrained choice rather than free agency
 *   - Merchant and Landowning Families: Institutional beneficiaries (powerful/mobile) — benefit from property consolidation and authority preservation but increasingly constrained by alternative structures (corporations, financial markets); maintain system for extraction even as coordination value declines
 *   - Urban Industrial Workers: Emerging exit class (moderate/mobile) — factory wages and urban migration provide genuine escape from household obligation; experience Ie system as temporary scaffold rather than trap
 *   - Meiji State: Legal enforcer (institutional/arbitrage) — codifies and maintains Ie system through civil code; benefits from household-level social control; experiences system as legitimate governance architecture; increasingly theatrical as enforcement capacity declines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing Ie system as inherent to human kinship; false summit risk if system is classified as mountain rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_succession_and_decadence, 0.58).
domain_priors:suppression_score(family_succession_and_decadence, 0.72).
domain_priors:theater_ratio(family_succession_and_decadence, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_succession_and_decadence, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_succession_and_decadence, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_succession_and_decadence, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_succession_and_decadence, tangled_rope).
narrative_ontology:human_readable(family_succession_and_decadence, "Meiji-Taisho Family Succession and the \"Ie\" System").
narrative_ontology:topic_domain(family_succession_and_decadence, "social/familial").

domain_priors:requires_active_enforcement(family_succession_and_decadence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_succession_and_decadence, male_primogeniture_heirs).
narrative_ontology:constraint_beneficiary(family_succession_and_decadence, patriarchal_authority_structure).
narrative_ontology:constraint_beneficiary(family_succession_and_decadence, landed_merchant_families).
narrative_ontology:constraint_victim(family_succession_and_decadence, non_heir_children).
narrative_ontology:constraint_victim(family_succession_and_decadence, daughters).
narrative_ontology:constraint_victim(family_succession_and_decadence, adopted_heirs).
narrative_ontology:constraint_victim(family_succession_and_decadence, family_emotional_bonds).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-HEIR SON (SNARE) — Trapped by primogeniture within the Ie system. Cannot exit family obligations, career constraints, or the cultural imperative to support the house. Bears full burden of succession system while stripped of inheritance and authority. Zero degrees of freedom within the patriarchal structure.
constraint_indexing:constraint_classification(family_succession_and_decadence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DAUGHTER (SNARE) — Doubly trapped: excluded from succession regardless of capability, subordinated to both father and husband under patrilineal Ie logic. Marriage is externalization, not partnership. Economic and legal dependency enforced by civil code. Maximum suppression with zero alternative pathways.
constraint_indexing:constraint_classification(family_succession_and_decadence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ADOPTED HEIR (TANGLED ROPE) — Constrained choice: adoption offers social mobility and property access but enforces complete assimilation into the Ie identity. Retains some agency (could refuse adoption) but faces severe career penalties and family shame. Benefits from inheritance coordination mechanism but extracted through obligation to suppress own lineage identity. Hybrid structure: coordination function (property transfer, family continuity) with asymmetric extraction (identity erasure).
constraint_indexing:constraint_classification(family_succession_and_decadence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRIMOGENITURE HEIR (ROPE) — Primary beneficiary. The Ie system functions as pure coordination from this perspective: property consolidation, authority clarity, and social status are unified. Exit options exist (renounce inheritance, break with family) but carry no cost — the heir profits from the system without coercion. Experiences the constraint as legitimate structure, not extraction.
constraint_indexing:constraint_classification(family_succession_and_decadence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL CODE AND STATE AUTHORITY (PITON) — The Meiji Civil Code (1898) formally codifies the Ie system through law, yet the system's actual function is largely performative by the Taisho period (1912-1926). The state maintains the legal architecture (household registry, primogeniture enforcement, paternal authority) but practical enforcement has degraded as urbanization and industrial capitalism create alternative identity structures. Theater ratio high: ceremonial affirmation of Ie authority persists even as economic reality (wage work, urban migration) undermines household sovereignty. The law maintains itself through inertia, not active coercion.
constraint_indexing:constraint_classification(family_succession_and_decadence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: URBAN INDUSTRIAL WORKING CLASS (SCAFFOLD) — Factory workers, urban laborers, and salaried employees in industrial districts experience the Ie system as a temporary constraint. Factory work fragments household economies and creates alternative identity structures (worker solidarity, urban networks). The Ie system's extraction mechanism (family obligation, property consolidation) loses force as wages replace inheritance as primary income source. The sunset is structural: as Japan urbanizes and industrializes (accelerating from 1920s onward), the Ie system's hold weakens. Not a formal policy sunset but a structural obsolescence driven by economic transformation. Theater high because Ie rhetoric persists while practical enforcement declines.
constraint_indexing:constraint_classification(family_succession_and_decadence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: MERCHANT AND LANDOWNING FAMILIES (TANGLED ROPE) — These institutional actors benefit from the Ie system's property consolidation and authority preservation, but increasingly constrained by alternatives (corporate organization, financial markets, colonial opportunities). The system provides genuine coordination benefit (unambiguous succession, property consolidation) but also extracts through opportunity cost: merchant houses locked into traditional structures lose competitive advantage to corporations with flexible capital allocation. Suppression remains high (breach of Ie duties carries severe social cost) but exit options exist for those with capital. Ambiguous classification: is this still extraction, or has the system degraded to theater?
constraint_indexing:constraint_classification(family_succession_and_decadence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational/universal perspective, hierarchical family succession and primogeniture appear as natural or inevitable social laws: all societies require property transfer mechanisms, authority clarity, and demographic reproduction. The Ie system looks like a natural solution to universal constraints. However, this perspective risks false summitry: the structural data contradicts mountain classification. The Ie system is contingent on specific legal, economic, and cultural conditions — not inherent to human society. The analytical observer naturalizes what is actually a mid-range institutional arrangement.
constraint_indexing:constraint_classification(family_succession_and_decadence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_succession_and_decadence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_succession_and_decadence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_succession_and_decadence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_succession_and_decadence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_succession_and_decadence, TR),
    TR >= 0.70.

:- end_tests(family_succession_and_decadence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The Ie system extracts substantial opportunity cost from non-heirs, daughters, and adopted members through suppression of alternative life paths and forced subordination. Yet extractiveness is not maximal (snare-level ≥0.66) because: (1) property consolidation and authority clarity provide genuine coordination value that partially justifies restriction; (2) exit options, though costly, exist through urban migration and wage work; (3) extraction operates primarily through opportunity suppression rather than direct coercion or resource transfer. The declining trajectory (0.62 → 0.58) reflects weakening enforcement as urban industrial development creates alternative structures. Suppression (0.72): High. The system enforces primogeniture through legal code (Civil Code Article 962-988), social honor/shame mechanisms, and economic dependency. Non-heirs have essentially zero exit options within their primary context (rural household). Daughters' legal options are severely restricted (no independent contract capacity until 1947). Adopted heirs face complete identity erasure as structural condition of acceptance. Suppression weakens with urbanization but remains high during the Meiji-Taisho interval. Theater ratio (0.65): Moderate-high. By the Taisho period, the Ie system exhibits increasing performativity: the legal architecture (household registry, primogeniture rules, paternal authority clauses) is maintained by the state, but actual enforcement has degraded. Urban workers, factory employees, and modernizing merchant houses increasingly ignore household obligations in practice while maintaining nominal compliance in civil registration. The rise from 0.38 → 0.65 reflects the growing gap between ceremonial affirmation of Ie authority (theater) and actual economic/social control (declining function). The Piton classification captures this degradation.
 *
 * PERSPECTIVAL GAP:
 *   Why do these perspectives disagree? (1) Exit options differ radically. The heir has genuine choice (renounce inheritance with minimal cost); the non-heir has none (breaking family ties means destitution). (2) Extraction direction differs. Benefits flow toward heirs; costs flow from non-heirs, daughters, and adopted members. (3) Temporal horizons differ. The heir thinks immediate (current authority and inheritance); the urban worker thinks generational (does this bind my children?); the observer thinks civilizational (is this inherent to society?). (4) Spatial scopes differ. The rural landowner experiences the system as national law; the urban factory worker experiences it as regional custom eroding; the analytical observer sees it from universal perspective. These are not different opinions about the same fact — they are different structural realities experienced by agents in different positions within the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position: (1) power level (constraint-relative, not absolute); (2) exit options available to them within this specific constraint; (3) beneficiary/victim status within the Ie system. Primogeniture heirs experience d ≈ 0.05 (beneficiary with arbitrage options) → low/negative chi; they do not experience extraction. Non-heir sons experience d ≈ 0.95 (victims with trapped exit) → maximum chi ≈ 1.5+; they experience maximum extraction. Daughters experience d ≈ 0.98 (doubly trapped) → maximum chi; they experience severe extraction. Adopted heirs experience d ≈ 0.65 (victims with constrained exit) → moderate-high chi; they experience significant extraction despite benefits. Urban workers experience d ≈ 0.55 (victims with mobile exit) → moderate chi; they experience extraction but have genuine escape routes. The merchant family at institutional level experiences d ≈ 0.40 (constrained by alternatives, still benefiting) → moderate chi approaching zero as alternatives strengthen. The state experiences d ≈ 0.10 (arbitrage position) → low chi; it enforces but does not bear the system's costs. These derivations follow from structural position, not from power as absolute ranking. The constraint-specific powerlessness of daughters is real even though they may have institutional agency in other contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK AND RESOLUTION: The Ie system is susceptible to false natural law classification (Mountain) because primogeniture and hierarchical succession appear inherent to human kinship. Resolution requires: (1) Recognition that alternative property transfer mechanisms (partible inheritance, cognatic succession, corporate organization) achieve coordination without primogeniture suppression — proves the Ie system is contingent, not natural law. (2) Measurement of extraction asymmetry — the Ie system extracts from non-heirs and daughters in ways unnecessary for basic succession coordination — proves extraction is present even when coordination is achieved. (3) Temporal analysis — extractiveness declines as urban industrial development provides alternative structures — proves the system's force is contingent on economic conditions, not inherent. (4) Acknowledgment of the Piton perspective — the system persists increasingly through legal theater rather than enforcement — proves degradation toward pure form maintenance. MANDATROPHY RESOLVED through: The Ie system is a Tangled Rope (coordination + extraction hybrid) with Piton degradation over time. It is NOT a Mountain because it is historically contingent, empirically avoidable, and weakening with structural economic change. It is NOT pure Rope because non-heirs and daughters are systematically excluded despite having capability. It is NOT pure Snare because the coordination function (property consolidation, succession clarity) is genuinely valuable and explains the system's persistence. The hybrid Tangled Rope classification accurately captures both the coordination function and the extraction asymmetry. The Scaffold and Piton perspectives accurately capture the system's degradation trajectory: urbanization and wage work provide real exit; legal theater persists as actual enforcement declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_boundary,
    'How much of the Ie system''s function is genuine coordination (property transfer, authority clarity) versus extraction (opportunity suppression, identity erasure)?',
    'Counterfactual analysis: alternative property transfer mechanisms that achieve coordination without primogeniture suppression; measurement of non-heir and female outcomes under alternative arrangements; historical comparison with systems (e.g., partible inheritance) that achieved coordination differently',
    'If coordination dominates: classification shifts toward Rope from multiple perspectives. If extraction dominates: Snare and Tangled Rope classifications confirmed. Critical for mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Boundary between Ie coordination function and extractive suppression').

omega_variable(
    adoption_mechanism_intent,
    'Does the adoption mechanism (yoshi system) represent genuine flexibility in heir selection or is it purely extractive — a safety valve to maintain the Ie system by allowing non-male or non-biological succession under controlled conditions?',
    'Historical analysis of adoption rates and outcomes; comparison of adopted heir success and family satisfaction versus biological heir outcomes; archival evidence of family decision-making in adoption cases; analysis of whether adoption maintained or subverted the Ie principle',
    'If adoption represents true flexibility: system has lower suppression than measured, and Rope classification gains strength. If adoption is a pressure relief valve: suppression remains high, and it is an extraction mechanism disguised as flexibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adoption_mechanism_intent, empirical, 'Whether adoption is flexible succession or extractive safety valve').

omega_variable(
    enforcement_mechanism_degradation,
    'By the Taisho period (1912-1926), how much of the Ie system''s persistence derives from active legal/social enforcement versus cultural inertia and theater?',
    'Analysis of court cases enforcing primogeniture; measurement of actual inheritance practices versus legal requirements; documentation of family disputes and how they were resolved; comparison of enforcement intensity across urban vs rural areas, merchant vs non-merchant households',
    'If enforcement remains high: Snare and Tangled Rope classifications sustained. If enforcement has degraded to theater: Piton classification gains strength, and the overall type shifts toward Piton or early Scaffold at multiple perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_degradation, empirical, 'Degree of active enforcement versus cultural inertia in Ie system persistence').

omega_variable(
    urban_industrial_exit_threshold,
    'At what point of urbanization and industrial development does the Ie system''s extractive force become negligible? Is the urban working class truly exiting the system, or are they sustaining it through remittances and periodic family obligation?',
    'Economic data on urban wage worker remittances to rural households; analysis of household composition and financial flows in urban workers'' families; documentation of family obligation claims on urban workers; comparison of Ie compliance between urbanized and rural populations',
    'If urban workers are genuinely exiting: Scaffold perspective is accurate, and sunset timeline is real. If remittances and obligation persist: urban workers remain constrained, and the system''s extractiveness extends into industrial zones despite theater claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urban_industrial_exit_threshold, empirical, 'Whether urban industrial development enables genuine exit from Ie system').

omega_variable(
    female_agency_and_resistance,
    'How much agency did women and non-heir children actually exercise within the Ie system? Were they passive victims or did they develop counter-strategies, resistance practices, or alternative structures?',
    'Analysis of personal correspondence, diaries, literary representations (e.g., Tanizaki''s ''Atsumono''); oral histories of women and non-heir family members; documentation of resistance practices (elopement, divorce, rejection of adoption, creation of female-centered social networks); analysis of how women navigated property, marriage, and identity within and against the system',
    'If significant agency and resistance: Snare classification weakens; Tangled Rope becomes more accurate for female perspectives. If passive victimhood dominates: Snare classification sustained. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_agency_and_resistance, empirical, 'Degree of female and non-heir agency within and against the Ie system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_succession_and_decadence, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(famsuc_tr_t0, family_succession_and_decadence, theater_ratio, 0, 0.38).
narrative_ontology:measurement(famsuc_tr_t7, family_succession_and_decadence, theater_ratio, 7, 0.52).
narrative_ontology:measurement(famsuc_tr_t14, family_succession_and_decadence, theater_ratio, 14, 0.65).

% Extraction over time
narrative_ontology:measurement(famsuc_be_t0, family_succession_and_decadence, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(famsuc_be_t7, family_succession_and_decadence, base_extractiveness, 7, 0.6).
narrative_ontology:measurement(famsuc_be_t14, family_succession_and_decadence, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_succession_and_decadence, resource_allocation).
narrative_ontology:affects_constraint(family_succession_and_decadence, patrilineal_inheritance_law).
narrative_ontology:affects_constraint(family_succession_and_decadence, women_legal_capacity_restrictions).
narrative_ontology:affects_constraint(family_succession_and_decadence, household_registry_system).

% DUAL FORMULATION NOTE:
% The Ie system as legal structure (Civil Code Articles 962-988) and the Ie system as social practice are distinct constraints with different extractiveness values. The legal system analysis yields ε=0.58 (coordination plus extraction); the social practice analysis in urban areas yields lower ε as enforcement degrades. These are linked through the legal-to-social mapping but have separable structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_succession_and_decadence, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
