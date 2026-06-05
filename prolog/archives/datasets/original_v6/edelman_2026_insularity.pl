% ============================================================================
% CONSTRAINT STORY: edelman_2026_insularity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_edelman_2026_insularity, []).

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
 *   constraint_id: edelman_2026_insularity
 *   human_readable: The Insular Trust Mindset
 *   domain: social/economic
 *
 * SUMMARY:
 *   The insular trust mindset represents a global structural condition where
 *   trust is increasingly allocated along in-group/out-group lines rather
 *   than along reciprocal or merit-based lines. This constraint has
 *   accelerated over the past 15 years due to digital media architectures
 *   that amplify identity salience and algorithmic curation that concentrates
 *   exposure within belief clusters. The constraint exhibits a classic
 *   Tangled Rope structure: it simultaneously provides a coordination
 *   function (enabling group solidarity, mutual aid, and cultural
 *   preservation within communities) and extracts from those outside
 *   designated groups (blocking economic opportunity, restricting social
 *   mobility, concentrating institutional power). The extractiveness has
 *   grown over the measurement interval (0.32 → 0.58) as institutional
 *   insularity has deepened, while the theater ratio has also increased (0.35
 *   → 0.64), indicating that performative identity signaling now exceeds the
 *   functional coordination gain. The constraint is neither a natural law of
 *   human cognition nor a temporary policy issue — it is an institutional
 *   arrangement built into specific social technologies (social media
 *   platforms, algorithmic feeds, identity-based organizational practices)
 *   that is currently extracting value from out-group members but faces
 *   genuine sunset pressure from educational institutions and cross-cultural
 *   professional networks that are building alternative trust mechanisms.
 *
 * KEY AGENTS:
 *   - In-Group Gatekeepers: Primary beneficiaries (institutional/arbitrage) — control access to group resources, status, and information; gain influence through boundary maintenance
 *   - Out-Group Members: Primary victims (powerless/trapped) — excluded from trust networks due to identity; no exit option except assimilation or isolation
 *   - Cross-Cultural Economic Participants: Secondary victims (moderate/constrained) — need access to multiple communities but face friction and suspicion in both; constrained exit
 *   - Integration Coalition: Organized agents (organized/constrained) — civil society, educational institutions, cross-cultural businesses building alternative trust pathways with generational sunset
 *   - Legacy Trust Institutions: Institutional actors (institutional/arbitrage) — maintain ceremonial boundary-enforcement through inertia; perform trust-vetting rituals with degraded functional value
 *   - Algorithmic Curation Systems: Institutional architects (institutional/arbitrage) — social media platforms amplify in-group/out-group psychology through engagement-maximizing algorithms; primary structural amplifiers of insularity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(edelman_2026_insularity, 0.58).
domain_priors:suppression_score(edelman_2026_insularity, 0.68).
domain_priors:theater_ratio(edelman_2026_insularity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(edelman_2026_insularity, extractiveness, 0.58).
narrative_ontology:constraint_metric(edelman_2026_insularity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(edelman_2026_insularity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(edelman_2026_insularity, tangled_rope).
narrative_ontology:human_readable(edelman_2026_insularity, "The Insular Trust Mindset").
narrative_ontology:topic_domain(edelman_2026_insularity, "social/economic").

domain_priors:requires_active_enforcement(edelman_2026_insularity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, in_group_gatekeepers).
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, ideological_entrepreneurs).
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, insular_media_platforms).
narrative_ontology:constraint_victim(edelman_2026_insularity, out_group_members).
narrative_ontology:constraint_victim(edelman_2026_insularity, cross_group_trust).
narrative_ontology:constraint_victim(edelman_2026_insularity, economic_integration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OUT-GROUP MEMBER (SNARE) — Individuals from non-dominant groups or those holding minority viewpoints face systematic exclusion from trust networks. Trapped by the structural condition: cannot change their origin, background, or core identity to gain entry. No exit option except assimilation or isolation. Maximum extraction: bear the cost of ingroup suspicion while gaining none of the trust coordination benefits.
constraint_indexing:constraint_classification(edelman_2026_insularity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CROSS-CULTURAL ECONOMIC PARTICIPANT (TANGLED ROPE) — Workers, entrepreneurs, and families spanning multiple communities benefit from and are harmed by insular trust boundaries. Need access to multiple networks for employment and economic survival (coordination benefit), but face friction and suspicion (extraction cost). Constrained exit: cannot fully leave either group without economic loss. Moderate experienced extraction — significant asymmetry but some agency.
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IN-GROUP GATEKEEPER (ROPE) — Individuals and institutions that maintain and enforce ingroup trust boundaries (community leaders, cultural authorities, institutional gatekeepers) experience the constraint primarily as a coordination mechanism: organizing and maintaining group cohesion. Net beneficiary — gate-keeping behavior delivers status, influence, and resource allocation control. Arbitrage exit: can select when to enforce or relax boundaries based on personal advantage.
constraint_indexing:constraint_classification(edelman_2026_insularity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTEGRATION COALITION (SCAFFOLD) — Organized actors (international business, civil society organizations, educational institutions, diversity-focused movements) see insular trust as a temporary coordination failure with a structural sunset. Cross-cultural education, professional networks, and mixed-background communities are creating alternative trust pathways with lower ingroup/outgroup friction. Sunset logic: as younger cohorts grow up with mixed networks and institutions normalize diversity, the extraction mechanisms lose force. Theater ratio declining.
constraint_indexing:constraint_classification(edelman_2026_insularity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY TRUST INSTITUTION (PITON) — Traditional institutions (churches, civic organizations, trade associations) that were built on and enforce ingroup trust boundaries now perform primarily ceremonial functions. The institutions persist through inertia: they maintain insider/outsider distinction without the former practical coordination benefit. High theater ratio (0.64): performative trust-vetting rituals with degraded real gatekeeping function, as digital networks and weak ties have become the primary trust mechanism. Inertial maintenance of boundaries that once served group survival but now serve mainly status performance.
constraint_indexing:constraint_classification(edelman_2026_insularity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal lens, in-group preference and suspicion of outsiders might appear as a fundamental feature of human social cognition: evolved preferences for kin and tribe, psychological categorization limits, cognitive boundaries on trust management. This perspective sees insularity as immutable law. However, the structural data contradicts the mountain classification — the base extractiveness (0.58) and active enforcement requirement indicate this is a contingent institutional arrangement amplified by specific social technologies (social media echo chambers, algorithmic curation, identity politics infrastructure), not a natural law.
constraint_indexing:constraint_classification(edelman_2026_insularity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(edelman_2026_insularity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(edelman_2026_insularity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(edelman_2026_insularity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(edelman_2026_insularity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(edelman_2026_insularity, TR),
    TR >= 0.70.

:- end_tests(edelman_2026_insularity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from out-group members through systematic exclusion from opportunity networks, but the extraction is not as extreme as a pure Snare (0.66+) because some institutional actors genuinely experience it as coordination (gatekeepers) and some participants can navigate multiple communities (constrained, not trapped). The value reflects measured institutional amplification via algorithmic curation (which has high extractiveness) tempered by existing cross-group economic relationships (which have lower extractiveness). Suppression (0.68): High. Significant barriers to exit and boundary-crossing include: psychological in-group preference, institutional gatekeeping, algorithmic echo chambers that reduce exposure to out-group perspectives, social costs of boundary-crossing, and performative identity signaling (theater) that raises the cost of relaxing group boundaries. Theater ratio (0.64): High and rising. Identity performance and group-signaling have become prominent features of the constraint; in-group/out-group rhetoric increasingly serves ceremonial/status functions rather than practical coordination (especially in institutionalized contexts). The growth trajectory (0.35 → 0.64) indicates theater is substituting for function, a classic degradation pattern.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. Out-group members experience pure extraction (Snare) with no exit. In-group gatekeepers experience pure coordination (Rope) with arbitrage exits. Cross-cultural participants experience mixed outcomes (Tangled Rope). The integration coalition sees a temporary problem with a structural sunset (Scaffold). Legacy institutions see their own degraded ritual (Piton). The analytical observer risks seeing natural law (Mountain). This is a diagnostic case for how the same structural metrics (extractiveness, suppression, theater) produce radically different classifications depending on the agent's position. The perspectival gap is not a measurement error — it is the fundamental reality of how the constraint operates: it is experienced as coordination by beneficiaries, as extraction by victims, as temporary by those building alternatives, and as natural by those who naturalize institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent structural position. Out-group members (powerless/trapped) experience d ≈ 0.95, producing high chi via f(d). They cannot exit the constraint's effect on their life chances — their identity is fixed, and institutional boundaries are enforced by others. In-group gatekeepers (institutional/arbitrage) experience d ≈ 0.05, producing negative or near-zero chi — they benefit from the constraint and can choose when to enforce or relax boundaries based on advantage. Cross-cultural participants (moderate/constrained) experience d ≈ 0.55, producing moderate chi — they benefit from group belonging but bear costs from friction in multiple communities. The integration coalition (organized/constrained) experiences d ≈ 0.40, producing moderate chi — they are constrained by existing institutional barriers but have sufficient organization and agency to build alternative pathways. Legacy institutions (institutional/arbitrage) experience d ≈ 0.10, producing low chi — they benefit from maintaining the constraint's appearance while actual trust mechanisms have migrated to digital networks. The analytical observer (analytical/analytical) risks assigning d ≈ 0.70 (mountain perspective) by treating insularity as natural, but the structural data (high theater ratio, rising extractiveness over time, identifiable institutional/technological amplifiers) indicates this is a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids the mislabeling trap by explicitly declaring both beneficiaries (in-group gatekeepers, ideological entrepreneurs) and victims (out-group members, cross-group trust). The beneficiary perspective (Rope) is genuine and important — the constraint does provide real coordination function for in-group members. The victim perspective (Snare) is equally genuine — it reflects the structural reality for those excluded. The tangled_rope classification at the primary analytical level integrates both: the constraint has a coordination function AND asymmetric extraction. The suppression value (0.68) confirms both elements: significant barriers to exit (extraction) AND significant group cohesion/solidarity benefit (coordination). The theater ratio rising to 0.64 indicates that the ratio of coordination to extraction is shifting — more performative identity signaling, less functional mutual aid — but the constraint remains hybrid, not pure. The scaffold and piton perspectives reveal aging institutions (legacy trust institutions with ceremonial functions) alongside emerging alternatives (cross-cultural educational networks, professional associations, mixed-background communities) that are creating structural sunset pressure. This is not a false snare mislabeled as coordination, nor a false rope mislabeled as extraction — it is a genuine hybrid where both elements are structurally present and perspectivally separable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_versus_institutional_insularity,
    'Is the insular trust mindset primarily a cognitive/evolutionary constraint on how humans categorize others, or is it primarily an institutional product of media ecosystems and political mobilization?',
    'Cross-cultural comparison of insularity levels in pre-digital vs digital communities; measurement of trust patterns in societies with different institutional/media architectures; analysis of within-individual trust shifts when media exposure changes',
    'If primarily cognitive: extractiveness floor is inherent (~0.25-0.35) and suppression is hard to reduce. If primarily institutional: extractiveness is contingent on design choices and can be substantially reduced via structural change (e.g., algorithm redesign, media literacy, institutional integration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_versus_institutional_insularity, empirical, 'Whether insularity is cognitive limit or institutional amplification').

omega_variable(
    identity_salience_threshold,
    'What threshold of identity salience (how prominent group identity is in daily interaction) determines whether trust boundaries become extractive vs coordinative?',
    'Measurement of trust behavior and group preference intensity across communities with varying identity salience; longitudinal tracking of shifts in trust patterns when identity salience increases or decreases',
    'If threshold is low (~0.3): even modest identity emphasis triggers extraction asymmetry (current classification holds). If threshold is high (~0.7): identity boundaries remain mostly coordinative until very high salience (classification shifts toward Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_salience_threshold, empirical, 'Identity salience threshold for extractive versus coordinative trust boundaries').

omega_variable(
    intergenerational_sunset_timeline,
    'How many cohorts need exposure to mixed-background networks before institutional insularity norms degrade sufficiently to shift the constraint from Tangled Rope/Snare toward Scaffold/Rope?',
    'Longitudinal cohort analysis of trust attitudes and in-group preference across 3-4 generational transitions; measurement of institutional boundary persistence when demographic composition of institutions changes',
    'If 1-2 generations (~20-40 years): sunset clause is real and timely, scaffold classification is structural. If 3+ generations (60+ years): sunset is aspirational, not structural — scaffold should downgrade to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_sunset_timeline, empirical, 'Intergenerational timeline for institutional insularity sunset').

omega_variable(
    algorithmic_amplification_reversibility,
    'If social media algorithms that amplify in-group/out-group boundaries are removed or fundamentally redesigned, what percentage of observed insularity persists vs reverses?',
    'Quasi-experimental design with platform-scale algorithm changes; analysis of trust metrics before/after; geographic variation in algorithm exposure',
    'If <20% persists: insularity is primarily institutional (current Tangled Rope classification confirmed). If 60%+ persists: significant cognitive substrate (mountain perspective has more merit; classification may shift).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_amplification_reversibility, empirical, 'Reversibility of algorithmic amplification of insularity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(edelman_2026_insularity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insularity_tr_t0, edelman_2026_insularity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(insularity_tr_t5, edelman_2026_insularity, theater_ratio, 5, 0.52).
narrative_ontology:measurement(insularity_tr_t10, edelman_2026_insularity, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(insularity_be_t0, edelman_2026_insularity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(insularity_be_t5, edelman_2026_insularity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(insularity_be_t10, edelman_2026_insularity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(edelman_2026_insularity, information_standard).
narrative_ontology:affects_constraint(edelman_2026_insularity, algorithmic_polarization).
narrative_ontology:affects_constraint(edelman_2026_insularity, cross_group_economic_mobility).
narrative_ontology:affects_constraint(edelman_2026_insularity, institutional_gatekeeping).

% DUAL FORMULATION NOTE:
% The insular trust mindset can be decomposed into three structurally distinct constraints: (1) cognitive/psychological in-group preference (natural law substrate, ε ≈ 0.15); (2) institutional gatekeeping practices (tangled rope, ε ≈ 0.45); (3) algorithmic amplification of identity boundaries (snare, ε ≈ 0.62). This story treats the composite institutional/technological phenomenon. Decomposition into three separate stories would enable measurement of the cognitive floor versus institutional amplification separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(edelman_2026_insularity, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
