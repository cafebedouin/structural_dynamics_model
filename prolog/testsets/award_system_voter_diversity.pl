% ============================================================================
% CONSTRAINT STORY: award_system_voter_diversity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_award_system_voter_diversity, []).

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
 *   constraint_id: award_system_voter_diversity
 *   human_readable: Award System Voter Diversity Constraint
 *   domain: cultural_institutional/governance
 *
 * SUMMARY:
 *   Award systems function simultaneously as coordination mechanisms
 *   (establishing shared standards for excellence across a cultural field)
 *   and as extraction systems (concentrating gatekeeping power in homogeneous
 *   voter populations that validate established traditions while excluding
 *   emerging ones). The constraint exhibits the full range of DR
 *   classification from different structural positions: institutional
 *   gatekeepers experience pure coordination (rope), underrepresented
 *   creators experience pure extraction (snare), reform coalitions see a
 *   temporary institutional failure with clear remedies (scaffold), and the
 *   award institution itself uses diversity rhetoric to maintain core
 *   gatekeeping unchanged (piton). The theater ratio (0.65) reflects that
 *   diversity initiatives — expanded membership, equity committees, inclusion
 *   statements — are largely performative responses that leave core voting
 *   gatekeeping mechanisms intact. The extractiveness (0.52) reflects genuine
 *   career and opportunity asymmetries faced by underrepresented creators
 *   competing in fields where voting pools concentrate validation power. This
 *   constraint demonstrates how institutional legitimation systems use
 *   diversification theater to absorb external pressure while preserving
 *   extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Underrepresented Creators: Primary victim (powerless/trapped) — face credential barriers, network exclusion, and aesthetic devaluation; no exit short of field abandonment
 *   - Emerging Creative Communities: Secondary victim (moderate/constrained) — structurally mobile but face high participation costs; benefit from occasional breakthrough recognition but most excluded from voting
 *   - Established Institutions: Primary beneficiary (institutional/arbitrage) — homogeneous voters validate established traditions; maximum flexibility in adapting criteria without reputational cost
 *   - Diversity Reform Coalition: Organized agent (organized/mobile) — institutional diversity committees, alternative awards, open-voting systems; building parallel legitimacy pathways
 *   - Award Institution Leadership: Institutional actor (institutional/arbitrage) — maintains voting gatekeeping while performing diversity commitment; sees diversity as risk-management ritual
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent gatekeeping as inherent to cultural excellence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(award_system_voter_diversity, 0.52).
domain_priors:suppression_score(award_system_voter_diversity, 0.58).
domain_priors:theater_ratio(award_system_voter_diversity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(award_system_voter_diversity, extractiveness, 0.52).
narrative_ontology:constraint_metric(award_system_voter_diversity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(award_system_voter_diversity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(award_system_voter_diversity, tangled_rope).
narrative_ontology:human_readable(award_system_voter_diversity, "Award System Voter Diversity Constraint").
narrative_ontology:topic_domain(award_system_voter_diversity, "cultural_institutional/governance").

domain_priors:requires_active_enforcement(award_system_voter_diversity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(award_system_voter_diversity, institutional_gate_keepers).
narrative_ontology:constraint_beneficiary(award_system_voter_diversity, established_aesthetic_consensus).
narrative_ontology:constraint_victim(award_system_voter_diversity, underrepresented_creators).
narrative_ontology:constraint_victim(award_system_voter_diversity, aesthetic_diversity).
narrative_ontology:constraint_victim(award_system_voter_diversity, emerging_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERREPRESENTED CREATOR (SNARE) — Faces structural barriers to voting eligibility: membership thresholds, institutional affiliation requirements, language/geography barriers, or exclusionary credential definitions. Trapped within the award system's gatekeeping mechanisms. Participation requires accepting the dominant aesthetic tradition as legitimate metric. Maximum extraction experienced — no exit short of abandoning the field.
constraint_indexing:constraint_classification(award_system_voter_diversity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING CREATIVE COMMUNITY (TANGLED ROPE) — Benefits from award system recognition when it breaks through (establishes legitimacy, attracts resources) but faces high barriers to voting participation. Constrained by credential requirements and network gatekeeping. System coordinates some cross-community visibility while extracting through access barriers and vote concentration. Mixed structural position — some coordination benefit, significant extraction cost.
constraint_indexing:constraint_classification(award_system_voter_diversity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED INSTITUTION (ROPE) — Benefits from homogeneous voter pool that reliably validates established aesthetic consensus. Experiences the award system as pure coordination: defining standards, communicating achievements across the field, directing resources to validated traditions. Net beneficiary with maximum exit flexibility — can adapt voting criteria without reputational cost.
constraint_indexing:constraint_classification(award_system_voter_diversity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIVERSITY REFORM COALITION (SCAFFOLD) — Organized reform agents (diversity committees, alternative awards, open-access voting systems) see voter diversity constraints as temporary institutional failures with clear sunset mechanisms. Diversity expansion, quota systems, and alternative award structures represent pathways toward distributed legitimacy. Suppression declines over the time horizon as alternative systems mature. Has sunset clause — as new voting mechanisms establish parallel prestige, the original constraint's enforcement power weakens.
constraint_indexing:constraint_classification(award_system_voter_diversity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE AWARD INSTITUTION (PITON) — The award mechanism itself sees diversity constraints as performance ritual: diversity statements, inclusion policies, and expanding membership categories are largely theatrical responses to external pressure. Core voting gatekeeping persists through institutional inertia despite visible commitment to diversity. The theater has increased as pressure has mounted, but functional verification (actual voter composition, actual access barriers) remains stable or worsens. Maintained by prestige inertia, not because it achieves stated diversity goals.
constraint_indexing:constraint_classification(award_system_voter_diversity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal frame, some aesthetic consensus concentration is inherent to how cultural validation operates: novel traditions take time to establish legitimacy, and the gap between emergence and recognition is a structural feature of cultural evolution. This perspective sees voter homogeneity as an immutable property of cultural authority systems. However, the structural data contradicts the mountain classification — the engine's false summit detector identifies it as naturalization of a contingent institutional arrangement that concentrates gatekeeping power.
constraint_indexing:constraint_classification(award_system_voter_diversity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(award_system_voter_diversity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(award_system_voter_diversity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(award_system_voter_diversity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(award_system_voter_diversity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(award_system_voter_diversity, TR),
    TR >= 0.70.

:- end_tests(award_system_voter_diversity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Underrepresented creators face real opportunity costs (reduced access to resources, visibility, career advancement) from voting exclusion, but extraction is not total — some creators do break through, and parallel prestige pathways are emerging. The value reflects that gatekeeping power is substantial but not absolute; escape is possible at high cost. Suppression (0.58): Moderate-high. Structural barriers include membership requirements, credential definitions, geographic/linguistic factors, and network gatekeeping. Underrepresented voters face multiple overlapping barriers — no single dominant barrier, but cumulative suppression is high. Theater ratio (0.65): High. Diversity initiatives (expanded membership tiers, equity committees, inclusion statements, external scrutiny) are substantially performative — they signal commitment to diversity without reducing core voting gatekeeping. Award institutions display high theater because diversity pressure is external and rising, forcing performative response while core mechanisms remain unchanged. The theater has increased over the interval from 0.48 to 0.65 as diversity critique has intensified.
 *
 * PERSPECTIVAL GAP:
 *   The established institution experiences the award system as pure coordination (rope) — defining standards, communicating validated achievements, directing resources to recognized traditions. The underrepresented creator experiences the same system as pure extraction (snare) — barriers to participation, devaluation of non-traditional aesthetics, concentration of gatekeeping power. The emerging community sees mixed coordination and extraction (tangled rope) — the system enables recognition when breakthrough occurs but erects high barriers to participation. The reform coalition sees a temporary institutional failure with remedies (scaffold) — diversity initiatives, alternative awards, and distributed legitimacy are building sunset mechanisms. The institution itself performs diversity commitment while maintaining gatekeeping (piton) — theater increases (visible diversity statements) while core extraction mechanisms persist. The civilizational analytical observer risks naturalizing gatekeeping as inherent to cultural authority (mountain), but structural evidence reveals this as contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to gatekeeping power. Established institutions with high aesthetic validation and voting arbitrage experience d ≈ 0.15 (beneficiaries with low extraction). Underrepresented creators with no voting participation and trapped exit experience d ≈ 0.92 (victims with maximum extraction). Emerging communities with constrained participation experience d ≈ 0.68 (moderate victims with significant extraction). Reform coalitions with organized agency and mobile exit experience d ≈ 0.45 (organized agents with moderate extraction). The award institution itself occupies d ≈ 0.10 (beneficiary of voter homogeneity, high arbitrage flexibility). These directionality values, combined with power atoms and time horizons, produce the perspectival range from rope to snare.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY: This story decomposes into three structurally distinct constraints with different ε values that share the label 'award system voter diversity.' (1) AESTHETIC GATEKEEPING (ε=0.52, Tangled Rope, this story) — career and opportunity extraction from homogeneous voting. (2) CREDENTIAL DEFINITION (ε=0.35, Tangled Rope, decomposed) — structural barriers to voting eligibility (membership tiers, professional affiliation, geographic requirements). (3) CONSENSUS BIAS (ε=0.42, Snare, decomposed) — aesthetic devaluation of non-traditional work beyond gatekeeping. Each has different beneficiary/victim dynamics and different remedies. Credential definition is most immediately remediable (expand eligibility, reduce barriers). Aesthetic gatekeeping requires voter diversity and behavioral change. Consensus bias requires long-term tradition shifting. The mandatrophy is resolved by recognizing that 'voter diversity' is not a single constraint but a family of three structurally distinct extraction mechanisms, each with different ε values and remedies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_threshold_ambiguity,
    'What threshold of voter diversity constitutes genuine representation vs performative inclusion?',
    'Longitudinal analysis of voting outcome distributions relative to voter population composition; measurement of decision-making influence by underrepresented voters across award categories',
    'If threshold is demographic parity (50%+ underrepresented voters): current system is snare for majority of creators. If threshold is proportional influence: system may achieve tangled rope. If threshold is merely presence: piton classification dominates (theater without function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_threshold_ambiguity, empirical, 'Threshold for genuine diversity representation vs performance').

omega_variable(
    aesthetic_meritocracy_assumption,
    'Does homogeneous voting pool reflect genuine aesthetic meritocracy or learned consensus bias within a dominant tradition?',
    'Comparative analysis: outcomes when same works evaluated by in-group voters vs out-group voters; examination of voting patterns for boundary-case works (aesthetically experimental, traditionally undervalued); historical analysis of overturned judgments',
    'If meritocratic: current gatekeeping is justified; diversity is additive. If consensus-biased: gatekeeping is extractive; diversity is corrective to systematic undervaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aesthetic_meritocracy_assumption, conceptual, 'Whether homogeneous voting reflects meritocracy or consensus bias').

omega_variable(
    alternative_legitimacy_pathway_viability,
    'Can parallel award systems (crowdsourced, community-based, algorithmic) establish sufficient prestige to reduce extraction from the traditional system?',
    'Tracking of prestige metrics: citation/recognition rates for alternative-award winners; career outcomes for creators validated through non-traditional pathways; funding and opportunity access through alternative legitimacy channels',
    'If alternative pathways gain parity prestige: scaffold sunset is real — extraction mechanism weakens as creators gain exit options. If alternatives remain marginal: suppression persists; snare classification hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_legitimacy_pathway_viability, empirical, 'Whether alternative award systems can establish competitive prestige').

omega_variable(
    identity_lock_voter_participation,
    'Are gatekeeping voters locked into exclusionary criteria by internalized professional identity rather than external enforcement?',
    'Qualitative analysis of voter reasoning; measurement of voting change when professional identity frames are explicitly challenged; longitudinal tracking of voters who shift criteria',
    'If identity-locked: voter diversity achieves rope/scaffold only through identity frame disruption (training, cultural critique). If structurally constrained: diversity achieves rope through institutional policy change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_voter_participation, conceptual, 'Whether voter gatekeeping is identity-locked or structurally enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(award_system_voter_diversity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(awvd_tr_t0, award_system_voter_diversity, theater_ratio, 0, 0.48).
narrative_ontology:measurement(awvd_tr_t3, award_system_voter_diversity, theater_ratio, 3, 0.58).
narrative_ontology:measurement(awvd_tr_t6, award_system_voter_diversity, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(awvd_be_t0, award_system_voter_diversity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(awvd_be_t3, award_system_voter_diversity, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(awvd_be_t6, award_system_voter_diversity, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(award_system_voter_diversity, identity_coordination).
narrative_ontology:affects_constraint(award_system_voter_diversity, credential_definition_gatekeeping).
narrative_ontology:affects_constraint(award_system_voter_diversity, aesthetic_consensus_bias).
narrative_ontology:affects_constraint(award_system_voter_diversity, alternative_legitimacy_pathway).

% DUAL FORMULATION NOTE:
% Award system voter diversity decomposes into credential definition (structural barriers), aesthetic gatekeeping (institutional extraction), and consensus bias (cognitive lock-in). Each component has distinct ε value and remediation pathway. This story addresses the institutional extraction component; sibling stories address credential and consensus mechanisms. All three are linked by network edges — removing one without addressing others produces incomplete remediation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(award_system_voter_diversity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
