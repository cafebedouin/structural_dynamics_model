% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority and Territorial Sovereignty (Sovereignty Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates the SOVEREIGNTY READING of the border
 *   legitimacy kernel. It answers the question: 'On what grounds can a state
 *   justifiably exclude non-citizens from its territory?' The sovereignty
 *   reading provides an answer grounded in territorial authority — states
 *   possess inherent right to determine membership and control access based
 *   on the doctrine that sovereignty entails exclusive territorial
 *   governance. This reading produces a specific structural arrangement: the
 *   state and incumbent citizens experience the border constraint as enabling
 *   legitimate coordination (resource distribution, security, governance);
 *   excluded migrants experience it as extraction with no alternative. The
 *   constraint exhibits high extractiveness (0.68) and high suppression
 *   (0.72) because the mechanism relies on sustained enforcement: legal
 *   prohibition, border patrol, detention infrastructure, deportation. The
 *   low theater ratio (0.35) reflects that enforcement is substantially
 *   functional — the state actually excludes people; it is not merely
 *   performing exclusion. However, this is a CONSTRUCTED constraint grounded
 *   in a legitimacy doctrine, not a natural law. The analytical observer
 *   recognizes that the sovereignty reading naturalizes what is historically
 *   contingent: territorial states as the primary political unit, borders as
 *   legitimate loci of authority, and membership as tied to birth or state
 *   authorization. The constraint is a 'false summit' candidate — it presents
 *   itself as inherent to political order but actually reflects a specific
 *   institutional arrangement that emerged circa 18th century and benefits
 *   those already inside the territory.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victims (powerless/trapped) — face absolute prohibition on entry with no appeal, no collective voice, no exit except to other state jurisdictions. Extractiveness maximal from their perspective.
 *   - Incumbent State Citizens: Primary beneficiaries (institutional/arbitrage) — access to state services, security, labor advantage, cultural continuity. Sovereignty doctrine legitimates their exclusionary authority.
 *   - State Institutions: Secondary beneficiary (institutional/arbitrage) — border enforcement apparatus, sovereignty doctrine, territorial administration. Maintain monopoly on legitimate exclusion.
 *   - Transnational Migrant Networks: Organized resistance (organized/constrained) — diaspora groups, advocacy coalitions, smuggling networks. Neither fully accept nor capitulate; perceive mixed extraction and coordination.
 *   - Neighboring Border Communities: Tertiary victims (moderate/constrained) — face labor competition, resource pressure, enforcement spillovers. Cost-bearers of border maintenance.
 *   - International State System: Meta-beneficiary (institutional/arbitrage) — sustains mutual recognition of territorial authority; provides framework for diplomacy and international law. Sovereignty doctrine is coordination mechanism at system level.
 *   - Analytical Observer: Recognizes constraint as false summit (analytical/analytical) — identifies that sovereignty reading naturalizes contingent institutional arrangement as inherent law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.68).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.72).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, snare).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority and Territorial Sovereignty (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, 'ad34c293-ec01-410d-b114-99637f84ed81').
narrative_ontology:cs_kernel_codification('ad34c293-ec01-410d-b114-99637f84ed81', formalized).
narrative_ontology:cs_authority_grounding('ad34c293-ec01-410d-b114-99637f84ed81', extraction).
narrative_ontology:cs_interpretation_layer_present('ad34c293-ec01-410d-b114-99637f84ed81').
narrative_ontology:cs_reading_relation('ad34c293-ec01-410d-b114-99637f84ed81', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad34c293-ec01-410d-b114-99637f84ed81', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('ad34c293-ec01-410d-b114-99637f84ed81', foundational, state_territorial_sovereignty).
narrative_ontology:cs_axiom_status(state_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ad34c293-ec01-410d-b114-99637f84ed81', state_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('ad34c293-ec01-410d-b114-99637f84ed81', foundational, exclusion_as_coordination).
narrative_ontology:cs_axiom_status(exclusion_as_coordination, holdable).
narrative_ontology:cs_axiom_grounding('ad34c293-ec01-410d-b114-99637f84ed81', exclusion_as_coordination, instrumental).
narrative_ontology:cs_reference_frame('ad34c293-ec01-410d-b114-99637f84ed81', state_territorial_authority_framework).
narrative_ontology:cs_drift_state('ad34c293-ec01-410d-b114-99637f84ed81', contemporary_mass_migration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ad34c293-ec01-410d-b114-99637f84ed81', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, incumbent_state_citizens).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, aspiring_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces absolute prohibition on entry with no appeal mechanism, no collective voice, and no exit option except physical relocation to another state's jurisdiction. Suppression is near-total: legal barriers, enforcement (border patrol, deportation), and informational exclusion (denied knowledge of asylum procedures, documentation requirements). Experienced extractiveness is maximal — the constraint extracts the migrant's desired location and opportunity set with no coordination benefit.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCUMBENT STATE (ROPE) — Experiences the constraint as pure coordination: maintaining territorial borders enables predictable resource allocation, security, and institutional governance. No extraction perceived by the state — the constraint solves a collective action problem (managing population, resource distribution, security). The state has full arbitrage: it can set terms of entry, negotiate exceptions, enforce selectively. The sovereignty reading legitimates the state's position as non-extractive coordination.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSNATIONAL MIGRANT NETWORKS (TANGLED ROPE) — Organized agents (diaspora groups, migrant advocacy coalitions, smuggling networks) perceive mixed extraction and coordination. The constraint enables coordination (family reunification, cultural maintenance) while imposing extraction (fees, documentation barriers, coercive smuggling as alternative pathway). These networks are constrained — they lack legal standing and face enforcement pressure — but possess some agency through informal organization. They neither fully accept the sovereignty framing nor capitulate entirely.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NEIGHBORING BORDER COMMUNITIES (SNARE) — Experience the constraint as extraction: labor competition, resource pressure from informal settlements, enforcement spillovers (police patrols, raids). They are constrained rather than trapped — some can relocate or exit the border region — but face high costs: economic dependency, family ties, regional isolation. The sovereignty constraint extracts from their communities while enforcing compliance with state immigration law they did not author.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL STATE SYSTEM (ROPE) — The global system of sovereign states perceives borders as enabling coordination at the civilizational level: mutual recognition of territorial authority, predictable framework for diplomacy, stable basis for international law. Each state's right to exclude sustains every state's right to exist as a bounded entity. From the system's perspective, the constraint is pure coordination with zero net extraction — it is the mechanism through which the international order perpetuates itself.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY READING (TANGLED ROPE) — From the analytical universal perspective, the constraint exhibits both coordination (enables state governance, sustains international legal order) and extraction (excludes those who lack territorial birth or state sponsorship). The sovereignty reading legitimates the state's exclusionary authority through the doctrine that states possess inherent right to control membership. However, this reading naturalizes a historically contingent power arrangement: territorial borders as we know them are recent institutional inventions (~18th century onwards), not timeless features of political order. The analytical observer recognizes the constraint as a hybrid: genuine coordination function for those inside, genuine extraction for those outside, with the legitimacy claim asymmetrically distributed.
constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_legitimacy__sovereignty_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): The constraint extracts substantially from excluded migrants (denial of location, opportunity, access to state resources) while providing genuine coordination benefits to the state and incumbent citizens. The value reflects asymmetry — extraction is concentrated on outsiders while coordination benefits accrue to insiders. The trajectory (0.45 → 0.62 → 0.68) shows rising extractiveness over the interval, indicating accumulation of enforcement infrastructure and intensified enforcement mechanisms as migration pressure increases. Suppression (0.72): Very high. Excluded migrants face legal prohibition, border patrol enforcement, detention, deportation, and informational exclusion (denied access to asylum procedures, documentation requirements). The suppression is both structural (legal barriers, enforcement capacity) and internalized (migrants develop expected fear of enforcement, self-censor attempts). The trajectory (0.55 → 0.68 → 0.72) shows rising suppression as enforcement intensifies — more patrols, more detention capacity, more aggressive deportation — in response to increased migration pressure. Theater Ratio (0.35): Low. The constraint is substantially functional — the state actually excludes people through legal and enforcement mechanisms. It is not primarily performative. However, some theatrical elements emerge: border ceremonies (flag displays), political spectacle around 'border security,' rhetorical framing of migrants as threats (performance). The rising trajectory (0.25 → 0.30 → 0.35) suggests modest increase in theatrical framing as political mobilization around immigration intensifies, but the core mechanism remains functional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a stark perspectival gap between the state's experience (rope — coordination) and the migrant's experience (snare — pure extraction). This gap is analytically fundamental: it reveals that 'coordination' and 'extraction' are not observer-independent properties but structural facts that depend on position within the constraint. The state genuinely solves a coordination problem (predictable resource allocation, security); the migrant genuinely experiences extraction (denial of location choice). Both perspectives are accurate from their positions; neither is privileged. The intermediate perspectives (transnational networks, border communities, international system) map different portions of the constraint's structure. The analytical observer recognizes that the sovereignty reading legitimizes the asymmetry — it treats the state's coordination need as more fundamental than the migrant's freedom of movement, thereby naturalizing what is actually a power-based distribution of legitimate authority. The false summit element: the sovereignty reading presents this arrangement as inherent to political order (mountain) when it is historically contingent (snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position — their power level, exit options, and role in the extraction flow. EXCLUDED MIGRANT: powerless + trapped → d approaches 1.0 (full target). Zero exit capacity; no coordination benefit to them from border enforcement. Maximal experienced extractiveness. INCUMBENT STATE: institutional + arbitrage → d approaches 0.0 (full beneficiary). Can set terms of entry, enforce selectively, negotiate exceptions. Perceives coordination, not extraction. MIGRANT NETWORKS: organized + constrained → d ≈ 0.5 (symmetric). Some agency (organization), some barriers (legal constraint). Mixed extraction and coordination. BORDER COMMUNITIES: moderate + constrained → d ≈ 0.7 (mostly target). Some exit (can relocate regionally) but high costs. Bear extraction costs while lacking power to change terms. INTERNATIONAL STATE SYSTEM: institutional + arbitrage → d approaches 0.0 (full beneficiary). The system's entire structure rests on mutual recognition of sovereign exclusion. ANALYTICAL OBSERVER: analytical + analytical → d ≈ 0.72 (observer's view of target position). The analytical perspective sees the migrant's structural position and recognizes the extraction; it also recognizes the false summit (sovereignty reading naturalizes contingency).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED. The constraint's extractiveness (0.68) exceeds 0.70; the mandatrophy is resolved by showing how the constraint simultaneously exhibits coordination and extraction, and how the sovereignty reading legitimizes the asymmetry. The mandatrophy dissolves when we recognize that 'coordination' and 'extraction' are position-dependent, not observer-independent. From the state's position, borders are coordination. From the migrant's position, they are extraction. The sovereignty reading makes an asymmetric legitimacy claim: it grants the state's coordination goal priority over the migrant's freedom of movement. This is not incoherent — it is a choice about whose interests warrant protection through the doctrine. The analytical observer recognizes this as a false summit: the sovereignty reading presents a contingent power arrangement (historically emergent ~18th century, institutionally specific to nation-state system) as inherent to political order. Resolving the mandatrophy requires acknowledging that the constraint is legitimately both coordinate-and-extractive, and that the legitimacy claim is doctrinally constructed rather than natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_naturalization,
    'Is territorial sovereignty an inherent feature of political order or a contingent institutional arrangement that benefits states and incumbent citizens?',
    'Historical analysis of pre-nation-state polities (empires, city-states, nomadic governance) and their treatment of movement; empirical comparison of exclusionary capacity across different institutional arrangements',
    'If inherent: sovereignty reading is a natural law, border exclusion is immutable. If contingent: sovereignty reading is a constructed doctrine that naturalizes extraction, and the constraint reclassifies as snare at the analytical level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_naturalization, conceptual, 'Whether territorial sovereignty is inherent or constructed').

omega_variable(
    migrant_harm_causation,
    'Does the state cause harm to excluded migrants (violation of rights), or do migrants bear the cost of their own failure to secure territorial access?',
    'Moral philosophy analysis: causal attribution vs luck egalitarianism; empirical assessment of whether exclusion is passive (non-admission) or active (enforcement, deportation); comparison with other exclusionary systems (caste, apartheid) where causation is inarguable',
    'If state causes harm: constraint is extraction from victims and should classify as snare from humanitarian perspectives. If migrants bear their own cost: constraint is coordination among insiders, and snare classification reflects failure of humanitarian duty rather than rights violation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_harm_causation, preference, 'Causal responsibility for migrant harm under border exclusion').

omega_variable(
    competing_rights_hierarchy,
    'When territorial sovereignty and freedom of movement conflict, which right has priority in legitimate authority ordering?',
    'Jurisprudential analysis across legal traditions; empirical study of which reading dominates in international law, human rights bodies, and state practice; normative philosophical argument about rights foundations (positive vs negative, self-determination vs individual autonomy)',
    'If sovereignty priority: this reading is validated and constraint remains snare-type from migrant perspective but rope from state perspective. If freedom of movement priority: this reading is demoted to coexisting alternative, and sibling reading gains analytical primacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_rights_hierarchy, preference, 'Priority hierarchy when sovereignty and freedom of movement collide').

omega_variable(
    enforcement_legitimacy_gap,
    'If a state''s sovereignty right to exclude is legitimate in principle, are ALL enforcement mechanisms (deportation, border patrol, detention, family separation) equally legitimate, or do some enforcement modes violate the legitimacy claim?',
    'Comparison of enforcement mechanisms across states and time periods; analysis of whether harm caused by enforcement exceeds harm of exclusion alone; assessment of whether excessive enforcement converts legitimate exclusion into illegitimate persecution',
    'If all enforcement modes are legitimate: the constraint''s suppression score reflects enforceability only. If some enforcement is illegitimate: the constraint''s suppression score overstates legitimacy, and the theater ratio should rise (enforcement performance becomes more theatrical than functional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_gap, conceptual, 'Whether all enforcement modes preserve legitimacy of exclusion').

omega_variable(
    asymmetry_of_legitimacy,
    'Does the sovereignty reading legitimately describe ONLY the state''s right to exclude, or does it equally legitimize migrant efforts to enter through coercive/deceptive means (since migrants have no legal pathway)?',
    'Analysis of whether migrants can appeal to sovereignty doctrine for their own purposes; examination of whether the reading is genuinely symmetric or privileges state power; empirical study of migrant legal strategy deployment',
    'If asymmetric: the constraint legitimizes state coercion while delegitimizing migrant coercion, masking a power imbalance. If symmetric: migrant smuggling and irregular border crossing are equally legitimate appeals to autonomy, which reframes the constraint as conflict between two competing sovereignty claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_of_legitimacy, conceptual, 'Whether sovereignty reading applies symmetrically to state and migrant interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_sov_theater_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(border_sov_theater_t50, border_legitimacy__sovereignty_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(border_sov_theater_t100, border_legitimacy__sovereignty_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(border_sov_extract_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(border_sov_extract_t50, border_legitimacy__sovereignty_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(border_sov_extract_t100, border_legitimacy__sovereignty_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(border_sov_suppress_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(border_sov_suppress_t50, border_legitimacy__sovereignty_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(border_sov_suppress_t100, border_legitimacy__sovereignty_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The border_legitimacy kernel decomposes into three structurally distinct constraints corresponding to three competing readings. Each reading instantiates a different ε value and victim set. The sovereignty_reading (this constraint, ε=0.68) places excluded migrants in victim set and legitimizes state enforcement as coordination. The freedom_of_movement_reading (ε estimated ~0.75+) places all non-admitted in victim set and delegitimizes sovereignty doctrine. The humanitarian_obligation_reading (ε estimated ~0.50) splits migrants into protected (asylum seekers) and unprotected (economic migrants), distributing legitimacy asymmetrically. All three are linked as reading_relations in cs_structure: sovereignty_reading coexists_with the other two; each influences the others' operating environment without foreclosing them. The three stories together model how a single kernel can generate multiple constraint types depending on which reading anchors legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
