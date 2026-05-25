% ============================================================================
% CONSTRAINT STORY: individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_right_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Firearms Ownership Decoupled from Militia Service
 *   domain: constitutional_law/rights_jurisprudence/political_theory
 *
 * SUMMARY:
 *   The individual right reading of the Second Amendment — holding that the
 *   amendment protects a personal right to firearm ownership independent of
 *   militia service — is ONE CONTESTED INTERPRETATION of a constitutional
 *   kernel. This constraint story models only this reading; sibling readings
 *   (collective militia right, civic participation right) are separate
 *   constraint stories with different ε values and beneficiary/victim
 *   structures. The individual right reading, endorsed as Supreme Law by
 *   District of Columbia v. Heller (2008) and McDonald v. Chicago (2010),
 *   generates a tangled hybrid of coordination and extraction. It coordinates
 *   around a clear constitutional baseline: individuals have a fundamental
 *   right to keep firearms for lawful purposes, especially self-defense. This
 *   clarity enables political mobilization, litigation strategy, and market
 *   stability for manufacturers. Simultaneously, it extracts from
 *   constituencies bearing elevated violence risk: the reading constrains
 *   regulatory response through strict scrutiny doctrine, asymmetrically
 *   privileging individual gun rights over competing rights to life and
 *   bodily security. The measurement trajectory shows extractiveness rising
 *   from 0.32 (when the reading was minority doctrine, pre-2008) to 0.58
 *   (post-Heller, as the doctrine became institutionalized across lower
 *   courts). Theater ratio remains low (0.35) because the doctrine, while
 *   contested, is not performative — courts genuinely apply strict scrutiny
 *   and sometimes strike down regulations. The doctrine's authority is
 *   increasingly maintained through repetition of the originalist discovery
 *   narrative (Piton perspective), but the underlying judicial action is
 *   substantive enforcement rather than ritual.
 *
 * KEY AGENTS:
 *   - Gun Owners and Second Amendment Advocates: Primary beneficiary (institutional/arbitrage) — experience the reading as coordination + rights confirmation; mobilized political constituency with high exit flexibility
 *   - Firearm Manufacturers and Industry: Primary beneficiary (institutional/arbitrage) — protected market, expanded addressable population, litigation defense against regulations
 *   - Gun Violence Victims and Affected Communities: Primary victim (powerless/trapped) — disproportionately bear violence risk; trapped in high-violence jurisdictions; no exit option; no voice in interpretive authority
 *   - Public Health and Safety Constituencies: Secondary victim (moderate/constrained) — regulatory capacity constrained by strict scrutiny; benefit from clarity of doctrine but constrained to design within strict scrutiny gates
 *   - State and Local Regulatory Authorities: Secondary actor (organized/constrained) — must operate within constitutional boundary established by individual right reading; locked into strict scrutiny framework
 *   - Federal Judiciary: Institutional enforcer (powerful/mobile) — maintains doctrine through continuous precedent; has doctrinal mobility but faces political cost to revision
 *   - Originalist Constitutional Interpretation Framework: Meta-level beneficiary (institutional/arbitrage) — authority claim depends on narrative of historical discovery; increasingly reliant on performative repetition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_right_reading, 0.58).
domain_priors:suppression_score(individual_right_reading, 0.48).
domain_priors:theater_ratio(individual_right_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(individual_right_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(individual_right_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_right_reading, tangled_rope).
narrative_ontology:human_readable(individual_right_reading, "Second Amendment Individual Right Reading: Firearms Ownership Decoupled from Militia Service").
narrative_ontology:topic_domain(individual_right_reading, "constitutional_law/rights_jurisprudence/political_theory").

domain_priors:requires_active_enforcement(individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(individual_right_reading, fixed_text).
narrative_ontology:cs_authority_grounding(individual_right_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(individual_right_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_right_reading, gun_owners).
narrative_ontology:constraint_beneficiary(individual_right_reading, firearm_manufacturers).
narrative_ontology:constraint_beneficiary(individual_right_reading, gun_rights_advocates).
narrative_ontology:constraint_victim(individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(individual_right_reading, public_safety_constituencies).
narrative_ontology:constraint_victim(individual_right_reading, urban_communities_disproportionately_affected).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUN VIOLENCE VICTIMS (SNARE) — Trapped in jurisdictions where the individual right reading constrains regulatory response. Cannot exit without relocation; bear extraction via elevated risk. No meaningful exit option; no seat in the interpretive authority that declares the right. Maximum experienced extraction — structural vulnerability weaponized.
constraint_indexing:constraint_classification(individual_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH CONSTITUENCY (TANGLED ROPE) — Constrained by constitutional doctrine (strict scrutiny on regulations) but also benefits from the clarity the individual right reading provides: clear constitutional boundary enables policy design within those constraints. Bears extraction through limited regulatory capacity; also coordinates around the settled rule. Mixed experience — agency available only within the constrained zone.
constraint_indexing:constraint_classification(individual_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GUN OWNERS & ADVOCATES (ROPE) — Institutional and organized beneficiaries who experience the constraint as pure coordination: the individual right reading clarifies entitlement and enables mobilization around defense of that entitlement. Net beneficiary with high exit flexibility (political mobilization, interstate arbitrage, litigation strategy). Low to negative experienced extraction — benefits vastly exceed costs.
constraint_indexing:constraint_classification(individual_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FIREARM MANUFACTURERS (ROPE) — Institutional beneficiary with near-total arbitrage: can operate across state lines, lobby for favorable interpretations, challenge regulations via litigation. The individual right reading protects market size and expands addressable population. Extraction asymmetry heavily favors the manufacturer — coordination benefit (clear rule, stable market) with minimal constraint cost.
constraint_indexing:constraint_classification(individual_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE/LOCAL REGULATORS (TANGLED_ROPE) — Organized actors constrained by strict scrutiny doctrine but also benefit from clear constitutional boundary: the individual right reading removes ambiguity about baseline entitlements, enabling precision in policy. Moderate extraction from loss of regulatory flexibility; moderate coordination benefit from settled rule. Trapped within constitutional doctrine but not powerless — can design within constraints.
constraint_indexing:constraint_classification(individual_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL JUDICIARY (TANGLED_ROPE) — Powerful institutional actor with genuine doctrinal mobility. The individual right reading represents one stable-state outcome but requires continuous enforcement against counter-interpretations. Judiciary maintains authority through interpretation; experiences constraint as both coordination (clear standard) and extraction (locked into path dependent precedent). Mobile exit available through doctrine revision, but political cost high.
constraint_indexing:constraint_classification(individual_right_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ORIGINALIST FRAMEWORK (PITON) — The individual right reading persists as institutional theater: originalism claims to discover immutable historical meaning, but the meaning has been actively constructed and continuously re-justified. The framework's authority derives partly from functional clarity (Rope benefit) but increasingly relies on performative repetition of the discovery narrative (theater). Theater ratio elevated because the 'historical original meaning' claim requires sustained rhetorical work to maintain against countervailing historical evidence.
constraint_indexing:constraint_classification(individual_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal perspective, individual rights to self-defense are presented as pre-political or natural law: the constraint appears immutable, transcending particular constitutional readings. However, the presence of identifiable beneficiaries (gun owners, manufacturers) and victims (affected communities) triggers false summit detection. The 'natural law' framing naturalizes a contested constitutional reading.
constraint_indexing:constraint_classification(individual_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_right_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_right_reading, TR),
    TR >= 0.70.

:- end_tests(individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.58): Moderate-high. The individual right reading extracts from gun violence victims and public health constituencies by constraining regulatory response through strict scrutiny doctrine. However, extraction is not total because: (1) strict scrutiny is a genuinely two-way gate — some regulations survive; (2) state/local regulators retain constrained agency to design policy within doctrine; (3) the beneficiary class (gun owners) experiences this as coordination (rights clarity) rather than pure extraction, meaning the overall structural asymmetry, while significant, is not maximally skewed. The rising trajectory from 0.32 to 0.58 reflects post-Heller consolidation: as the doctrine moved from contested minority to institutional baseline, its extraction mechanism deepened — more courts applied strict scrutiny, more lower-court regulations were struck down, more jurisdictional variation arose from regulatory arbitrage. SUPPRESSION (0.48): Moderate. Barriers to effective regulation exist but are not total. Affected communities face structural barriers (judicial doctrine, lobbying power asymmetry, concentrated industry resources) but also retain limited agency: ballot initiatives, state constitutional amendment, federal legislative effort (though rare post-Heller), and grassroots political mobilization. Urban gun violence victims have the lowest agency; rural gun owners and manufacturers have the highest. Suppression would be higher (0.60+) if regulatory paths were completely closed; it would be lower (0.30) if strict scrutiny were merely one factor in judicial balancing. At 0.48, it reflects genuine constraint without total closure. THEATER RATIO (0.35): Low. The doctrine is substantive, not performative. Courts genuinely apply strict scrutiny and sometimes uphold regulations (e.g., permitless carry bans, felon-in-possession laws, licensing schemes). The interpretive work is real, not theatrical. The theater ratio rises only when examining the originalist framework's authority claim (Piton perspective) — the narrative of 'historical discovery' requires sustained rhetorical work because the historical evidence is indeterminate and the reading was minority doctrine for 200 years.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between BENEFICIARY READING (gun owners, manufacturers, originalist framework: ROPE) and VICTIM READING (gun violence populations, public health regulators: TANGLED ROPE / SNARE). Both agents agree on the basic facts: the individual right reading constrains regulation and protects firearm availability. They disagree radically on whether this is coordination (beneficiary framing) or extraction + constraint (victim framing). The JUDICIARY's TANGLED ROPE position is structurally necessary — it must enforce both the individual right (via strict scrutiny) and the state's interest in regulating (by applying genuine two-tier scrutiny). But this structural compromise papers over the asymmetry: strict scrutiny applied to gun regulations is empirically more protective of gun ownership than intermediate scrutiny applied to (e.g.) abortion restrictions or voting rights regulations. The theater arises in the claim of neutrality — the doctrine claims to be applying neutral constitutional methodology when it is actually instantiating one reading with identifiable winners and losers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural relationship to this specific constraint — whether they benefit or bear extraction. GUN OWNERS (d ≈ 0.10): Strong beneficiary with high arbitrage exit; low f(d) → negative chi. MANUFACTURERS (d ≈ 0.08): Strong beneficiary with market protection; near-zero chi. GUN VIOLENCE VICTIMS (d ≈ 0.92): Full target with trapped exit; high f(d) → high chi. PUBLIC HEALTH (d ≈ 0.65): Moderate victim with constrained exit (agency within strict scrutiny); moderate f(d) → moderate chi. REGULATORS (d ≈ 0.55): Mixed (benefit from clarity, bear extraction from constraint); moderate f(d). JUDICIARY (d ≈ 0.50): Symmetric (enforces doctrine, maintains authority through enforcement); mid f(d) → moderate chi reflecting institutional stability cost. ORIGINALIST FRAMEWORK (d ≈ 0.35): Beneficiary of institutional entrenchment; low f(d) but high absolute extraction through suppression of alternative readings.
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED READING MANDATROPHY: The individual right reading resolves the mandatrophy by being explicit about its own contestedness. This is not a case where classification is indeterminate; it is a case where one clear classification is perspectivally dependent on accepting the reading itself. The mandatrophy resolves into two claims: (1) ASSUMING the individual right reading is correct constitutional law: the constraint classifies as TANGLED ROPE with moderate extraction, coordination benefit, and structured asymmetry in who experiences which. (2) QUESTIONING whether the individual right reading is the correct constitutional reading: the constraint appears as FALSE SUMMIT — a contingent historical choice naturalized as immutable law, with identifiable beneficiaries (gun owners, manufacturers) and victims (affected communities). The omega variables document the indeterminacy at the kernel level (is the reading correct?) while accepting the tangled rope classification (IF the reading is law, it functions as tangled rope). The false summit detection mechanism (beneficiaries present on a constraint claimed as natural law) fires automatically when this story is compiled, signaling the reading's contested status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint the individual right reading of the Second Amendment, or a false naturalization of what is fundamentally a contested constitutional claim?',
    'Historical analysis of alternative readings (collective militia right, civic participation right); comparative jurisprudence across democracies; recognition that the current individual right reading post-dates 2008 Supreme Court majority and was contested for 200+ years.',
    'If this is one legitimate reading among contested alternatives: constraint properly classified as Tangled Rope / False Summit candidate (beneficiaries present; naturalization framing revealed). If this is THE natural law: constraint should be Mountain without beneficiaries. The extractiveness value (0.58) and beneficiary declarations mark this as the contested-reading interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Contested constitutional reading vs. natural law claim').

omega_variable(
    strict_scrutiny_extraction_scope,
    'Does strict scrutiny doctrine genuinely protect state regulatory capacity, or does it function primarily as a veto that elevates individual gun rights above other public safety goods?',
    'Empirical analysis of regulations that survived strict scrutiny vs. those struck down; comparative application of strict scrutiny to other constitutional rights (First Amendment speech vs. Second Amendment bearing); longitudinal tracking of regulatory success rate post-Heller (2008).',
    'If strict scrutiny provides real regulatory pathway: Tangled Rope classification holds; public health actors retain constrained agency. If strict scrutiny functions as de facto veto: classify as approaching Snare; extraction intensifies and suppression rises. Current extractiveness (0.58) assumes strict scrutiny is a genuine two-way gate; empirical evidence may require upward revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_scrutiny_extraction_scope, empirical, 'Whether strict scrutiny scrutiny provides meaningful regulatory pathway or functions as veto').

omega_variable(
    interstate_regulatory_arbitrage,
    'To what degree does the individual right reading enable regulatory arbitrage (purchasing firearms in permissive states for use in restrictive states), and does this arbitrage constitute a hidden extraction channel?',
    'Forensic tracing of firearms recovered in high-restriction cities to source states; comparison of gun violence trajectories in adjacent states with different regulatory regimes; analysis of illegal interstate weapons trafficking networks.',
    'If arbitrage is significant: suppression rises (effective regulatory escape reduces victim agency); extraction rises (manufacturers and out-of-state sellers benefit from asymmetric regulatory landscape). If arbitrage is negligible: current metrics hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_regulatory_arbitrage, empirical, 'Interstate regulatory arbitrage as extraction mechanism').

omega_variable(
    militia_clause_historical_determinacy,
    'Does the historical record unambiguously support either the individual right reading or the collective militia reading as the ''original meaning''?',
    'Scholarly consensus meta-analysis of original intent scholarship; analysis of founding-era state constitutions and their interpretations; comparative review of judicial opinions citing identical historical sources to reach opposite conclusions.',
    'If history is determinate: one reading is correct, the other incorrect. Current treatment assumes indeterminacy — both readings plausible from historical evidence, and current doctrine has chosen the individual right reading as a policy choice. If one reading is demonstrably correct: the current doctrine is either legitimately discovered or falsely naturalized. Indeterminacy supports the omega itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_clause_historical_determinacy, conceptual, 'Historical determinacy of original meaning: individual vs. collective right').

omega_variable(
    regulatory_necessity_empirical_claim,
    'What is the empirical relationship between firearm availability (scope of individual right) and gun violence incidence?',
    'Cross-national comparative analysis (firearm availability vs. homicide/suicide rates in high-income democracies); within-US time-series analysis (changes in state regulations vs. violence metrics); natural experiments (changes in state law; impacts of enforcement variation on interstate trafficking).',
    'If strong empirical relationship exists (high firearm availability → high violence): public health victim status is factually grounded; extraction flows directly from the reading to affected communities. If relationship is weak or confounded: extraction mechanism operates through other channels (fear, disproportionate policing, structural inequality). Empirical clarification affects suppression valuation and victim classification credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_necessity_empirical_claim, empirical, 'Empirical causal relationship: firearm availability to violence incidence').

omega_variable(
    justiciability_of_competing_rights,
    'Can the judiciary apply equal weight to competing fundamental rights (life, bodily security vs. individual firearm ownership) under the individual right reading, or does the reading structurally privilege one over the other?',
    'Comparative doctrine analysis: application of strict scrutiny to regulations protecting life/bodily security vs. regulations restricting gun ownership; analysis of balancing tests across constitutional domains; examination of whether strict scrutiny as applied to Second Amendment differs from strict scrutiny as applied to First Amendment or Fifth Amendment takings.',
    'If weights are equal: true Tangled Rope with balanced extraction/coordination. If one right is structurally privileged: extraction asymmetry increases; classification may shift toward Snare from victim perspective; tangled rope from regulator perspective remains accurate but with higher extraction coefficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justiciability_of_competing_rights, conceptual, 'Structural priority among competing fundamental rights under individual right reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_right_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(individual_right_theater_t0, individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(individual_right_theater_t8, individual_right_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(individual_right_theater_t16, individual_right_reading, theater_ratio, 16, 0.35).

% Extraction over time
narrative_ontology:measurement(individual_right_extract_t0, individual_right_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(individual_right_extract_t8, individual_right_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(individual_right_extract_t16, individual_right_reading, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(individual_right_reading, 0.12).
narrative_ontology:affects_constraint(individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(individual_right_reading, civic_right_reading).
narrative_ontology:affects_constraint(individual_right_reading, strict_scrutiny_doctrine_second_amendment).
narrative_ontology:affects_constraint(individual_right_reading, gun_violence_epidemic).
narrative_ontology:affects_constraint(individual_right_reading, state_regulatory_capacity_constraint).

% DUAL FORMULATION NOTE:
% The Second Amendment scope kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures. The individual_right_reading (this story, ε=0.58) is upstream of and affects the strict_scrutiny_doctrine and gun_violence_epidemic constraints. The collective_right_reading and civic_right_reading (sibling stories) are alternative instantiations of the same kernel with different ε values and classifications. All three readings should be linked in the network as alternative-formulation pairs, not as sequential dependencies. This differs from the BGS constraint family (spectral universality → eigenvector thermalization) where upstream has lower confidence; here the readings are parallel — equally valid from a jurisprudential perspective, competing in the institutional arena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_right_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
