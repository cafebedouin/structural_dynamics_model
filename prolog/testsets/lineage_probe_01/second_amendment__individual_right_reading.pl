% ============================================================================
% CONSTRAINT STORY: second_amendment__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment__individual_right_reading, []).

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
 *   constraint_id: second_amendment__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional_law/fundamental_rights
 *
 * SUMMARY:
 *   The Second Amendment individual-right reading construes the amendment to
 *   protect a pre-existing, natural right of individuals to possess arms for
 *   self-defense, with the militia clause serving as a prefatory statement of
 *   purpose rather than a limiting condition. This reading became dominant
 *   U.S. constitutional doctrine via District of Columbia v. Heller (2008)
 *   and McDonald v. City of Chicago (2010). The constraint operates as a
 *   tangled rope: it coordinates legitimate interests (individual security,
 *   constitutional stability, predictable legal boundaries) while
 *   simultaneously extracting from comprehensive-regulation jurisdictions
 *   that would prefer to implement broad licensing, magazine restrictions,
 *   and assault-weapon bans. The structural delta from the competing
 *   collective_militia_reading is stark: the individual-right reading
 *   suppresses broad regulation by shifting the burden of proof to the state
 *   to demonstrate narrowly-tailored compelling interest under strict or
 *   intermediate scrutiny (Heller framework). The measurement trajectory
 *   shows rising extractiveness (0.35 → 0.58) as the doctrine has matured
 *   from a doctrinal hypothesis to settled law, with corresponding increases
 *   in suppression of alternative regulatory regimes. Theater ratio remains
 *   relatively low (0.35) because the core constraint is substantive — the
 *   individual right exists and must be accommodated — rather than
 *   performative, though the originalist methodological justification
 *   exhibits significant theatrical elements.
 *
 * KEY AGENTS:
 *   - Individual gun owners (powerful/arbitrage): Primary beneficiary — experience the constraint as enabling self-defense, property ownership, and constitutional protection. Institutional backing enables litigation to preserve the right.
 *   - Comprehensive-regulation jurisdictions (institutional/constrained): Primary victim — suppressed from implementing licensing, magazine, and weapons restrictions that their democratic processes prefer. Constrained by constitutional doctrine (Heller) that forecloses broad regulation.
 *   - High-violence urban communities (powerless/trapped): Secondary victim — bear costs of distributed gun availability without exit capacity or institutional recourse. No voice in the constitutional interpretation that prioritizes individual self-defense.
 *   - Gun-regulation reform coalition (organized/mobile): Secondary actor — sees the constraint as negotiable through constitutional amendment, electoral pressure, or doctrinal narrowing. Has agency and exit pathways.
 *   - Originalist jurisprudence establishment (institutional/constrained): Institutional actor maintaining the methodological framework that justifies the individual-right reading. Performs originalist analysis with selective historical evidence.
 *   - Analytical observer (analytical/analytical): Risks naturalizing the individual-right reading as a natural law rather than recognizing it as a contingent doctrinal choice grounded in institutional power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment__individual_right_reading, 0.62).
domain_priors:theater_ratio(second_amendment__individual_right_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment__individual_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment__individual_right_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment__individual_right_reading, "constitutional_law/fundamental_rights").

domain_priors:requires_active_enforcement(second_amendment__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment__individual_right_reading, '522cb11d-0fd3-4c0c-8448-4dbbaa4145e3').
narrative_ontology:cs_kernel_codification('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', formalized).
narrative_ontology:cs_authority_grounding('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', lineage).
narrative_ontology:cs_interpretation_layer_present('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3').
narrative_ontology:cs_reading_relation('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', second_amendment__collective_militia_reading, forecloses).
narrative_ontology:cs_axiom('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', foundational, individual_natural_self_defense_right).
narrative_ontology:cs_axiom_status(individual_natural_self_defense_right, holdable).
narrative_ontology:cs_axiom_grounding('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', individual_natural_self_defense_right, deontological).
narrative_ontology:cs_axiom('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', foundational, prefatory_clause_announce_not_limit).
narrative_ontology:cs_axiom_status(prefatory_clause_announce_not_limit, holdable).
narrative_ontology:cs_axiom_grounding('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', prefatory_clause_announce_not_limit, empirically_contingent).
narrative_ontology:cs_reference_frame('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', founding_era_individual_armament).
narrative_ontology:cs_drift_state('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', contemporary_post_heller_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('522cb11d-0fd3-4c0c-8448-4dbbaa4145e3', '').
narrative_ontology:cs_kernel_id(second_amendment__individual_right_reading, second_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment__individual_right_reading, comprehensive_regulation_jurisdictions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARMED INDIVIDUAL (ROPE) — Experiences the constraint as enabling coordination: the legal recognition of self-defense right provides a stable framework for personal security, lawful ownership, and community defense. The constraint coordinates multiple legitimate interests (personal safety, lawful property ownership, deterrence of government overreach) with minimal coercive overhead from the individual's standpoint. Arbitrage exit (can litigate if right is violated) and powerful institutional backing (courts, constitutional tradition) make this a genuine coordination mechanism rather than extraction.
constraint_indexing:constraint_classification(second_amendment__individual_right_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: HIGH-VIOLENCE URBAN COMMUNITIES (SNARE) — Trapped in communities experiencing endemic gun violence. The individual-right reading prioritizes the armed person's self-defense over the community's collective safety. No exit: cannot leave the geographic space, cannot organize the gun supply, cannot opt into a different legal regime. Bears costs (injury, death, fear) with no corresponding benefit. The constraint suppresses alternatives to individual-level response (comprehensive regulation, supply reduction, violence interruption programs). Maximum experienced extraction from the perspective of those harmed by distributed gun availability.
constraint_indexing:constraint_classification(second_amendment__individual_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: COMPREHENSIVE-REGULATION JURISDICTIONS (TANGLED ROPE) — Constrained by constitutional doctrine (Heller, McDonald) that forecloses broad licensing, magazine restrictions, and assault-weapon bans at the individual level. But also benefit from the coordination function: the individual-right reading enables predictable litigation outcomes, reduces legislative whipsawing between permissive and restrictive regimes, and provides a stable baseline for public health planning within permitted scope (background checks, age restrictions, certain regulatory niches). Extraction runs toward gun-rights coalitions (lower burden of proof to strike down regulations), but genuine coordination persists (predictable doctrinal scope).
constraint_indexing:constraint_classification(second_amendment__individual_right_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GUN-REGULATION REFORM COALITION (SCAFFOLD) — Organized actors (mayors, public health advocates, ballot-initiative movements) see the constraint as temporary, negotiable, and subject to constitutional amendment or doctrinal narrowing. They have agency (electoral power, media capacity, legislative pathways in permissive states) and see an exit path: constitutional amendment via the Article V process or Heller doctrinal limitation via future litigation and appointments. Sunset logic: as demographics shift and state-level experimentation accumulates, political pressure for constitutional revision or doctrinal recalibration builds. Low effective extraction because the coalition has exit capacity and sees the constraint as structurally contingent.
constraint_indexing:constraint_classification(second_amendment__individual_right_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the right to self-defense and the possession of arms for that purpose might appear as a natural law or pre-political right — grounded in the natural capacity of humans to defend themselves and in the principle that rights inhere to individuals qua individuals, not as members of organized collectives. This reading treats the individual-right reading as discovering a pre-existing natural law rather than constructing a legal doctrine. However, the structural data (beneficiaries, victims, institutional enforcement required) suggests this is a false summit: the constraint naturalizes what is actually a contested doctrinal interpretation grounded in institutional power.
constraint_indexing:constraint_classification(second_amendment__individual_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ORIGINALIST JURISPRUDENCE ESTABLISHMENT (PITON) — The originalist methodology that grounds the individual-right reading (historical sources, founding-era meaning, textualism) has become institutionalized but is largely performative in its application. Originalist scholars and judges apply selective historical evidence, calibrate inquiry to reach preferred outcomes, and diverge substantially from non-originalist peers on which sources count as authoritative. The methodology persists through institutional inertia and professional network effects rather than because it produces uniquely compelling or falsifiable results. Theater ratio reflects this performative dimension: the ritual of originalist interpretation matters more than the empirical rigor of the historical reconstruction.
constraint_indexing:constraint_classification(second_amendment__individual_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment__individual_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment__individual_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment__individual_right_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment__individual_right_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The individual-right reading extracts from comprehensive-regulation jurisdictions by foreclosing their preferred regulatory regimes and shifting constitutional burden of proof. The extraction is not maximal (snare level) because the reading also provides genuine coordination benefits: predictable doctrinal boundaries, enabling litigation, stabilizing self-defense as a legal category. The reading benefits the armed individual by providing robust constitutional protection with institutional backing (courts, precedent). Suppression (0.62): High. The reading suppresses alternative regulations by requiring strict or intermediate scrutiny and imposing strict-construction doctrine. Urban communities seeking to reduce gun violence through supply-side regulation are suppressed from implementing comprehensive regimes. Alternative public-health approaches to violence reduction are structurally disfavored relative to individual-level armed response. However, suppression is not total — background checks, age restrictions, and some other categories survive Heller scrutiny. Theater ratio (0.35): Low-moderate. The core substantive constraint (individual self-defense right exists) is real and enforced through genuine litigation. But the originalist historical justification exhibits significant theatrical elements: selective use of founding-era sources, construction of historical consensus where disagreement existed, performative application of originalist methodology by judges with political commitments. The reading's durability rests partly on substantive constitutional principle and partly on institutional entrenchment of originalism in legal academia and judiciaries.
 *
 * PERSPECTIVAL GAP:
 *   The individual-armed-person perspective sees coordination (Rope): stable framework for lawful ownership, self-defense, and constitutional rights. The high-violence-community perspective sees extraction (Snare): trapped in a legal regime that prioritizes individual gun possession over collective safety. The comprehensive-regulation jurisdiction perspective sees constrained extraction (Tangled Rope): structurally suppressed from preferred regulations but also coordinated by predictable doctrinal boundaries. The reform coalition perspective sees a temporary scaffold: negotiable through amendment or doctrinal narrowing. The originalist establishment perspective sees performative ritual (Piton): the methodology persists through institutional inertia. The analytical observer perspective risks mountain misclassification: naturalizing a contested doctrinal reading as natural law. The perspectival gap reveals that the same constitutional text produces radically different experienced constraints depending on the observer's structural position and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations plus exit options. Individual gun owners are beneficiaries with arbitrage exit (can litigate); this produces low d → negative χ (they experience the constraint as enabling, not extracting). Comprehensive-regulation jurisdictions are victims with constrained exit (can litigate but face unfavorable doctrine); this produces high d → high χ (they experience extraction). High-violence communities are victims with trapped exit (no litigation pathway, no alternative legal regime available); this produces maximum d → maximum χ (they experience maximum extraction). The reform coalition is organized with mobile exit; this produces moderate d → moderate χ. Originalist jurists are institutional beneficiaries with arbitrage exit (can cite precedent, shape doctrine through appointments); this produces low d. The cannon of directionality values reflects that the individual-right reading concentrates benefits on armed individuals and extracts from comprehensive-regulation and high-violence communities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_interpretation,
    'Does the militia clause (prefatory clause) constrain the operative clause, or does it merely announce a purpose without limiting scope?',
    'Linguistic analysis of 18th-century drafting conventions, comparative textual study of other constitutional provisions with prefatory clauses, examination of founding-era militia doctrine and its relationship to private armed possession',
    'If militia clause constrains: the collective_militia_reading is correct and this reading is foreclosed. If militia clause is merely prefatory: this reading''s operative clause is unconstrained, supporting broad individual-right protection. This is the kernel-level disagreement between the two readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_interpretation, conceptual, 'Whether the militia clause constrains the operative right or merely announces purpose').

omega_variable(
    self_defense_scope_limits,
    'What weapons, training standards, and licensing regimes are compatible with the individual-right reading? Where does self-defense utility become mere accumulation?',
    'Common-law self-defense doctrine analysis; empirical testing of weapons effectiveness for personal defense vs. mass casualty; comparative international law on self-defense right scope; jurisdictional variation in Heller post-hoc regulations and their survival rates',
    'Narrow self-defense scope: permits extensive subcategories of regulation (licensing, training, safe-storage mandates, magazine limits, assault-weapon restrictions). Broad self-defense scope: forecloses most regulations. Influences extractiveness value and suppression magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_scope_limits, empirical, 'Scope of weapons and training compatible with legitimate self-defense').

omega_variable(
    empirical_self_defense_efficacy,
    'How often and under what conditions does armed self-defense actually prevent harm vs. increase injury or lethality?',
    'Meta-analysis of self-defense outcomes; comparative injury/fatality rates in defensive-gun-use scenarios vs. alternative responses; randomized trials where feasible (unlikely); observational studies with causal inference methods',
    'If defensive-gun-use is net-protective: self-defense narrative is empirically sound, supporting individual-right reading''s beneficiary/victim distinction. If net-harmful or inconclusive: the individual-right reading relies on a deontological self-defense right uncoupled from empirical protective effects, shifting the reading toward reliance on natural-right axioms rather than efficacy. Influences the ''self-defense'' framing''s persuasiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_self_defense_efficacy, empirical, 'Whether armed self-defense produces net protective outcomes vs. increases injury').

omega_variable(
    reading_contest_logical_structure,
    'Are the individual_right_reading and collective_militia_reading logically incompatible (foreclosure) or do they represent different emphasis within a single permissible constitutional framework (coexistence)?',
    'Strict logical analysis of the two core premises: (1) ''the right to keep and bear arms belongs to individuals as individuals'' vs (2) ''the right to keep and bear arms belongs to the people in their militia capacity.'' Can both be true in the same framework? Does rejecting one require rejecting the other? Historical-institutional analysis of whether courts could hold both positions simultaneously without internal contradiction.',
    'Foreclosure: one reading''s fundamental premise rules out the other; courts cannot coherently apply both. Coexistence: the readings emphasize different aspects of a multi-valued constitutional commitment; different interpretive communities can hold both positions. Influences the reading_relations classification (forecloses vs coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_logical_structure, conceptual, 'Logical compatibility of individual-right vs collective-militia readings').

omega_variable(
    originalism_empirical_foundation,
    'Does the historical evidence actually support the individual-right reading, or does the originalist methodology produce confirmatory bias toward that reading?',
    'Systematic analysis of founding-era sources: state constitutions, militia laws, founding-era individual gun ownership rates, self-defense doctrine, regulatory precedents. Cross-examination by non-originalist historians. Comparison of Heller''s historical claims to scholarly consensus on 18th-century gun-bearing practices.',
    'If historical evidence is robust: originalism is epistemically sound and the reading is grounded in empirical fact. If evidence is selective or contested: the originalist grounding is performative, and the reading relies more heavily on deontological axioms. Affects piton classification (theater magnitude).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_empirical_foundation, empirical, 'Whether historical evidence robustly supports the individual-right interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment__individual_right_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_pre_originalism, second_amendment__individual_right_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(theater_t5_originalism_ascendant, second_amendment__individual_right_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(theater_t10_established_methodology, second_amendment__individual_right_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(extract_t0_heller_era, second_amendment__individual_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_t5_post_mcdonald, second_amendment__individual_right_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(extract_t10_contemporary, second_amendment__individual_right_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(suppress_t0_pre_heller, second_amendment__individual_right_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(suppress_t5_post_mcdonald, second_amendment__individual_right_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(suppress_t10_contemporary, second_amendment__individual_right_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment__individual_right_reading, second_amendment__collective_militia_reading).
narrative_ontology:affects_constraint(second_amendment__individual_right_reading, gun_violence_suppression_mechanism).
narrative_ontology:affects_constraint(second_amendment__individual_right_reading, urban_self_defense_paradox).

% DUAL FORMULATION NOTE:
% The individual-right reading and collective-militia reading are two structurally distinct constraints instantiated by the same constitutional kernel. Their ε values differ substantially (0.58 vs. estimated ~0.35) because they define different victim sets and suppression mechanisms. The individual-right reading suppresses broad regulation (high extraction from regulation jurisdictions); the collective-militia reading suppresses individual gun accumulation (high extraction from gun owners). They are not two measurements of the same constraint — they are two different constraints grounded in conflicting interpretations of a single ambiguous text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment__individual_right_reading, powerless, 0.96).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
