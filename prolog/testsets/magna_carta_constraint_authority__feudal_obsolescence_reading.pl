% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_feudal_obsolescence, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta as Feudal Obsolescence: Authority Claim Enabling Modern Executive Extraction
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   The feudal-obsolescence reading of Magna Carta's authority declares that
 *   the 1215 Charter was a specific response to 13th-century feudal
 *   grievances and therefore lacks binding authority over modern sovereign
 *   states whose legal and social structures have fundamentally changed.
 *   Under this reading, Magna Carta is a historical artifact — culturally
 *   important as a precedent for constitutional thinking, but structurally
 *   inapplicable to modern governance. The reading has substantial
 *   contemporary appeal: it resolves the interpretive puzzle of how a
 *   800-year-old document could regulate 21st-century state action, and it
 *   appears to reflect historical accuracy — feudalism is gone, the baronial
 *   context is extinct, and the specific grievances are anachronistic.
 *   However, the reading creates a structural problem: if Magna Carta's
 *   restraints are obsolete, what principle restrains executive power in
 *   common-law jurisdictions? The reading offers no successor principle. This
 *   gap reveals the constraint's true function: it operates as a one-way
 *   ratchet, clearing space for executive discretion while maintaining the
 *   symbolic authority of constitutional tradition. The feudal-obsolescence
 *   claim simultaneously naturalizes executive power maximization (by
 *   declaring the alternative restraint anchor obsolete) and obscures this
 *   function (by framing the claim as historical accuracy rather than
 *   political choice). The constraint's extractiveness increases over the
 *   interval (0.32 → 0.58) as courts apply the obsolescence reasoning to
 *   progressively broader domains, and the theater ratio rises (0.35 → 0.68)
 *   as invocations of Magna Carta become increasingly ceremonial rather than
 *   functionally limiting.
 *
 * KEY AGENTS:
 *   - Executive Power Structures: Primary beneficiary (institutional/arbitrage) — gains maximal discretion when Magna Carta's restraints are declared feudal and inapplicable
 *   - Juridical Restraint Tradition: Primary victim (powerless/trapped) — loses foundational anchor if Magna Carta becomes merely historical reference
 *   - Popular Constitutionalism: Secondary victim (moderate/constrained) — constrained by lack of enforceable principle when the restraint foundation is declared obsolete
 *   - Constitutional Law Scholarship: Organized actor (organized/constrained) — maintains performative citation while accepting that the Charter is not binding; high theater, atrophied function
 *   - Comparative Constitutional Analysis: Analytical perspective (analytical/analytical) — simultaneously affirms principle of written restraints while exempting the specific historical case
 *   - Living-Constitutionalist Tradition: Competing reading held by different jurisprudential faction — coexists with feudal-obsolescence reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.58).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta as Feudal Obsolescence: Authority Claim Enabling Modern Executive Extraction").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a').
narrative_ontology:cs_kernel_codification('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', fixed_text).
narrative_ontology:cs_authority_grounding('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', lineage).
narrative_ontology:cs_interpretation_layer_present('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a').
narrative_ontology:cs_reading_relation('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', foundational, historical_context_boundedness).
narrative_ontology:cs_axiom_status(historical_context_boundedness, holdable).
narrative_ontology:cs_axiom_grounding('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', historical_context_boundedness, empirically_contingent).
narrative_ontology:cs_axiom('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', foundational, institutional_modernity_incompatibility).
narrative_ontology:cs_axiom_status(institutional_modernity_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', institutional_modernity_incompatibility, deontological).
narrative_ontology:cs_reference_frame('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', feudal_baronial_restraint_framework).
narrative_ontology:cs_drift_state('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', contemporary_nation_state_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e6f8d757-c3bc-49ed-990e-ae7d1e1cb32a', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power_structures).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_tradition).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JURIDICAL RESTRAINT TRADITION (SNARE) — Trapped between the reading's claim of feudal obsolescence and the normative requirement to defend limited government. If Magna Carta is merely feudal artifact, the tradition has no anchor; if still binding, the reading's premise fails. Cannot exit without abandoning its own foundational commitments. Bears full cost of the obsolescence claim — restraint becomes optional when its historical source is declared obsolete.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POPULAR CONSTITUTIONALISM / ORGANIZED CITIZENS (SNARE) — Constrained by lack of enforcement mechanism if the foundation (Magna Carta as living restraint) is declared obsolete. Can organize and speak, but the structural argument undercuts their epistemic authority to claim restraint. Exit would require either: (a) finding alternative constitutional foundation (costly, uncertain), or (b) accepting executive discretion as legitimate. Significant extraction — the obsolescence reading transfers authority from popular sovereignty to state apparatus.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE POWER STRUCTURE (ROPE) — Benefits from the reading's core claim. If Magna Carta is feudal artifact, executive discretion is maximized and constrained only by positive law (which the executive often controls or influences through enforcement mechanisms). Experiences the constraint as coordination — communicating the constraint's modern irrelevance solves the problem of political legitimacy for executive action. Arbitrage exit: can switch between framing actions as 'within Magna Carta restraints' (when expedient) and 'beyond feudal constraints' (when restraint is inconvenient). Net beneficiary.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL LAW SCHOLARSHIP (PITON) — Organized actors (law professors, jurists, courts) maintain ritualistic invocations of Magna Carta's 'historical importance' while simultaneously declaring it feudal and inapplicable. High theater: ceremonial mention without functional restraint. The scholarship has atrophied from producing genuine limits (Coke, the 17th century) to producing historical narrative (Magna Carta as foundational symbol, not binding constraint). Sunset clause implicit: constitutional discourse could abandon the historical theater and operate directly from modern statutory frameworks, but institutional inertia maintains the performative citation.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPARATIVE CONSTITUTIONAL ANALYSIS (TANGLED ROPE) — From a generational/global analytical position, the reading simultaneously affirms a genuine coordination function (Magna Carta as historical precedent for written constraints on power) and enables extraction (declaring the specific constraint obsolete while preserving the precedent's symbolic authority). The reading coordinates modern constitutionalism around the principle that written limits matter — while extracting exemption from those limits by historicizing away the specific case. Moderate-to-high extractiveness because the coordination is real but the asymmetry is substantial.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal perspective, the reading appears to describe an immutable law of history: feudal constraints become obsolete when feudalism ends; 13th-century baronial compacts cannot bind modern sovereign states. This perspective treats the obsolescence as a structural necessity rather than a contingent claim. However, the reading contains identifiable beneficiaries (executive power) who benefit from declaring the constraint obsolete, making this a false summit candidate — the 'inevitable obsolescence' naturalizes what is actually a choice about which constraints remain binding.
constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_constraint_authority__feudal_obsolescence_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The reading creates substantial asymmetry between executive power (which benefits from the obsolescence claim) and restraint traditions (which lose argumentative ground). The extraction is not total (0.66+) because courts still invoke Magna Carta symbolically, limiting crude executive discretion. The reading allows executives to claim Magna Carta compliance while ignoring its specific restraints — arbitrage extraction. Suppression (0.62): Moderate-high. Significant barriers prevent popular constitutionalism from asserting restraint: (a) the reading claims historical authority (courts cite it regularly), making alternative claims appear ignorant or politicized; (b) the tradition of judicial deference to executive power is institutionalized; (c) alternative restraint anchors (statutory limits, parliamentary oversight) are weaker than constitutional principle. Theater ratio (0.68): High. Continued invocation of Magna Carta in judicial opinions and constitutional discourse occurs despite the reading's claim that it is feudal and inapplicable. Courts treat citation as ceremonially important but doctrinally non-binding — performative rather than limiting. The theater has increased from 0.35 (when Magna Carta was still treated as genuinely binding in the common law) to 0.68 (when it is cited primarily for historical legitimacy). The measurement trajectory shows progressive atrophy: as the feudal-obsolescence reading gained acceptance in jurisprudence, actual functional restraint declined while symbolic citation increased — the classic piton signature combined with snare-like extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the executive's rope perspective and the restraint tradition's snare perspective is the diagnostic core. From the executive's view, the obsolescence reading is coordination — it clarifies that modern governance need not be bound by feudal categories, enabling efficient state action. From the restraint tradition's view, it is pure extraction — the same reading that clears space for executive discretion also removes the founding principle that justified restraint. The snare classification at powerless/trapped reflects that once the feudal-obsolescence claim takes hold, juridical restraint has no alternative anchor and cannot exit. The piton classification at the scholarly level reflects that constitutional law maintains the ritual while abandoning the function. The mountain classification at the analytical/civilizational level risks naturalizing the reading as historical necessity rather than political choice — the false summit detector identifies that executive power has clear beneficiary interest in the reading's acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness derives from their structural relationship to the obsolescence claim. Executives (institutional/arbitrage) have low d: they benefit from the reading, so d ≈ 0.10, producing low χ (they experience the constraint as enabling). Restraint traditions (powerless/trapped) have high d: they lose argumentative ground, so d ≈ 0.90, producing high χ (snare perspective). Scholars (organized/constrained) have moderate d: they maintain the symbol while accepting obsolescence, d ≈ 0.55, producing moderate χ and piton classification. The analytical observer (analytical/analytical) uses canonical d ≈ 0.73, producing high χ and the appearance of mountain classification that the false summit detector undermines. The directionality overrides are not needed — the structural data (beneficiaries = executives; victims = restraint tradition and popular constitutionalism) automatically derive correct d values for each perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_scope_boundary,
    'Does ''feudal obsolescence'' refer to the feudal social structure (which is genuinely obsolete), the feudal legal framework (which persists in property law), or the constitutional principle of restraint on executive power (which may be trans-feudal)?',
    'Distinction analysis: (a) if obsolescence is about feudal tenure systems, the reading does not logically entail that Magna Carta''s restraint clauses are obsolete; (b) if about the feudal hierarchy, parliamentary sovereignty and common law have already superseded feudalism without declaring Magna Carta void; (c) if about the principle itself, the reading''s claim is categorical but empirically contingent on defining which principles are ''feudal''.',
    'If scope is narrow (feudal social structure): the reading is consistent with living constitutionalism; Magna Carta''s restraint content survives the feudalism critique. If scope is broad (all pre-modern law): the reading must explain why common law and parliamentary sovereignty are not equally obsolete. If scope is about the principle: the reading must defend that executive discretion is a legitimate evolutionary endpoint, not merely that feudalism is.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_scope_boundary, conceptual, 'Definitional ambiguity: what counts as feudally-obsolete?').

omega_variable(
    constraint_continuity_paradox,
    'If Magna Carta is feudal artifact and therefore obsolete, how do modern courts, constitutions, and legal traditions justify continuous citation and invocation of its principles?',
    'Genealogical analysis: trace which specific clauses of the 1215 Charter persist in modern law and which have genuinely lapsed. Courts citing Magna Carta either (a) appeal to continuity of principle across institutional change (contradicting pure obsolescence), or (b) treat citation as performative/symbolic (suggesting piton classification rather than snare).',
    'If continuous principles explain citation: the reading''s obsolescence claim is partial/conditional, not categorical. If citation is purely performative: the constraint is piton (atrophied function maintained by theater), and the extraction mechanism is different (institutional inertia rather than active executive maximization). The snare classification depends on finding active extraction; pure theater points to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_continuity_paradox, empirical, 'Whether Magna Carta principles show genuine continuity or only performative citation').

omega_variable(
    alternative_restraint_anchors,
    'If Magna Carta is declared feudal and therefore obsolete, what constitutional/legal principle replaces it as the anchor for restraint on executive power in common-law jurisdictions?',
    'Doctrinal analysis: identify the positive law or constitutional principle that the reading offers as Magna Carta''s successor. If no successor is explicitly offered, the reading tacitly assumes executive discretion limited only by statute (which the executive influences). If a successor is offered, evaluate whether it provides equivalent or greater restraint.',
    'If no successor offered: the reading functions as a one-way argument for executive maximization; snare classification confirmed. If successor offered: evaluate whether the reading is proposing genuine constitutional evolution (tangled rope) or merely substituting a weakly-enforceable principle (snare). The absence of a successor principle is strong evidence that extraction (executive benefit from obsolescence claim) rather than coordination (constitutional evolution) is the primary function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_restraint_anchors, conceptual, 'What successor principle anchors restraint if Magna Carta is obsolete?').

omega_variable(
    kernel_reading_contest,
    'This reading is one of three competing interpretations of the Magna Carta kernel. Does the feudal-obsolescence reading logically foreclose the living-constitutionalism reading, coexist with it, or merely influence the conditions under which it operates?',
    'Logical structure analysis: (a) forecloses if the reading''s core premise (feudal artifact → not binding on modern states) directly contradicts living constitutionalism''s core premise (principles evolve across contexts while remaining binding); (b) coexists if both readings remain live positions held by different judicial factions; (c) influences if the reading creates epistemic or resource pressure on living constitutionalism without logically eliminating it.',
    'Classification determines how the constraint family is modeled. Foreclosure is rare and requires mutual logical contradiction. Coexistence is most likely — the readings map to different jurisprudential schools (originalism vs evolutionism). Influence is accurate if the obsolescence reading dominates contemporary jurisprudence and makes living-constitutionalist arguments harder to sustain (epistemic pressure via authority capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relationship between feudal-obsolescence and living-constitutionalism readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_feudal_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mc_feudal_tr_t200, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 200, 0.52).
narrative_ontology:measurement(mc_feudal_tr_t400, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 400, 0.68).

% Extraction over time
narrative_ontology:measurement(mc_feudal_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mc_feudal_be_t200, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(mc_feudal_be_t400, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mc_feudal_su_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mc_feudal_su_t200, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 200, 0.54).
narrative_ontology:measurement(mc_feudal_su_t400, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_prerogative_restraint__common_law).

% DUAL FORMULATION NOTE:
% The Magna Carta authority constraint family decomposes into three readings with different ε values reflecting different empirical claims about what binding the Charter does or does not perform. This reading (feudal-obsolescence) has ε=0.58 because the reading actively enables extraction by removing a restraint anchor. The living-constitutionalism reading has ε≈0.30-0.40 because it preserves restraint while updating its application. The parliamentary-sovereignty reading has ε≈0.25-0.35 because it substitutes an alternative (parliamentary) restraint mechanism. Each story gets its own perspectives, beneficiary/victim declarations, and measurements. They are linked by network.affects_constraints to enable contamination analysis: if the feudal-obsolescence reading's authority erodes (e.g., through historical reappraisal), the executive-prerogative constraint may shift classification because its restraint foundation is affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
