% ============================================================================
% CONSTRAINT STORY: foundational_charters__petition_of_right_1628
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_petition_of_right_1628, []).

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
 *   constraint_id: foundational_charters__petition_of_right_1628
 *   human_readable: Petition of Right (1628): Prerogative Finance and Detention Under Crown
 *   domain: political/constitutional/early_modern
 *
 * SUMMARY:
 *   The Petition of Right (1628) restates medieval liberties against a
 *   modernizing Crown engaged in novel extraction mechanisms: forced loans,
 *   arbitrary detention without trial, and forced billeting of soldiers.
 *   Charles I's ministers had escalated prerogative finance (direct Crown
 *   borrowing without parliamentary consent) as revenues from traditional
 *   customs and feudal incidents declined. The Petition re-invokes Magna
 *   Carta (1215) as precedent, asserting that the Crown cannot tax without
 *   Parliament, imprison without cause, or quarter soldiers in private homes
 *   without consent. This constraint is ONE READING of a contested kernel
 *   (the foundational_charters kernel, alongside Magna Carta 1215 and Habeas
 *   Corpus 1679). The Petition reading instantiates a specific
 *   interpretation: liberties as parliamentary supply consent and procedural
 *   detention limits, grounded in medieval feudal precedent. The constraint
 *   exhibits tangled_rope structure: it coordinates Crown revenue needs with
 *   parliamentary supply consent (genuine coordination function), while
 *   simultaneously extracting institutional cost from the Crown (prerogative
 *   powers surrendered) and suppressing Crown extraction options (forced
 *   loans abolished). The extractiveness value (0.62) reflects that
 *   suppression remains high for non-parliamentary subjects facing forced
 *   billeting and arbitrary detention, while propertied parliamentarians gain
 *   material protections. Theater rises over the interval (0.48→0.62) as the
 *   performative appeal to medieval precedent hardens; suppression
 *   requirement falls (0.72→0.64) as institutional alternatives to force
 *   emerge. The constraint is unstable: the Parliamentary Civil War
 *   (1642–1651) represents the scaffold's failure — the interim settlement
 *   does not hold.
 *
 * KEY AGENTS:
 *   - Forced Loan Subjects (powerless/trapped): Common subjects unable to petition, facing arbitrary taxation and billeting without consent or recourse
 *   - Imprisoned Gentlemen (moderate/constrained): Property-holders detained for resisting forced loans; have status to petition but no trial mechanism
 *   - Parliamentary Coalition (organized/mobile): Commons and Lords united asserting supply consent; can withhold supply as enforcement mechanism
 *   - Crown Authority (institutional/constrained): Charles I and his ministers facing revenue crisis; prerogative powers constrained by parliamentary assertion
 *   - Feudal Obligation Advocates (institutional/arbitrage): Those who use medieval precedent narrative to re-frame power dynamics; can arbitrage between 'restoring ancient law' and 'resisting innovation'
 *   - Moderate Parliamentarians (organized/mobile): Those who see Petition as temporary settlement buying time for permanent fiscal reform; expect sunset (either acceptance or Civil War)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(foundational_charters__petition_of_right_1628, 0.62).
domain_priors:suppression_score(foundational_charters__petition_of_right_1628, 0.68).
domain_priors:theater_ratio(foundational_charters__petition_of_right_1628, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(foundational_charters__petition_of_right_1628, extractiveness, 0.62).
narrative_ontology:constraint_metric(foundational_charters__petition_of_right_1628, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(foundational_charters__petition_of_right_1628, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(foundational_charters__petition_of_right_1628, tangled_rope).
narrative_ontology:human_readable(foundational_charters__petition_of_right_1628, "Petition of Right (1628): Prerogative Finance and Detention Under Crown").
narrative_ontology:topic_domain(foundational_charters__petition_of_right_1628, "political/constitutional/early_modern").

domain_priors:requires_active_enforcement(foundational_charters__petition_of_right_1628).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(foundational_charters__petition_of_right_1628, '6e052e8c-12ff-4ed6-85f2-045030114103').
narrative_ontology:cs_kernel_codification('6e052e8c-12ff-4ed6-85f2-045030114103', formalized).
narrative_ontology:cs_authority_grounding('6e052e8c-12ff-4ed6-85f2-045030114103', lineage).
narrative_ontology:cs_interpretation_layer_present('6e052e8c-12ff-4ed6-85f2-045030114103').
narrative_ontology:cs_reading_relation('6e052e8c-12ff-4ed6-85f2-045030114103', foundational_charters__magna_carta_1215, influences).
narrative_ontology:cs_reading_relation('6e052e8c-12ff-4ed6-85f2-045030114103', foundational_charters__habeas_corpus_act_1679, coexists_with).
narrative_ontology:cs_axiom('6e052e8c-12ff-4ed6-85f2-045030114103', foundational, parliamentary_supply_consent_necessary).
narrative_ontology:cs_axiom_status(parliamentary_supply_consent_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6e052e8c-12ff-4ed6-85f2-045030114103', parliamentary_supply_consent_necessary, conventional).
narrative_ontology:cs_axiom('6e052e8c-12ff-4ed6-85f2-045030114103', foundational, arbitrary_detention_impermissible).
narrative_ontology:cs_axiom_status(arbitrary_detention_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('6e052e8c-12ff-4ed6-85f2-045030114103', arbitrary_detention_impermissible, deontological).
narrative_ontology:cs_reference_frame('6e052e8c-12ff-4ed6-85f2-045030114103', medieval_feudal_liberty_restoration).
narrative_ontology:cs_drift_state('6e052e8c-12ff-4ed6-85f2-045030114103', civil_war_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6e052e8c-12ff-4ed6-85f2-045030114103', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(foundational_charters__petition_of_right_1628, foundational_charters).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(foundational_charters__petition_of_right_1628, propertied_parliamentarians).
narrative_ontology:constraint_beneficiary(foundational_charters__petition_of_right_1628, imprisoned_without_cause).
narrative_ontology:constraint_victim(foundational_charters__petition_of_right_1628, forced_loan_kingship).
narrative_ontology:constraint_victim(foundational_charters__petition_of_right_1628, crown_prerogative_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORCED LOAN SUBJECT (SNARE) — Common subjects face arbitrary taxation without consent and forced billeting of soldiers. They cannot petition Parliament (no representation); cannot refuse without imprisonment; cannot exit the kingdom. Maximum extraction with no alternatives. The forced loan is pure mechanism of prerogative extraction, with suppression enforced through detention without trial.
constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMPRISONED GENTLEMAN (TANGLED ROPE) — A property-holder imprisoned for refusing a forced loan faces genuine constraint: the constraint coordinates the Crown's revenue need with parliamentary consent, but executes that coordination through detention without trial. Suppression is high (no bail, no trial date specified) but agent has some structural position (can petition, has property as leverage). Experiences both coordination (the regime does ultimately negotiate releases) and extraction (imprisonment used as coercive tool).
constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENTARY COALITION (ROPE) — Organized parliamentarians (Commons and Lords united) see the constraint as a coordination mechanism: establishing consent-based taxation solves the underlying collective action problem (Crown needs funds; Parliament wants fiscal control). The Petition restates medieval liberties precisely to show this is not novel — it is re-assertion of standing coordination terms. Exit option: Parliament can withhold supply entirely (mobile). Experience: genuine coordination function, minimal extraction.
constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: CROWN AUTHORITY (TANGLED ROPE) — The Crown (Charles I via his ministers) sees the constraint as necessary coordination: Parliament IS the revenue mechanism, not a competitor to it. But the constraint also extracts institutional cost — prerogative detention powers are surrendered; forced loans (major revenue stream in 1620s) are abolished. The Crown experiences suppression of its revenue options (constrained exit: must now negotiate rather than impose). Coordination function exists (supply depends on consent) but is asymmetrically forced.
constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEUDAL OBLIGATION NARRATIVE (PITON) — The medieval liberties framing (Magna Carta echo) presents the constraint as restoration of natural feudal order rather than novel limitation. The theater here is the appeal to precedent: the Petition performs medievalism to naturalize what is actually a modern parliamentary assertion. The Crown can arbitrage this narrative (argue it is defending ancient liberties against innovations) even while losing power. Theater ratio reflects the gap between claimed restoration and actual structural change.
constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational view, the constraint appears to rest on a natural law: a sovereign authority cannot fund itself indefinitely through pure extraction; at some threshold suppression becomes unsustainable and renegotiation becomes inevitable. The Petition rests on this premise: forced loans cannot be the sole revenue source indefinitely. However, this risks naturalizing what is a contingent institutional structure — different fiscal regimes (tax farming, customs monopoly, church seizure, colonial extraction) have sustained extraction at different scales. The mountain classification is perspectival and strategically valuable to the parliamentarians (who want to present their demands as natural law rather than power assertion), but the structural data contradicts it.
constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INTERIM SETTLEMENTS (SCAFFOLD) — Moderate parliamentarians see the Petition as a temporary stabilization mechanism (1628–1642): it re-establishes parliamentary supply consent without overturning the monarchy itself. This reading expects a sunset: either the Crown accepts the constraint and fiscal governance stabilizes, or conflict resumes. The constraint has explicit sunset logic: it is presented as a restatement of existing law (Magna Carta), not new limitation — which means it succeeds if the Crown 'merely' respects ancient precedent. If the Crown refuses, the constraint becomes unenforceable and the relationship dissolves (Civil War). Theater is moderate: the Petition performs as legal document, not innovation.
constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(foundational_charters__petition_of_right_1628_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(foundational_charters__petition_of_right_1628, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(foundational_charters__petition_of_right_1628, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(foundational_charters__petition_of_right_1628, TR),
    TR >= 0.70.

:- end_tests(foundational_charters__petition_of_right_1628_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The Crown's prerogative extraction mechanism (forced loans, arbitrary detention) is curtailed by the Petition, but the constraint achieves this through an indirect mechanism — not abolishing the Crown's revenue need, but re-chaining it to parliamentary consent. The extraction persists in reduced form: suppression remains high for subjects unable to invoke parliamentary protections, and the coordination still favors the Crown (Parliament typically grants supply, even at cost). The value reflects that extraction is partially suppressed (from 0.72 at forced-loan peak) but not eliminated. Suppression (0.68): High. Enforcement of detention limits is weak without habeas corpus procedure; subjects remain imprisoned despite Petition; forced billeting persists in practice. The constraint suppresses the Crown's extraction options (cannot use forced loans as sole revenue) but cannot fully suppress the underlying coercive mechanisms. Theater ratio (0.55): Moderate-high, rising. The Petition performs as legal document restating precedent, but the restatement itself constitutes a novel power assertion disguised as restoration. The medieval liberties framing is performative — the actual constraint is the modern parliamentary assertion of supply control. Theater rises because the gap between claimed restoration and actual structural change widens as the Crown resists (non-compliance with Petition escalates the performative conflict).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival divergence across power positions. The forced loan subject experiences snare (pure extraction, no exit). The imprisoned gentleman experiences tangled rope (mixed coordination and extraction). The parliamentary coalition experiences rope (genuine coordination, beneficiary position). The Crown experiences tangled rope (coordination function but constrained exit). The feudal narrative advocates experience piton (performative medievalism that arbitrages power dynamics). The civilizational observer risks mountain classification (imagining immutable fiscal limits on extraction), but the structural data contradicts this — different regimes sustain different levels of extraction. The gap reveals that 'liberty' itself is constraint-relative: what appears as natural law to the philosophical observer is coordination to the beneficiary and snare to the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps each agent's structural position in the extraction flow. Forced loan subjects (victims, trapped, powerless) face maximum d ≈ 0.95 (nearly pure targets). Imprisoned gentlemen (mixed victim/beneficiary, constrained, moderate) face d ≈ 0.65 (high cost but some agency). Parliamentarians (beneficiaries, mobile, organized) face d ≈ 0.40 (benefit from coordination, can exit via supply withholding). Crown (beneficiary of coordination, constrained, institutional) faces d ≈ 0.55 (revenue dependent on parliamentary consent; exit is military force, which invokes civilization-scale costs). The dispersion of d values across perspectives drives the classification diversity: those with high d see snare; those with moderate d see tangled rope; those with low d see rope. The false summit (mountain from analytical perspective) emerges when the observer naturalizes the constraint as immutable fiscal law rather than contingent institutional structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_versus_coercion_threshold,
    'At what fiscal pressure does ''consent-based'' taxation become indistinguishable from coerced supply? When does parliamentary negotiation collapse into extraction theater?',
    'Longitudinal analysis of parliamentary supply votes 1628–1642: correlation between economic crisis severity and parliament''s ability to withhold supply without regime consequence. Track moments where parliamentarians were forced to grant supply under duress.',
    'If threshold is low (parliament retains real veto power even under pressure): constraint is genuine Rope from parliamentary perspective. If threshold is high (parliament capitulates under economic or military crisis): constraint is theater and reverts to Crown prerogative during emergencies, making it Piton rather than Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_versus_coercion_threshold, empirical, 'Threshold at which consent-based taxation becomes coercive').

omega_variable(
    prerogative_detention_enforcement_mechanism,
    'What enforcement mechanism makes the ban on arbitrary detention credible? Without habeas corpus procedure, how do imprisoned subjects challenge ''without cause'' determinations?',
    'Historical case analysis: petitions for release 1628–1640; crown responses; rate of release vs. sustained detention. Comparison to post-Habeas Corpus Act cases (1679+) showing difference in enforcement credibility.',
    'If enforcement is weak (many subjects remain imprisoned despite Petition; crown ignores petitions): suppression remains high and the constraint is more snare-like. If enforcement is strong (crown releases detained subjects who invoke Petition protections): suppression drops and constraint becomes more rope-like.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prerogative_detention_enforcement_mechanism, empirical, 'Credibility of detention ban enforcement without habeas corpus procedure').

omega_variable(
    medieval_liberties_versus_modern_assertion,
    'Is the Petition genuinely restating unchanged medieval liberties, or does the restatement itself constitute a novel assertion that redefines those liberties for a modern fiscal/military state?',
    'Comparative textual analysis: Magna Carta (1215) vs. Petition of Right (1628) on taxation, detention, billeting clauses. Semantic drift analysis: do identical words mean identical constraints in different contexts (medieval feudal levy vs. 1620s parliamentary supply)?',
    'If genuinely medieval restatement: constraint is restoration (piton narrative supported). If novel assertion: constraint is modern parliamentary innovation disguised as precedent (false medieval framing). This determines whether the constraint''s legitimacy rests on lineage authority or on contemporary parliamentary power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_liberties_versus_modern_assertion, conceptual, 'Whether Petition restates medieval liberties or constitutes novel assertion disguised as precedent').

omega_variable(
    kingdom_scope_versus_parliament_scope,
    'Does the Petition protect ''the liberties of the subject'' uniformly across the kingdom, or does it effectively protect only those propertied enough to petition Parliament and defend themselves?',
    'Social history analysis: who invokes the Petition protections in practice? Are protection rates proportional to wealth/status? Do common subjects (laborers, peasants) gain protection, or only gentry and merchants?',
    'If uniform protection: the snare perspective is overstated; many subjects do gain material protection. If status-stratified (real protection only for propertied): the Petition is a rope for propertied agents and a snare for the masses. This determines whether the beneficiary set should expand to ''all subjects'' or remain ''propertied parliamentarians and imprisoned gentlemen.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kingdom_scope_versus_parliament_scope, empirical, 'Whether Petition protections apply uniformly across kingdom or are status-stratified').

omega_variable(
    reading_kernel_ambiguity_natural_law_versus_power,
    'Is the constraint a reading of a kernel (an ambiguously specified, contested commitment) or a reading of a settled natural law that the Petition merely re-articulates?',
    'Historical analysis of how later actors (Commonwealth, Restoration, 1689) reinterpret the Petition. If reinterpretation is extensive and contested, the constraint is a reading of an ambiguous kernel (foundational_charters kernel with sibling readings Magna Carta, Habeas Corpus). If later actors treat it as settled law requiring no re-interpretation, it is a stable constraint, not a kernel reading.',
    'If kernel reading: the constraint instantiates ONE interpretation of ''liberties'' among others (coexists_with Magna Carta and Habeas Corpus readings). If settled: the constraint is a point solution, not part of a lineage of contested readings. This determines whether committer structure (cs_structure.reading_relations, axioms) is relevant or whether the constraint should be authored as a standalone statement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity_natural_law_versus_power, conceptual, 'Whether constraint is reading of kernel or settled natural law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(foundational_charters__petition_of_right_1628, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(petright_tr_t0, foundational_charters__petition_of_right_1628, theater_ratio, 0, 0.48).
narrative_ontology:measurement(petright_tr_t7, foundational_charters__petition_of_right_1628, theater_ratio, 7, 0.55).
narrative_ontology:measurement(petright_tr_t14, foundational_charters__petition_of_right_1628, theater_ratio, 14, 0.62).

% Extraction over time
narrative_ontology:measurement(petright_be_t0, foundational_charters__petition_of_right_1628, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(petright_be_t7, foundational_charters__petition_of_right_1628, base_extractiveness, 7, 0.6).
narrative_ontology:measurement(petright_be_t14, foundational_charters__petition_of_right_1628, base_extractiveness, 14, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(petright_su_t0, foundational_charters__petition_of_right_1628, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(petright_su_t7, foundational_charters__petition_of_right_1628, suppression_requirement, 7, 0.68).
narrative_ontology:measurement(petright_su_t14, foundational_charters__petition_of_right_1628, suppression_requirement, 14, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(foundational_charters__petition_of_right_1628, resource_allocation).
narrative_ontology:affects_constraint(foundational_charters__petition_of_right_1628, magna_carta_1215).
narrative_ontology:affects_constraint(foundational_charters__petition_of_right_1628, habeas_corpus_act_1679).
narrative_ontology:affects_constraint(foundational_charters__petition_of_right_1628, parliamentary_supply_consent_mechanism).
narrative_ontology:affects_constraint(foundational_charters__petition_of_right_1628, prerogative_detention_authority).

% DUAL FORMULATION NOTE:
% The Petition of Right is one reading of the foundational_charters kernel alongside Magna Carta (1215) and Habeas Corpus (1679). Each reading has distinct ε (empirical status of the liberty claim), distinct beneficiary/victim structure, and distinct enforcement mechanism. They form a lineage rather than a competition: each adds specificity and procedure to the prior. The Petition reading emphasizes parliamentary supply consent; Habeas Corpus reading emphasizes detention procedure. Both coexist after 1679.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
