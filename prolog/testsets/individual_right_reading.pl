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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Heller Framework)
 *   domain: constitutional_law/political_theory/federalism
 *
 * SUMMARY:
 *   The individual-right reading of the Second Amendment instantiates a
 *   specific constitutional interpretation: that the prefatory militia clause
 *   announces purpose but does not limit the operative clause granting the
 *   right to bear arms; that the right inheres in individual citizens
 *   independent of militia service; and that this right is grounded in
 *   self-defense and personal autonomy rather than collective militia
 *   readiness. This reading became ascendant in Heller v. District of
 *   Columbia (2008), which held that the Second Amendment 'protects an
 *   individual right to possess a firearm independent of service in a well
 *   regulated Militia, and to use that arm for lawfully protected purposes,
 *   such as self-defense.' The constraint exhibits Tangled Rope structure: it
 *   coordinates with individual liberty and self-defense values while
 *   extracting from state regulatory capacity to manage public health and
 *   urban safety. The reading coexists with competing interpretations (the
 *   collective-militia reading holds that the right applies only to militia
 *   members; the sophisticated-collective reading holds that the right is
 *   individual but serves militia-enabling purposes). These readings share a
 *   kernel—the text of the Second Amendment itself—but diverge radically on
 *   what that text requires.
 *
 * KEY AGENTS:
 *   - Individual Citizens: Primary beneficiary (powerful/mobile) — gain constitutional recognition of right to bear arms; self-defense becomes constitutionally protected purpose; can exercise right across jurisdictions where legal (mobile exit options)
 *   - Gun Manufacturing Industry: Secondary beneficiary (institutional/arbitrage) — protected market for firearms; immunity from certain legal theories; arbitrage across state regulatory regimes; property rights reinforced
 *   - Originalist Judiciary: Secondary beneficiary (institutional/arbitrage) — anchors constitutional authority in historical interpretation method; maintains institutional role as guardian of founding principles; theaters historical fact-finding to support predetermined reading
 *   - Regulatory State (Federal/State Governments): Primary victim (organized/constrained) — constrained in firearm regulation authority; must work within narrow exceptions (felon prohibitions, sensitive places); faces pressure from litigation challenging regulations; retains some agency through careful regulation drafting
 *   - Urban Safety Governance Systems: Secondary victim (powerless/trapped) — bear public health costs of widespread individual firearm access without regulatory tools to manage outcomes; face irreconcilable pressure between constitutional right and safety governance; no exit option from constitutional constraint
 *   - Public Health Regulatory Capacity: Tertiary victim (powerless/trapped) — access to firearms as tool for suicide, interpersonal violence, accidental discharge; cannot restrict weapons by public health criteria; regulatory science constrained by constitutional bounds
 *   - Collective-Militia Reading Constituency: Interpretive loser (organized/constrained) — hold competing reading that now has lower judicial authority; constrained to argue within individual-right framework even when advocating militia-centric interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_right_reading, 0.58).
domain_priors:suppression_score(individual_right_reading, 0.62).
domain_priors:theater_ratio(individual_right_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(individual_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(individual_right_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_right_reading, tangled_rope).
narrative_ontology:human_readable(individual_right_reading, "Second Amendment Individual Right Reading (Heller Framework)").
narrative_ontology:topic_domain(individual_right_reading, "constitutional_law/political_theory/federalism").

domain_priors:requires_active_enforcement(individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(individual_right_reading, '099e99f9-9c9e-4fa1-865a-4f0acba9b34e').
narrative_ontology:cs_created_at('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', '').
narrative_ontology:cs_kernel_codification('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', fixed_text).
narrative_ontology:cs_authority_grounding('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', lineage).
narrative_ontology:cs_interpretation_layer_present('099e99f9-9c9e-4fa1-865a-4f0acba9b34e').
narrative_ontology:cs_kernel_id(individual_right_reading, second_amendment_text).
narrative_ontology:cs_reading_relation('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', collective_militia_reading, forecloses).
narrative_ontology:cs_reading_relation('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', sophisticated_collective_reading, coexists_with).
narrative_ontology:cs_axiom('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', foundational, operative_clause_independent_scope).
narrative_ontology:cs_axiom_status(operative_clause_independent_scope, holdable).
narrative_ontology:cs_axiom_grounding('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', operative_clause_independent_scope, empirically_contingent).
narrative_ontology:cs_axiom('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', foundational, self_defense_independent_purpose).
narrative_ontology:cs_axiom_status(self_defense_independent_purpose, holdable).
narrative_ontology:cs_axiom_grounding('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', self_defense_independent_purpose, deontological).
narrative_ontology:cs_reference_frame('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', individual_natural_rights_framework).
narrative_ontology:cs_drift_state('099e99f9-9c9e-4fa1-865a-4f0acba9b34e', contemporary_post_heller, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_right_reading, individual_citizens).
narrative_ontology:constraint_beneficiary(individual_right_reading, self_defense_practitioners).
narrative_ontology:constraint_beneficiary(individual_right_reading, gun_manufacturers).
narrative_ontology:constraint_beneficiary(individual_right_reading, originalist_judiciary).
narrative_ontology:constraint_victim(individual_right_reading, regulatory_state_authority).
narrative_ontology:constraint_victim(individual_right_reading, public_health_regulatory_capacity).
narrative_ontology:constraint_victim(individual_right_reading, urban_safety_governance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ARMED CITIZEN (ROPE) — Experiences the constraint as coordinate protection of a fundamental right. Sees the Second Amendment as recognizing pre-existing individual capacity for self-defense. Benefits from clear constitutional recognition without significant suppression (mobile exit; can exercise right across jurisdictions where legal). Coordination benefit is genuine: the right enables self-defense without requiring state apparatus. No systemic extraction from this perspective.
constraint_indexing:constraint_classification(individual_right_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN SAFETY GOVERNANCE (SNARE) — Cannot exit the constitutional constraint; bears asymmetric extraction. Cities face irreconcilable pressure: constitutional right to bear arms in urban density creates public health costs (accidental discharge, suicide access, interpersonal violence) that governance systems must absorb without regulatory tools. The constraint removes policy options for concentrated population centers while mandating absorption of public health consequences. Maximum extraction, maximum suppression — no escape path.
constraint_indexing:constraint_classification(individual_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE REGULATORY STATE (TANGLED ROPE) — Constrained by the constitutional reading but retains some regulatory space (Heller preserves 'sensitive places' exception; affirms long-standing restrictions remain valid). The regulatory state both coordinates with the individual-right framework and experiences extraction: must recognize the right while managing public health. Has agency through careful distinction between protected individual use and permissible regulation (background checks, felon restrictions, sensitive places). Mixed coordination-extraction; agency available through interpretation boundaries.
constraint_indexing:constraint_classification(individual_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GUN MANUFACTURING INDUSTRY (TANGLED ROPE) — Experiences the constraint as coordinating with property rights and legitimate commerce. Benefits substantially from constitutional protection (market size, immunity from certain litigation theories, arbitrage across state regulatory regimes). Also experiences suppression through varying state regulations, liability concerns, and reputational cost. Net beneficiary with constrained arbitrage options — can arbitrage across state lines but faces federal baseline uncertainty (Congress retains power to regulate interstate commerce in firearms). Mixed benefit and constraint; high agency through market adaptation.
constraint_indexing:constraint_classification(individual_right_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALIST JUDICIARY (PITON) — Maintains the individual-right reading through a particular interpretive method (original public meaning at ratification), but the performance of that method has become increasingly theatrical. The judiciary invokes originalism to anchor the reading in historical intent, but faces two performative pressures: (a) 18th-century evidence of individual vs militia understanding is genuinely contested by historians, creating theater around 'historical fact-finding'; (b) modern application doctrine (what counts as 'common use'? what counts as 'sensitive place'?) requires policy judgment dressed as historical interpretation. Theater ratio elevated by the gap between historical methodology aspired to and actual decision-making structure. The originalist commitment persists through inertia and institutional identity rather than methodological success.
constraint_indexing:constraint_classification(individual_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the right to self-defense is presented as an unchangeable feature of human nature or natural law: all individuals have an inherent right to defend themselves against force. The Second Amendment is framed as recognizing rather than creating this right. This perspective claims the individual-right reading emerges naturally from immutable principles. However, the structural data contradicts the mountain classification — identifiable beneficiaries exist, and the constraint requires active enforcement through judicial interpretation. The engine will identify this as a false summit: the 'natural law' framing masks a contingent institutional choice.
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
    constraint_indexing:constraint_classification(individual_right_reading, TypeOther, context(agent_power(powerful), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The individual-right reading provides genuine coordination benefit (protects self-defense capacity without state intermediation) but asymmetrically extracts from state regulatory authority. The beneficiaries (individual citizens, gun manufacturers) capture clear value; the victims (public health systems, urban safety governance) bear suppressed regulatory capacity without equivalent benefit. The extraction is not maximal (0.66+) because the regulatory state retains some authority through exceptions and through rational-basis regulation unrelated to firearm restriction (background checks, felon prohibitions remain valid). The growth from 0.42 to 0.58 over the interval reflects Heller's expansion of the individual-right framework: early in the interval (pre-2008), the reading competed with collective interpretations; post-Heller, it has institutional authority and can extract through litigation that constrains regulatory innovation. Suppression (0.62): Moderate-high. Urban jurisdictions face real barriers to public health regulation: they cannot restrict common-use weapons, cannot easily regulate accumulation, face constant litigation challenging regulations. State governments face political barriers (gun-rights constituency mobilization) and constitutional barriers (Heller precedent). These are material constraints, not merely ideological. However, suppression is not maximal (0.70+) because federal authority over interstate commerce could theoretically override state constraints, and the sensitive-places exception provides some regulatory flexibility. Theater ratio (0.45, rising to 0.50 then 0.45 by time point 30): Moderate. The originalist interpretive method (historical fact-finding about original public meaning) carries performative elements—historians legitimately contest the militia-clause question, yet courts present the answer as historical truth rather than institutional choice. However, the theater is not extreme (0.70+) because the individual-right reading does genuinely track founding-era language and some plausible historical evidence; it is not pure performance like a degraded ritual. The variation over time reflects rising pressure from academic historiography questioning the militia-clause reading (raising theater in the middle interval) followed by consolidation of the reading through case law (theater declining as institutional commitment hardens).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival diversity across all six classification types, revealing the reading-choice embedded in institutional position. The armed citizen sees Rope (pure coordination of a natural right). Urban safety governance sees Snare (trapped extraction with no exit). The regulatory state sees Tangled Rope (mixed coordination and constraint with limited agency). The gun industry sees Tangled Rope (genuine commerce coordination with some regulatory headwinds). The originalist judiciary sees Piton (maintains a degraded historical method through institutional inertia and theater). The civilizational analytical observer risks seeing Mountain (natural law of self-defense immutable and pre-political) but structural analysis reveals this as a false summit—the reading requires active institutional enforcement through litigation, beneficiaries are identifiable, and the classification depends on accepting a particular originalist methodology. The perspectival gap reveals that the reading is not discovered but chosen, and the choice distributes benefits and costs asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's power level, exit options, and structural relationship to the constraint. Individual citizens with arbitrage exit options (mobile, can cross state lines) experience low or negative effective extraction—they benefit from the right and can exit constraints by moving. The originalist judiciary with institutional power and arbitrage options (can choose interpretive methods, can arbitrage across doctrinal boundaries) experience negative extraction—they benefit from the authority the reading grants. The regulatory state with organized power but constrained exit options (cannot simply opt out of constitutional constraints; constrained to work within the individual-right framework) experiences moderate positive extraction. Urban safety governance and public health systems with no exit options (trapped by constitutional bounds, cannot regulate away the right) experience maximum extraction. The core directionality distinction: beneficiaries have exit or institutional authority; victims have neither. The engine's computation of d from beneficiary/victim declarations and power level will show the asymmetry clearly.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER-FRAME RESOLUTION: This constraint is one reading of the second_amendment_text kernel, and mandatrophy is resolved through the reading_relations structure. The individual_right_reading forecloses the collective_militia_reading at the level of constitutional meaning: if the operative clause grants a right to individuals independent of militia service, then the reading that the right applies only to militia members is logically impossible within the same constitutional framework. However, the individual_right_reading COEXISTS_WITH the sophisticated_collective_reading: both can hold that individuals have the right, but disagree about whether the right serves primarily militia-enabling purposes or primarily self-defense purposes. The mandatrophy dissolves because the readings are not competing claims about the same constraint—they are competing readings of a kernel, and this reading (individual_right_reading) is one stable instantiation. The constraint classification (Tangled Rope) is correct for THIS reading; the other readings would produce different classifications from their own bases (collective_militia_reading would classify the constraint as inverted, harming the state's militia interests). No single type is 'correct'—each reading instantiates a distinct constraint with its own extractiveness structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_interpretive_weight,
    'Does the prefatory militia clause function as a constraint on the operative right, or is it purely announced purpose that does not limit application?',
    'Historical linguistic analysis of 18th-century legal documents; comparative study of how prefatory and operative clauses functioned in founding-era legal texts; examination of whether the operative right tracked strictly to militia-supporting individuals or extended to all individuals',
    'If prefatory clause IS limiting: reclassifies to collective_militia_reading (constraint applies only to militia members; victims include individual non-militia citizens; beneficiaries are state militia systems). If prefatory clause is NOT limiting: confirms individual_right_reading and its beneficiary/victim structure. This is the core reading-sibling distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_interpretive_weight, empirical, 'Whether the militia clause limits the operative right or is merely stated purpose').

omega_variable(
    framers_intent_on_individual_vs_collective,
    'What was the original public meaning at ratification: was the right understood as individual (each person bears arms independently) or collective (right exists only through militia membership)?',
    'Corpus linguistic analysis of founding-era texts (Federalist Papers, state ratification debates, contemporaneous state constitutions with ''right to bear arms'' language); comparison with how ''bear arms'' appears in other legal/military contexts; examination of state constitutions and whether states with similar language permitted universal individual arms ownership or militia-only',
    'If individual understanding was dominant at ratification: supports individual_right_reading as faithful to original meaning. If collective understanding was dominant or ambiguous: sophisticated_collective_reading becomes stronger, and individual_right_reading appears as post-hoc expansion beyond original intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framers_intent_on_individual_vs_collective, empirical, 'Original public meaning at ratification: individual vs collective right').

omega_variable(
    common_use_boundary_determination,
    'What counts as ''common use'' for purposes of constitutional protection, and who determines it: courts, legislatures, market data, or historical practice?',
    'Empirical tracking of which weapons courts have protected/restricted; analysis of whether ''common use'' standard tracks consumer preference data or is judicial policy judgment; examination of how different courts measure commonness (sales figures, militia surveys, historical precedent)',
    'If common use is determined by market/practice: highly dynamic boundary; regulatory constraint is strong (new weapons readily protected). If determined by courts with narrow historical definition: regulatory capacity is greater (courts can restrict innovations). This determines how tightly the urban_safety_governance victim is bound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_use_boundary_determination, empirical, 'Determinants of ''common use'' boundary for constitutional protection').

omega_variable(
    self_defense_vs_militia_service_distinction,
    'Is self-defense a constitutionally protected purpose independent of militia service, or does constitutional protection depend on demonstrating connection to militia readiness?',
    'Textual analysis of Heller decision; subsequent case law on whether pure self-defense (home protection) receives same constitutional tier as militia-related use; examination of whether courts recognize hierarchy of purposes or equality of all individual uses',
    'If self-defense is independent purpose: individual_right_reading stands; all individual citizens are beneficiaries. If self-defense must be tied to militia readiness: interpretive collapse toward collective_militia_reading; beneficiary set narrows to militia-relevant individuals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_defense_vs_militia_service_distinction, empirical, 'Independence of self-defense as constitutional purpose from militia service').

omega_variable(
    sensitive_places_exception_scope,
    'How broadly can courts interpret ''sensitive places'' exception, and does the exception privilege remain stable or expand over time as new regulatory challenges emerge?',
    'Tracking of sensitive places doctrine development across jurisdictions; examination of whether courts define sensitive places narrowly (government buildings, courts, military bases) or expansively (schools, hospitals, airports, public transit); analysis of whether exception scope tracks regulatory necessity or remains historically anchored',
    'If narrowly defined: regulatory state faces continued pressure (many venues remain unprotected); victim status of public_health_regulatory_capacity persists. If broadly interpreted: regulatory state gains flexibility; tangled_rope classification for regulatory_state remains but extraction asymmetry decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sensitive_places_exception_scope, empirical, 'Scope stability of the sensitive places exception to individual-right protection').

omega_variable(
    false_summit_natural_law_claim,
    'Is the individual right truly grounded in natural law / pre-political human rights, or is the ''natural right'' framing a constructed legitimacy narrative that conceals contingent institutional choices?',
    'Historical analysis of pre-18th-century individual arms ownership by civilians; comparison with other natural-law claims that have been empirically contested; examination of societies with stable self-defense mechanisms without individual gun ownership rights; analysis of whether ''natural right'' claim predicts outcomes better than institutional-choice model',
    'If grounded in natural law: mountain classification holds; no false summit signature. If constructed: engine''s false summit detector fires; reveals that natural-law framing naturalizes a particular institutional choice (American federalism + common law tradition + manufacturing industry interests + revolutionary-era political theory). Omega resolution would require either surrendering the natural-law claim or providing empirical grounding that survives cross-cultural comparison.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether natural law grounding is genuine or constructed legitimacy narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, individual_right_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(indi_tr_t15, individual_right_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(indi_tr_t30, individual_right_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, individual_right_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(indi_be_t15, individual_right_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(indi_be_t30, individual_right_reading, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(individual_right_reading, collective_militia_reading).
narrative_ontology:affects_constraint(individual_right_reading, sophisticated_collective_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_text kernel instantiates three structurally distinct constraints corresponding to three reading interpretations. Each reading has its own ε value reflecting the empirical authority of the reading's textual and historical claims, its own beneficiary/victim structure reflecting which agents benefit from each interpretation, and its own type classification reflecting the extractiveness asymmetry. All three constraints share the same textual kernel but diverge in how they resolve the prefatory/operative clause relationship and how they understand militia-relative purpose. They are linked as a constraint family via network.affects_constraints because each reading's institutional authority directly constrains the others: as individual_right_reading gains precedential weight in federal courts, the collective_militia_reading loses institutional grounding, which changes the structural environment for sophisticated_collective_reading (forced to argue within individual-right doctrinal framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
