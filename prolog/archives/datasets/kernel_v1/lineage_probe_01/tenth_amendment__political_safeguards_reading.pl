% ============================================================================
% CONSTRAINT STORY: tenth_amendment__political_safeguards_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenth_amendment__political_safeguards_reading, []).

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
 *   constraint_id: tenth_amendment__political_safeguards_reading
 *   human_readable: Tenth Amendment Political Safeguards Reading: Federalism via Senate and Party Politics
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The political safeguards reading of the Tenth Amendment claims that
 *   federalism's real protection is not judicial enforcement of state/federal
 *   boundaries but political protection through the Senate and party
 *   coalitions. States defend themselves through Senate representation and
 *   electoral politics, not through courts. Under this reading, the judiciary
 *   should not police federalism boundaries—that work belongs to the
 *   political process. This constraint exhibits extraction dynamics: the
 *   reading benefits federal political majorities (who can legislate without
 *   judicial gatekeeping), extracts from states lacking Senate leverage, and
 *   suppresses judicially-enforced federalism policing as an alternative
 *   mechanism. The constraint is a Tangled Rope: it has genuine coordination
 *   function (states do negotiate federalism terms through legislative
 *   dealmaking) alongside asymmetric extraction (federal majorities can
 *   override state preferences, and powerless states have no remedy). The
 *   theater ratio remains moderate (0.48) because the political process is
 *   functionally real—legislative negotiation actually happens—but
 *   performative elements exist (the myth that political process equally
 *   protects all states, including those without coalition leverage).
 *   Extractiveness has risen over the 50-year interval from 0.25 to 0.58,
 *   reflecting steady federal scope expansion at the expense of state
 *   autonomy despite the reading's assurance that political safeguards would
 *   suffice.
 *
 * KEY AGENTS:
 *   - Federal Political Majorities: Primary beneficiary (institutional/arbitrage) — can legislate federalism scope without judicial limits; Senate coordination is pure coalition advantage
 *   - States with Senate Leverage: Secondary beneficiary (powerful/mobile) — can negotiate federalism terms through legislative coalition-building; extract from weaker states via coalition power
 *   - States Without Senate Leverage: Primary victim (powerless/trapped) — have no court recourse and no political exit; face federal mandates with minimal bargaining power
 *   - State Legislatures: Secondary victim (moderate/constrained) — absorb federal regulatory burden and compliance costs; constrained lobbying capacity
 *   - Judicially-Protected Federalism Boundaries: Victim (abstract/trapped) — suppressed by the reading's core claim that courts should defer; no institutionalized protector
 *   - Interstate Political Coalitions: Organized agent (organized/constrained) — can mobilize collective action through party organizations and gubernatorial associations, but success depends on credible political threat
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks treating the reading's normative choice (defer to politics) as a natural structural inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenth_amendment__political_safeguards_reading, 0.58).
domain_priors:suppression_score(tenth_amendment__political_safeguards_reading, 0.65).
domain_priors:theater_ratio(tenth_amendment__political_safeguards_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenth_amendment__political_safeguards_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tenth_amendment__political_safeguards_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tenth_amendment__political_safeguards_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenth_amendment__political_safeguards_reading, tangled_rope).
narrative_ontology:human_readable(tenth_amendment__political_safeguards_reading, "Tenth Amendment Political Safeguards Reading: Federalism via Senate and Party Politics").
narrative_ontology:topic_domain(tenth_amendment__political_safeguards_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(tenth_amendment__political_safeguards_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenth_amendment__political_safeguards_reading, 'd6278987-1d10-4166-b68f-a8a2bdb7e22e').
narrative_ontology:cs_kernel_codification('d6278987-1d10-4166-b68f-a8a2bdb7e22e', fixed_text).
narrative_ontology:cs_authority_grounding('d6278987-1d10-4166-b68f-a8a2bdb7e22e', lineage).
narrative_ontology:cs_interpretation_layer_present('d6278987-1d10-4166-b68f-a8a2bdb7e22e').
narrative_ontology:cs_reading_relation('d6278987-1d10-4166-b68f-a8a2bdb7e22e', tenth_amendment__anticommandeering_doctrine, influences).
narrative_ontology:cs_reading_relation('d6278987-1d10-4166-b68f-a8a2bdb7e22e', tenth_amendment__truism_reading, coexists_with).
narrative_ontology:cs_axiom('d6278987-1d10-4166-b68f-a8a2bdb7e22e', foundational, political_process_federalism_sufficient).
narrative_ontology:cs_axiom_status(political_process_federalism_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('d6278987-1d10-4166-b68f-a8a2bdb7e22e', political_process_federalism_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('d6278987-1d10-4166-b68f-a8a2bdb7e22e', foundational, judicial_federalism_review_illegitimate).
narrative_ontology:cs_axiom_status(judicial_federalism_review_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('d6278987-1d10-4166-b68f-a8a2bdb7e22e', judicial_federalism_review_illegitimate, deontological).
narrative_ontology:cs_reference_frame('d6278987-1d10-4166-b68f-a8a2bdb7e22e', tenth_amendment_structural_reservation).
narrative_ontology:cs_drift_state('d6278987-1d10-4166-b68f-a8a2bdb7e22e', post_garcia_political_safeguards_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d6278987-1d10-4166-b68f-a8a2bdb7e22e', '').
narrative_ontology:cs_kernel_id(tenth_amendment__political_safeguards_reading, tenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenth_amendment__political_safeguards_reading, federal_political_majorities).
narrative_ontology:constraint_beneficiary(tenth_amendment__political_safeguards_reading, senate_majority).
narrative_ontology:constraint_beneficiary(tenth_amendment__political_safeguards_reading, electoral_coalitions).
narrative_ontology:constraint_victim(tenth_amendment__political_safeguards_reading, judicially_protected_state_spheres).
narrative_ontology:constraint_victim(tenth_amendment__political_safeguards_reading, minority_state_interests).
narrative_ontology:constraint_victim(tenth_amendment__political_safeguards_reading, state_autonomy_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE WITHOUT SENATE LEVERAGE (SNARE) — A state minority in the Senate has no court recourse and no political exit. The reading's core claim — rely on political process — offers no protection to states that lack coalition power. These states experience maximal extraction through federal mandates with no remedy. The trap is structural: the remedy prescribed (political organizing) is unavailable to those without partisan leverage.
constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SWING-STATE COALITION (TANGLED ROPE) — A coalition of states with Senate influence and electoral importance experiences the reading as genuine coordination: they can negotiate federalism terms with the federal majority through legislative dealmaking and party coalition-building. They benefit from federal programs while defending state prerogatives via political bargaining. But the mechanism is also extractive: the federal majority can credibly threaten to override state preferences if coalition leverage fails. Real agency, but under conditional threat.
constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL POLITICAL MAJORITY (ROPE) — The reading's intended beneficiary. A federal majority (House + Senate + President) experiences the constraint as pure coordination: federalism is solved through legislating what the majority wants, knowing that states with Senate seats can negotiate amendments. No judicial gatekeeping needed. The majority has agency and arbitrage options (override states via statute, or accommodate them in exchange for coalition support). Net beneficiary—this perspective aligns perfectly with the reading's prescriptive claim.
constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE LEGISLATURES (CONSTRAINED/MODERATE) — State legislatures face federal mandates (Medicaid conditions, Clean Air Act, ADA implementation) with limited political recourse. They can lobby Congress, but lobbying is expensive and success depends on finding 50+ senators willing to listen. The reading offers no exit: courts won't help, and the political process is structurally tilted toward federal scope expansion once majorities form. Over time, this becomes a snare—state legislatures absorb federal regulatory burden with minimal bargaining power.
constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERSTATE COALITIONS AND PARTIES (ORGANIZED/CONSTRAINED) — Organized state actors (party machines, gubernatorial associations, state attorney general coalitions) can mobilize collective action to defend federalism boundaries through the political process. These actors see the reading as enabling genuine coordination: collective state organizing can shape federal legislation and executive policy. But they are also constrained by the reading's core rule: no judicial fallback if political organizing fails. The extractive element is the conditionality: the federal majority will accommodate state concerns only if state organizing poses a credible political threat.
constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a civilizational/universal perspective, the reading naturalizes what is actually a contested institutional choice. The claim 'courts should leave federalism to politics' presents political process safeguards as structurally inevitable—as if the Constitution's design inherently relied on party coalitions and Senate leverage, rather than on textual boundaries and judicial review. This perspective risks treating a reading (political safeguards are sufficient) as a natural law (federalism is inherently protected by politics). The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenth_amendment__political_safeguards_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenth_amendment__political_safeguards_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenth_amendment__political_safeguards_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenth_amendment__political_safeguards_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The reading enables federal scope expansion. States retain some negotiating power through Senate representation, but this power is distributed unequally—swing states and coalition members have leverage; others do not. The extractiveness reflects the asymmetric flow: federal majorities extract federalism concessions from the political process, and within the political process, coalition states extract from non-coalition states. Suppression (0.65): High. The reading suppresses the alternative mechanism (judicial federalism review), and it suppresses remedies for states lacking political leverage. A state that loses a federalism battle in the Senate has no court review—the reading's core claim is 'courts should stay out.' This creates structural suppression of exit options. Theater ratio (0.48): Moderate. The political process is functionally real—legislative dealmaking genuinely occurs—but the reading contains performative elements. The myth that political process 'protects' all states equally is theater: states without coalition leverage get no protection at all. The narrative that the Constitution 'relies on' political safeguards is also theater—it presents as discovery what is actually a normative choice. The interval shows rising extractiveness (0.25→0.58) and rising suppression (0.40→0.65) over 50 years, consistent with the empirical pattern: post-Garcia, federal scope has expanded steadily while state autonomy has declined. This suggests the political safeguards reading has not protected state boundaries as promised.
 *
 * PERSPECTIVAL GAP:
 *   The reading produces radical perspectival divergence. The federal political majority sees pure Rope—coordination mechanism where states negotiate federalism terms through legislating. The swing-state coalition sees Tangled Rope—real agency but conditional on coalition power. States without coalition leverage see pure Snare—no court recourse, no political exit. The organized interstate coalition sees Tangled Rope—collective action possible but dependent on credible political threat. State legislatures see Snare—federal mandates without negotiating power. The analytical observer risks seeing Mountain (federalism is naturally protected by politics) when the structural data reveals a choice to defer to politics, which then extracts from powerless states. The perspectival gap reveals the reading's core asymmetry: it works for actors with political leverage and fails for those without. The reading's claim 'courts should stay out' benefits beneficiaries and suppresses alternatives. The claim that states 'defend themselves through Senate' is true only for states with Senate influence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the political safeguards mechanism. Beneficiaries (federal majorities, Senate coalition leaders) have low d → low/negative χ because they benefit from the mechanism—they experience coordination, not extraction. Victims (powerless states, non-coalition members) have high d → high χ because the mechanism extracts from them—they experience suppression without remedy. Swing states and organized coalitions occupy intermediate d values: they have agency (mobile/constrained exit) but are also targets of extraction if they lose political leverage. The analytical observer's d is derived from the mechanism's asymmetry: observers who perceive the mechanism as benefiting powerful actors will derive high d (experiencing it as extraction); those who perceive political process as intrinsically fair derive low d. The reading's core claim ('defer to politics') is itself a choice of which d values to prioritize—favoring beneficiaries' low d over victims' high d.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_process_sufficiency_empirical,
    'Do empirical federalism outcomes under the political safeguards reading actually protect state autonomy, or do they show steady federal scope expansion at the expense of judicially-unprotected state spheres?',
    'Longitudinal measurement: track allocation of regulatory domain over 50-year periods before and after adoption of political safeguards as primary doctrine. Compare federalism outcomes under political safeguards (post-Garcia v San Antonio) vs. under judicial policing (pre-Garcia, Tenth Amendment doctrine). Measure: percentage of policy domains subject to federal preemption, state revenue absorbed by federal compliance, state regulatory autonomy per sector.',
    'If political process protects state autonomy: reading confirmed, extractiveness ≤ 0.40. If steady federal expansion: reading is cover for federal extraction, extractiveness ≥ 0.70, reclassify to Snare. If mixed (some domains protected, others lost): Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_process_sufficiency_empirical, empirical, 'Empirical comparison of federalism outcomes under political vs. judicial safeguards').

omega_variable(
    senate_leverage_distribution,
    'Is Senate leverage for federalism protection distributed such that all states have meaningful political recourse, or only states with swing-state/coalition status?',
    'Analysis of Senate coalition formation in federalism disputes: track which states successfully defend federalism interests through political process (winning amendments, blocking overrides) vs. states that consistently lose federal preemption battles. Measure: proportion of states with credible Senate leverage on their core federalism interests vs. those without.',
    'If all states have leverage: political safeguards work (Rope/Tangled Rope). If leverage concentrated in swing states: only powered/organized states benefit, others face Snare conditions—reading becomes cover for extraction by coalition states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(senate_leverage_distribution, empirical, 'Distribution of Senate political leverage across states').

omega_variable(
    judicial_review_scope_trade,
    'Does abandoning judicial federalism review (Tenth Amendment policing) actually reduce extraction, or does it enable federal extraction that would otherwise be policed by courts?',
    'Counterfactual analysis: compare regulatory burdens on states in domains where courts retained federalism policing (commerce clause limits, Medicaid conditions) vs. domains where courts deferred entirely to political process. Measure: compliance costs, state revenue impact, degree of state autonomy retained.',
    'If deference reduces extraction: reading is sound, Tangled Rope confirmed. If abandoning review increases extraction: reading is cover for expansion, reclassify to Snare for victim perspectives. If trade-offs vary by domain: reading is domain-specific, not universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_review_scope_trade, empirical, 'Whether deferring to political process reduces or increases federal extraction of state autonomy').

omega_variable(
    reading_vs_anticommandeering_structural_difference,
    'Is the political safeguards reading structurally compatible with anticommandeering doctrine, or do they represent incompatible theories of federalism enforcement?',
    'Doctrinal analysis: Printz and New York v. US established anticommandeering (Congress may not commandeer state legislatures). Political safeguards reading defers federalism boundary to political process, not courts. If anticommandeering also applies: both readings coexist (courts police commandeering, politics handles scope). If anticommandeering is abandoned as part of pure political safeguards: readings foreclose each other.',
    'If compatible: both readings can be held simultaneously (coexists_with relation). If incompatible: one forecloses the other (forecloses relation). Current doctrine remains ambiguous—Supreme Court retains anticommandeering while also deferring scope questions to politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_anticommandeering_structural_difference, conceptual, 'Structural compatibility between political safeguards and anticommandeering readings').

omega_variable(
    naturalization_of_institutional_choice,
    'Is the reading''s claim that ''courts should defer'' a logical entailment from the Constitution''s text and structure, or a normative choice to prioritize political over judicial federalism enforcement?',
    'Interpretive analysis: identify the textual or structural premises the reading rests on. If the reading claims the text mandates political safeguards (necessary interpretation), the mountain perspective fails—the reading is a construction, not a discovery. If the reading acknowledges that the text could support either political or judicial safeguards, but the reading chooses political: the choice is normative, not inevitable, and the analytical mountain perspective is a false summit.',
    'If necessary interpretation: the mountain perspective has merit. If normative choice: the mountain perspective naturalizes a choice, false summit detection fires, reclassify analytical perspective to Tangled Rope or Snare (depending on whether the choice distributes extraction equitably).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_of_institutional_choice, conceptual, 'Whether the reading''s deferential stance is textually necessary or normatively chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenth_amendment__political_safeguards_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tent_tr_t0, tenth_amendment__political_safeguards_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tent_tr_t25, tenth_amendment__political_safeguards_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(tent_tr_t50, tenth_amendment__political_safeguards_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(tent_be_t0, tenth_amendment__political_safeguards_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(tent_be_t25, tenth_amendment__political_safeguards_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(tent_be_t50, tenth_amendment__political_safeguards_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tent_su_t0, tenth_amendment__political_safeguards_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tent_su_t25, tenth_amendment__political_safeguards_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(tent_su_t50, tenth_amendment__political_safeguards_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenth_amendment__political_safeguards_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenth_amendment__political_safeguards_reading, tenth_amendment__anticommandeering_doctrine).
narrative_ontology:affects_constraint(tenth_amendment__political_safeguards_reading, tenth_amendment__truism_reading).

% DUAL FORMULATION NOTE:
% The Tenth Amendment kernel decomposes into three structurally distinct readings with different ε values. This constraint (political_safeguards_reading) claims extractiveness ε=0.58 because political process leaves federalism boundaries to electoral coalitions, with asymmetric protection. The anticommandeering reading would have lower ε (judicially-policed boundary on commandeering only, narrower scope). The truism reading would have ε≈0.05 (no independent protection, mere restatement). Each reading is a complete constraint story; they share a kernel but differ in claims about where enforcement belongs and whom it protects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenth_amendment__political_safeguards_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
