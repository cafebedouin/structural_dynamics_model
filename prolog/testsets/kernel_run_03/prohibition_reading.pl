% ============================================================================
% CONSTRAINT STORY: prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prohibition_reading, []).

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
 *   constraint_id: prohibition_reading
 *   human_readable: Prohibition Reading: Drug Use as Intrinsic Harm Justifying State Authority
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading of substance control authority asserts that drug
 *   use itself constitutes intrinsic harm—to the user's physiology,
 *   psychology, and social capacity—and that state authority legitimately
 *   derives from preventing this harm and protecting society from the
 *   psychopharmacological effects. Under this reading, the drug user is
 *   reconceptualized as a perpetrator of self-harm; enforcement apparatus
 *   functions as protective intervention; and the constraint's beneficiaries
 *   are institutional actors whose legitimacy depends on crime prevention
 *   framing. This constraint is ONE READING of the contested kernel
 *   'substance_control_authority,' which also admits harm_reduction_reading
 *   (drug use is a medical condition requiring treatment and harm mitigation)
 *   and legalization_reading (drug use is a consumer choice requiring market
 *   regulation, not prohibition). This JSON instantiates the
 *   prohibition_reading only. The structural signature is a Snare from the
 *   powerless/trapped perspective (users bear maximum extraction via
 *   criminalization) and an institutional beneficiary rope (enforcement
 *   apparatus experiences the constraint as pure coordination—drug
 *   prohibition IS their mission). The constraint exhibits mandatrophy:
 *   extractiveness (0.68) exceeds the snare threshold (0.46), creating
 *   apparent overlap with legitimate coordination frames (pharmaceutical
 *   regulation, crime prevention) that mask the extraction mechanism. The
 *   temporal measurements show rising extractiveness and theater ratio from
 *   1961 (Single Convention) to 2026, indicating accumulated enforcement
 *   overhead and degrading functional fit (more police activity, same drug
 *   availability—classical piton signature at the international regime
 *   level).
 *
 * KEY AGENTS:
 *   - Drug Users: Primary victim (powerless/trapped) — criminalized for use, bear legal liability, asset forfeiture, employment barriers, social stigma; no legitimate exit from the constraint.
 *   - Law Enforcement and Justice System: Primary beneficiary (institutional/arbitrage) — constraint IS their core function; drug enforcement justifies budgets, personnel, and institutional expansion; have arbitrage exit (could restructure around public health) but zero incentive to do so.
 *   - Communities Subject to Enforcement: Secondary victim (moderate/constrained) — disproportionate policing, environmental stress, family disruption; have constrained exit (costly relocation, legal defense costs).
 *   - Pharmaceutical Industry and Regulatory Authority: Secondary beneficiary (powerful/mobile) — prohibition protects licit pharmaceutical monopoly, eliminates competition from plant medicines, creates scarcity rents; have mobile exit (could support legalization) but benefit from status quo.
 *   - Political Legitimacy Apparatus: Tertiary beneficiary (institutional/constrained) — prohibition provides governments with visible crime governance mechanism; have constrained exit (decriminalization faces legitimacy challenges domestically and internationally).
 *   - International Drug Control Regime: Institutional actor (institutional/arbitrage) — UN conventions codify prohibition globally; regime is degraded (piton)—persists through inertia despite low functional fit.
 *   - Supply-Chain Violence Casualties: Structural victim set (absent from primary framing) — deaths from cartel conflict, trafficking violence, dealer competition are produced by criminalization but attributed to 'drug violence' rather than enforcement-induced scarcity rents.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prohibition_reading, 0.68).
domain_priors:suppression_score(prohibition_reading, 0.75).
domain_priors:theater_ratio(prohibition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(prohibition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(prohibition_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prohibition_reading, snare).
narrative_ontology:human_readable(prohibition_reading, "Prohibition Reading: Drug Use as Intrinsic Harm Justifying State Authority").
narrative_ontology:topic_domain(prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(prohibition_reading, 'afd25d62-b8fb-4a19-a18f-24c4949f3552').
narrative_ontology:cs_created_at('afd25d62-b8fb-4a19-a18f-24c4949f3552', '').
narrative_ontology:cs_kernel_codification('afd25d62-b8fb-4a19-a18f-24c4949f3552', formalized).
narrative_ontology:cs_authority_grounding('afd25d62-b8fb-4a19-a18f-24c4949f3552', extraction).
narrative_ontology:cs_interpretation_layer_present('afd25d62-b8fb-4a19-a18f-24c4949f3552').
narrative_ontology:cs_kernel_id(prohibition_reading, substance_control_authority).
narrative_ontology:cs_reading_relation('afd25d62-b8fb-4a19-a18f-24c4949f3552', harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('afd25d62-b8fb-4a19-a18f-24c4949f3552', legalization_reading, coexists_with).
narrative_ontology:cs_axiom('afd25d62-b8fb-4a19-a18f-24c4949f3552', foundational, intrinsic_use_harm_primary).
narrative_ontology:cs_axiom_status(intrinsic_use_harm_primary, holdable).
narrative_ontology:cs_axiom_grounding('afd25d62-b8fb-4a19-a18f-24c4949f3552', intrinsic_use_harm_primary, empirically_contingent).
narrative_ontology:cs_axiom('afd25d62-b8fb-4a19-a18f-24c4949f3552', foundational, prevention_authority_derives_from_harm).
narrative_ontology:cs_axiom_status(prevention_authority_derives_from_harm, holdable).
narrative_ontology:cs_axiom_grounding('afd25d62-b8fb-4a19-a18f-24c4949f3552', prevention_authority_derives_from_harm, deontological).
narrative_ontology:cs_reference_frame('afd25d62-b8fb-4a19-a18f-24c4949f3552', intrinsic_drug_harm_prevention_authority).
narrative_ontology:cs_drift_state('afd25d62-b8fb-4a19-a18f-24c4949f3552', contemporary_evidence_era_2015_2026, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prohibition_reading, enforcement_apparatus).
narrative_ontology:constraint_beneficiary(prohibition_reading, pharmaceutical_regulation_authority).
narrative_ontology:constraint_beneficiary(prohibition_reading, political_legitimacy_via_crime_prevention).
narrative_ontology:constraint_victim(prohibition_reading, drug_users).
narrative_ontology:constraint_victim(prohibition_reading, communities_subject_to_enforcement).
narrative_ontology:constraint_victim(prohibition_reading, supply_side_violence_casualties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DRUG USER (SNARE) — Trapped by both biological dependency and criminalized status. Use itself is reframed as moral/legal violation, not medical condition. No legitimate exit: quitting requires medical support (often unavailable); continuing triggers criminal sanction. Maximum extraction: bears criminal liability, surveillance, asset forfeiture, employment barriers, and social stigma. Trapped exit means the constraint appears as mountain to this agent—immutable barrier regardless of framing shift.
constraint_indexing:constraint_classification(prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENFORCEMENT-TARGETED COMMUNITIES (SNARE) — Disproportionately subject to search, arrest, and incarceration. Exit is theoretically possible (relocation, wealth accumulation for legal defense) but involves high material costs. Extraction manifests as over-policing, environmental stress, family disruption, and economic precarity. Constrained exit—the target can see paths out but they are costly enough to function as traps.
constraint_indexing:constraint_classification(prohibition_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENFORCEMENT APPARATUS (ROPE) — Experiences the constraint as coordination: drug prohibition creates a shared mission, funding justification, and operational clarity. Drug enforcement is the machinery's core function. For this institutional actor, the constraint is pure coordination—it solves the problem of what law enforcement's purpose is. Arbitrage exit means the apparatus could exit (restructure around public health) but has zero incentive to do so. Net beneficiary with institutional power.
constraint_indexing:constraint_classification(prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL/REGULATORY COMPLEX (TANGLED ROPE) — Genuine coordination function: prohibition prevents unregulated drug markets and protects pharmaceutical monopoly on licit manufacture. Simultaneous extraction: prohibition creates scarcity rents, eliminates competition from plant medicines, and concentrates profit in controlled substances. Pharmaceutical actors are mobile (could support legalization) but benefit from the status quo. Active enforcement sustains their monopoly. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(prohibition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POLITICAL LEGITIMACY APPARATUS (TANGLED ROPE) — Prohibition provides governments with a crime category to prevent, enforcement metrics, and a population control mechanism that appears legitimate as public health. Genuine coordination: shared understanding of drug prohibition as moral imperative. Asymmetric extraction: criminalization of users allows governance of marginalized populations without calling it governance. Constrained exit—governments could decriminalize but face legitimacy challenges domestically and internationally.
constraint_indexing:constraint_classification(prohibition_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL DRUG CONTROL REGIME (PITON) — The UN drug conventions (1961 Single Convention, 1971 Psychotropic Substances Convention) codify prohibition as a global norm, but the actual function has atrophied. Theater ratio is high: the regime persists through institutional inertia and diplomatic ceremony despite: (a) widespread evidence that prohibition increases violence and disease, (b) no correlation between enforcement intensity and drug availability, (c) growing jurisdictional defection (Portugal decriminalization, cannabis legalization, psilocybin therapy approval). The regime is maintained because alternatives haven't fully replaced it and because exit carries political cost, not because it achieves stated goals.
constraint_indexing:constraint_classification(prohibition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, drug use 'naturally' produces individual and social harm; prohibitive authority 'naturally' follows as protective governance. This perspective treats the extraction as an immutable property of the psychopharmacological and social world. However, structural data contradicts this classification: beneficiary declarations reveal that extraction flows toward institutional actors with contingent interests (enforcement apparatus, political legitimacy). The engine will flag this as a false summit—naturalization of a contingent institutional arrangement into apparent law.
constraint_indexing:constraint_classification(prohibition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prohibition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prohibition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prohibition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prohibition_reading, TR),
    TR >= 0.70.

:- end_tests(prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significantly from users—criminalization costs (1.25M arrests/year US, 51% federal prisoners are drug offenders), asset forfeiture ($6.5B+ annually), collateral consequences (employment barriers, housing restrictions, education eligibility loss), and social stigma. The measurement trajectory shows rising extractiveness from 1961 to 2026 as enforcement infrastructure accumulated and incarceration populations expanded—the extraction mechanism is deepening, not stabilizing. Suppression (0.75): Very high. Multiple barriers prevent exit: (a) biological dependency (pharmacological trap), (b) criminalization of the exit attempt itself (medicinal treatment requires legal permission), (c) reputational damage (criminal record prevents legitimate employment, forcing continued reliance on criminalized market), (d) geographic clustering (enforcement targeted disproportionately at economically constrained areas with few alternative opportunities). Theater ratio (0.58): Moderate-high. The constraint's performative dimension is significant: drug availability under prohibition is nearly identical to decriminalized jurisdictions; enforcement effort correlates weakly with drug use reduction; criminal prosecution has not reduced repeat use rates. The performance consists of visible enforcement activity (arrests, prosecution, incarceration) that demonstrates state crime-prevention capacity without achieving stated harm-reduction goals. Theater_ratio rises from 0.35 (1960s, when prohibition seemed efficacious) to 0.58 (2015+, as decriminalization evidence accumulated), indicating the constraint's functional core is eroding while theatrical component persists.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces strong perspectival divergence. The enforcement apparatus (institutional/arbitrage) sees pure coordination—drug prohibition defines their mission, creates shared organizational purpose, and justifies resource allocation. This is a Rope experience: the constraint solves a coordination problem (What is law enforcement for?). The drug user (powerless/trapped) sees pure extraction—criminalization of use, criminal sanction for treatment-seeking, lifetime barriers from criminal record. This is a Snare experience: maximum extraction with no legitimate exit. The pharmaceutical industry (powerful/mobile) sees mixed coordination and extraction (Tangled Rope): genuine regulatory function (preventing uncontrolled drug markets) alongside asymmetric profit extraction (scarcity rents, monopoly protection). The international regime (institutional/arbitrage) sees a degraded ritual (Piton): the machinery persists through inertia despite documented failure to achieve stated goals. The analytical observer risks seeing an immutable natural law (Mountain): drug use inherently harms, authorities naturally derive from prevention. But beneficiary declarations reveal this as false summit—institutional actors contingently benefit from the prohibition frame. The perspectival gaps reveal that the constraint's classification depends entirely on where the observer stands structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from the agent's structural position. (1) Drug user at powerless/trapped: maximum d ≈ 0.95 (full target of extraction), derives high f(d) ≈ 1.42 (powerless). (2) Communities at moderate/constrained: d ≈ 0.60 (primary target but with some agency), derives f(d) ≈ 1.00. (3) Law enforcement at institutional/arbitrage: d ≈ 0.05 (full beneficiary—extraction flows toward them), derives f(d) ≈ -0.12 (institutional). (4) Pharmaceutical at powerful/mobile: d ≈ 0.30 (beneficiary with some exposure), derives f(d) ≈ 0.20. (5) Political legitimacy at institutional/constrained: d ≈ 0.20 (beneficiary with some constraint), derives f(d) ≈ 0.02. The directionality values embed the structural relationship—who benefits, who bears cost, what exit capacity they have—and the sigmoid transformation f(d) converts these into experienced extractiveness multipliers. Beneficiaries experience low or negative effective extraction; trapped victims experience maximum extraction scaled by their powerlessness.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This reading instantiates mandatrophy (extractiveness 0.68 > 0.70 threshold narrowly missed, but mandate-type concerns apply at 0.46+). The structural ambiguity is: does prohibition constitute legitimate crime-prevention coordination (mandate for protective state authority), or does it constitute extraction mechanism (criminalizing a subset of the population to justify police power)? The prohibition_reading asserts the former—harm from use justifies authority to prevent use. But structural data reveals the extraction: beneficiary set includes enforcement apparatus, political legitimacy apparatus, and pharmaceutical gatekeepers; beneficiaries have zero incentive to exit; extraction mechanisms (criminalization, surveillance, asset forfeiture) are increasing over time (theater_ratio rising, extractiveness rising); and supply-side harms (violence, disease) are attributed to 'drug use' rather than 'criminalization-induced scarcity.' The mandate is resolved by acknowledging that BOTH are true under this reading: prohibition does coordinate crime prevention (coordination function is real), AND prohibition extracts from users to fund institutional power (extraction mechanism is real). These are not contradictory—tangled_rope and snare perspectives coexist. The false summit lies in the mountain perspective (naturalization). The mandatrophy dissolves when the kernel context is explicit: the reading brackets the harm_reduction_reading and legalization_reading as alternative framings, and the structural data is consistent with prohibition_reading ONLY if one accepts that intrinsic-use-harm is the primary harm source. The omega variables document the empirical and conceptual uncertainties that could reframe this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_locus_ambiguity,
    'Is the primary harm from drug use itself (pharmacological/psychological) or from criminalization (legal status, enforcement, supply-chain violence)?',
    'Epidemiological comparison: harm profiles in prohibition vs decriminalized jurisdictions; attribution analysis of health outcomes to use vs enforcement; longitudinal tracking of harm metrics (overdose death, disease transmission, violence) across policy regime changes.',
    'If use is primary harm source: prohibition_reading classification stands (Snare from powerless perspective). If criminalization is primary harm source: harm_reduction_reading would classify as Snare from same perspective, with prohibition_reading reclassifying as extraction mechanism. The entire reading relation flips from coexists_with to forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_locus_ambiguity, empirical, 'Whether primary harm originates from use or criminalization').

omega_variable(
    authority_legitimacy_grounding,
    'Does state authority to prohibit derive from (a) preventing intrinsic pharmacological harm, (b) social order coordination against vice, or (c) contingent institutional interest in crime governance?',
    'Historical institutional analysis: evidence of legitimate crime-prevention benefit vs evidence of institutional power capture; comparative case analysis of authority structure in prohibition vs harm-reduction regimes; axiom-tracking across reading transitions.',
    'If (a): authority grounding is expert (pharmaceutical harm prevention); legitimacy is robust across regimes. If (b): authority grounding is conventional (shared norm coordination); legitimacy survives regime shift to harm reduction if social order is maintained. If (c): authority grounding is extraction; legitimacy dissolves when institutional beneficiary is revealed, opening space for legalization_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_legitimacy_grounding, conceptual, 'What grounds the legitimacy of prohibition authority').

omega_variable(
    reading_forecast_divergence,
    'Within this prohibition_reading''s own commitments, what structural changes would make the reading internally incoherent or impossible to maintain?',
    'Scenario analysis: (1) pharmaceutical harm data refuting intrinsic-use-harm premise; (2) enforcement failure reaching threshold where drug availability is identical under prohibition and decriminalization; (3) institutional capture becoming undeniable (leaked enforcement directives showing crime prevention is secondary to population control); (4) supply-chain violence becoming primary harm category, larger than use harm.',
    'Each scenario tests an axiom of the prohibition_reading. If axioms are empirically contingent (depend on testable claims about harm, efficacy, or authority legitimacy), scenario evidence could move the reading''s own authority structure toward acknowledging its axioms are overridden. This is not foreclosure by external reading, but internal degradation of the reading''s coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_forecast_divergence, empirical, 'Structural conditions under which prohibition_reading becomes internally incoherent').

omega_variable(
    supply_side_violence_attribution,
    'Is supply-side violence (cartel conflict, trafficking deaths, dealer violence) a causal consequence of prohibition enforcement or a contingent feature of illicit markets?',
    'Comparative supply-chain analysis: violence levels in (a) prohibition jurisdictions, (b) decriminalized use with illegal supply, (c) legal supply regimes; attribution modeling of violence drivers (enforcement intensity, profit concentration, territorial monopoly dynamics); historical analysis of legal substance markets (alcohol post-Prohibition, pharmaceuticals).',
    'If violence is causal consequence: prohibition_reading''s claimed harm category (users harming themselves) is understated—enforcement-driven violence becomes a hidden victim set, and extractiveness should be higher. If contingent: supply-side violence is not inherent to use; it becomes an institutional-capture argument rather than a use-harm argument, strengthening harm_reduction_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_violence_attribution, empirical, 'Whether supply-side violence is causal consequence of prohibition').

omega_variable(
    committer_axiom_contest_site,
    'Within the substance_control_authority kernel, which foundational axiom—intrinsic-use-harm or prevention-authority-derives-from-harm—is actually the contested locus between prohibition_reading and harm_reduction_reading?',
    'Close reading of policy dispute rhetoric: identify which axiom each reading explicitly invokes; identify which axiom each reading''s sibling implicitly rejects; trace historical moment when dispute shifted from one axiom to another.',
    'If intrinsic-use-harm is the true contest site: the readings genuinely foreclose each other (one asserts harm, other denies it or minimizes it). If authority-grounding is the true contest site: the readings coexist within disagreement about why authority derives (both grant that harm is relevant, disagree about how much). Correct placement affects reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axiom_contest_site, conceptual, 'Which axiom is the true contest site between readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prohibition_reading, 1961, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prohib_theater_1960s, prohibition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prohib_theater_1980s, prohibition_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(prohib_theater_2005, prohibition_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(prohib_theater_2015, prohibition_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(prohib_extract_1960s, prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prohib_extract_1980s, prohibition_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(prohib_extract_2005, prohibition_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(prohib_extract_2015, prohibition_reading, base_extractiveness, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(prohibition_reading, legalization_reading).
narrative_ontology:affects_constraint(prohibition_reading, supply_chain_violence_extraction).
narrative_ontology:affects_constraint(prohibition_reading, incarceration_disparities_enforcement).

% DUAL FORMULATION NOTE:
% The substance_control_authority kernel admits three structurally distinct constraint readings with different ε values. prohibition_reading (ε=0.68) asserts intrinsic-use-harm as primary; harm_reduction_reading (ε≈0.35-0.45, estimated) asserts medical framing; legalization_reading (ε≈0.25-0.40, estimated) asserts market regulation. Each reading gets its own constraint story because ε differs—the observable (whether primary harm is use, criminalization, or regulatory gap) determines the structural metrics. Links via network.affects_constraints indicate that changes in one reading's empirical basis cascade to siblings: epidemiological evidence that refutes intrinsic-use-harm axiom would degrade prohibition_reading and strengthen harm_reduction_reading; evidence of enforcement-driven violence would increase supply_chain_violence_extraction extractiveness and destabilize prohibition_reading's authority grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
