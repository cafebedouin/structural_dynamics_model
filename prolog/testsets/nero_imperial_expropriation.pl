% ============================================================================
% CONSTRAINT STORY: nero_imperial_expropriation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nero_imperial_expropriation, []).

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
 *   constraint_id: nero_imperial_expropriation
 *   human_readable: Nero's Imperial Legitimacy via Expropriation and Spectacle
 *   domain: political/economic/ancient_rome
 *
 * SUMMARY:
 *   Nero's later reign (c. 59-68 CE) developed a systematic mechanism for
 *   concentrating imperial power and wealth through large-scale expropriation
 *   of aristocratic and merchant assets, justified through theatrical trials,
 *   accusations of treason, and political necessity. This constraint exhibits
 *   the hallmark structure of a Snare: high effective extraction (χ ≈
 *   0.70-0.85 from victims' perspectives), suppression of legal alternatives
 *   (confiscation courts are theaters with predetermined outcomes), and
 *   suppression of physical exit (exile or death for refusal).
 *   Simultaneously, the same structural arrangement functions as a
 *   coordination mechanism for the imperial household and court factions — it
 *   consolidates wealth into a central redistribution pool that rewards
 *   military loyalty, secures the Palatine Guard, and clarifies succession
 *   through control of resources. The theater ratio (0.78 at the constraint's
 *   peak) reflects the heavy reliance on legal theater: trials before the
 *   Senate, accusations of crimes, formal confiscation proceedings — all
 *   performative in their outcome but necessary to maintain the fiction of
 *   legitimacy. The constraint's extractiveness increases over the interval
 *   from 0.48 to 0.68 as Nero's financial pressures mount (Domus Aurea
 *   construction, military campaigns, court expansion) and his willingness to
 *   use expropriation as a primary revenue mechanism intensifies. The
 *   measurement trajectory shows both theater and extraction rising together
 *   — a signature of Goodhart drift, where procedural legitimacy becomes
 *   theater while the underlying extraction function grows more severe.
 *
 * KEY AGENTS:
 *   - Nero (Emperor): Primary beneficiary (institutional/arbitrage) — centralizes wealth and authority, consolidates court loyalty
 *   - Landed Aristocracy: Primary victim (powerless/trapped) — face arbitrary confiscation with no legal recourse or exit option
 *   - Provincial Merchant Class: Secondary victim (moderate/constrained) — face requisitions and expropriation with constrained mobility across provinces
 *   - Palatine Guard and Court Officers: Primary beneficiary (institutional/arbitrage) — receive steady salaries, promotions, and share of confiscated wealth
 *   - Provincial Governors: Mixed participant (powerful/mobile) — extract from provinces to send to Rome, but also extract for themselves; have constrained mobility within empire
 *   - Imperial Senate: Degraded actor (institutional/arbitrage) — retains ceremonial authority but has lost functional constraint power; maintains theater of legitimacy
 *   - Organized Resistance Coalition: Organized agents (organized/constrained) — secret meetings and coordinated defense; see sunset clause in regime change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nero_imperial_expropriation, 0.68).
domain_priors:suppression_score(nero_imperial_expropriation, 0.72).
domain_priors:theater_ratio(nero_imperial_expropriation, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nero_imperial_expropriation, extractiveness, 0.68).
narrative_ontology:constraint_metric(nero_imperial_expropriation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nero_imperial_expropriation, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nero_imperial_expropriation, snare).
narrative_ontology:human_readable(nero_imperial_expropriation, "Nero's Imperial Legitimacy via Expropriation and Spectacle").
narrative_ontology:topic_domain(nero_imperial_expropriation, "political/economic/ancient_rome").

domain_priors:requires_active_enforcement(nero_imperial_expropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nero_imperial_expropriation, nero_imperial_household).
narrative_ontology:constraint_beneficiary(nero_imperial_expropriation, palatine_guard).
narrative_ontology:constraint_beneficiary(nero_imperial_expropriation, favored_aristocrats).
narrative_ontology:constraint_victim(nero_imperial_expropriation, landed_aristocracy).
narrative_ontology:constraint_victim(nero_imperial_expropriation, merchant_class).
narrative_ontology:constraint_victim(nero_imperial_expropriation, imperial_treasury_stability).
narrative_ontology:constraint_victim(nero_imperial_expropriation, provincial_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPROPRIATED ARISTOCRAT (SNARE) — Roman senators and wealthy landowners face forced confiscation of estates and wealth with no legal recourse. Exit from Rome means exile or death. Suppression is absolute: imperial courts are theatrical and outcome-predetermined. The aristocrat experiences maximum extraction with no option to opt out or appeal.
constraint_indexing:constraint_classification(nero_imperial_expropriation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROVINCIAL MERCHANT (SNARE) — Wealthy traders and local elites face arbitrary taxation, requisition of goods, and expropriation under imperial pretext. Exit by fleeing to another province merely invites pursuit. Constrained mobility: the empire is unified and extractive across all regions. Suppression derives from military enforcement and provincial governor alignment with Rome.
constraint_indexing:constraint_classification(nero_imperial_expropriation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: PALATINE GUARD & COURT FACTION (ROPE) — Professional soldiers and imperial functionaries benefit from steady salaries, promotions, and shared plunder. Expropriation is a coordination mechanism: it consolidates wealth into the imperial system, which then distributes to court favorites and military officers. For this group, the constraint functions as Rope — a coordination solution to elite loyalty and reward allocation.
constraint_indexing:constraint_classification(nero_imperial_expropriation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PROVINCIAL GOVERNOR (TANGLED ROPE) — Governors occupy a hybrid position: they participate in expropriation (extracting from their provinces to send tribute to Rome), but also extract for themselves. Their exit option is migration to another province or retirement (mobile). The constraint offers them coordination (shared governance structure) and extraction (opportunity to skim provincial wealth). Suppression is moderate: governors can resist or reallocate, but face military consequences.
constraint_indexing:constraint_classification(nero_imperial_expropriation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: IMPERIAL SENATE (PITON) — The Senate retains formal consultative authority and ceremonial roles but has lost functional veto power over confiscations. Their continued meetings and formal approvals create theater of legitimacy for expropriation. The constraint is Piton: the Senate's institutional role persists through inertia, but its substantive coordination function (checking imperial power) has atrophied. Theater ratio is very high here (≥0.80) — the Senate legitimizes without constraining.
constraint_indexing:constraint_classification(nero_imperial_expropriation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ORGANIZED RESISTANCE (SCAFFOLD) — Some senators and wealthy families attempt coordinated resistance to arbitrary expropriation through secret meetings and mutual protection pacts. This coalition has a sunset clause: Nero's successor (Galba, Otho, Vitellius, Vespasian) may reverse confiscations and restore rule of law. The constraint appears as temporary from their vantage — a crisis to be endured with sunset in institutional reform or regime change. Suppression is high (Nero's spies), but agents retain some agency and see a path to the constraint's dissolution.
constraint_indexing:constraint_classification(nero_imperial_expropriation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE, CIVILIZATIONAL) — From a long-term structural perspective, Nero's expropriation system exhibits both coordination (consolidation of imperial power, regularized elite reward distribution) and pure extraction (rents flowing to the palace and favored court). The system is not a Mountain — it is contingent institutional design, not natural law. From the analytical view, the constraint is a hybrid Tangled Rope: genuine coordination functions (military loyalty, information flow, succession clarity) coexist with asymmetric extraction (wealth concentration). The constraint's extractiveness is high because the distribution skews heavily toward the beneficiaries.
constraint_indexing:constraint_classification(nero_imperial_expropriation, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nero_imperial_expropriation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nero_imperial_expropriation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nero_imperial_expropriation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nero_imperial_expropriation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nero_imperial_expropriation, TR),
    TR >= 0.70.

:- end_tests(nero_imperial_expropriation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significant wealth and resources from the expropriated classes (aristocrats, merchants, provincial elites) to the imperial palace and court. The average expropriated estate represents 30-50% of a family's wealth, and multiple confiscations of the same families occur. Not maximal (0.72+) because some victims retain residual assets, and succession to heirs eventually stabilizes some wealth. Suppression (0.72): High. Legal alternatives are theater (confiscation courts are predetermined). Physical exit is suppressed by empire-wide military authority. Appeal mechanisms are nonexistent or subordinate to imperial will. Information about confiscation plans is controlled. Theater ratio (0.78): High. Trials before Senate, formal accusations, legal proceedings, and theatrical denunciations form the legitimating facade. The actual mechanism (imperial decree + Praetorian Guard seizure) is simple coercion, but the constraint requires the trial theater to maintain the fiction that expropriation is judicial rather than arbitrary.
 *
 * PERSPECTIVAL GAP:
 *   Expropriated aristocrats and provincial merchants classify the constraint as Snare because they experience high extraction with no exit. The Palatine Guard and court officers classify it as Rope because they experience the same structural mechanism as a fair distribution and coordination system. The provincial governors classify it as Tangled Rope because they are both extractors (from provinces) and victims (from Rome) — mixed agency. The imperial Senate classifies it as Piton because their formal role persists but has no functional power to constrain expropriation; their continued trials and formal approvals are theater. The organized resistance coalition classifies it as Scaffold because they see a sunset in regime change and institutional reform. The analytical civilizational observer classifies it as Tangled Rope because both genuine coordination and asymmetric extraction are structurally present. The gap is wide: five distinct classifications from seven perspectives. This gap is diagnostic — it reveals that the constraint is NOT a Mountain (which would show invariance) and is NOT uniform-type (which would show consistency). The classification variance is the signature of a hybrid extraction-coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to expropriation flows. Nero and court factions are net beneficiaries with arbitrage options (can reallocate confiscated wealth flexibly) — low d, negative effective extraction chi. Expropriated aristocrats are pure targets with trapped exit — high d, high chi. Provincial merchants are victims with constrained mobility — moderate-high d, high chi. Provincial governors are both extractors (from their provinces) and targets (facing confiscation from Rome) — d around 0.50-0.55 (mixed), producing moderate chi. Senators retain arbitrage options (can adjust political loyalty to survive confiscation) — d lower than pure victims but higher than court factions. The analytical observer has analytical exit options and no exposure to confiscation — d around 0.72 (typical for analytical), producing moderate perspective chi but high base constraint extractiveness. The engine computes effective chi for each perspective by multiplying base extractiveness (0.68) × f(d) × σ(continental=1.1). For the expropriated aristocrat: high d (~0.90) → high f(d) (~1.35) → chi ≈ 0.68 × 1.35 × 1.1 ≈ 1.01 (capped at 1.0). For court factions: low d (~0.15) → low f(d) (~-0.01) → chi ≈ 0.68 × (-0.01) × 1.1 ≈ -0.007 (negative, reflecting benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT AND UNRESOLVED: This constraint exhibits classic mandatrophy risk. The court factions and institutional beneficiaries experience the constraint as pure Rope — a coordination mechanism for elite reward allocation and military loyalty. From their perspective, extractiveness should be low, suppression should be low, and the constraint should be classified as Rope or even Scaffold. However, the base extractiveness (0.68) and suppression (0.72) values reflect the lived experience of the expropriated victims — high extraction, high suppression. The mandatrophy arises from the false equivalence: 'Is this Rope (coordination) or Snare (extraction)?' The resolution is that both are true from different structural positions. The constraint is Tangled Rope at the analytical level — it has both genuine coordination function (consolidates imperial authority, regularizes elite distribution, clarifies succession) AND asymmetric extraction (wealth flows to palace, not proportional to contribution). The mandatrophy is resolved by accepting that the constraint is hybridized: coordination for the beneficiaries, extraction for the victims. The constraint is classified as Snare at the victim level (the primary target perspective) because the extraction dominates their experience, but as Rope at the beneficiary level (institutional perspective) because the coordination dominates their experience. The analytical civilizational observer must classify this as Tangled Rope with high extractiveness and moderate-high theater — a structural hybrid that stabilizes power but at the cost of equity. Mandatrophy resolved: the constraint is NOT mislabeled as pure Rope (which would hide the extraction) and is NOT mislabeled as pure Snare (which would hide the coordination). It is Tangled Rope with an acknowledged asymmetry in distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nero_survival_mechanism,
    'Does expropriation represent Nero''s rational response to legitimate treasury depletion, or is it a pathological extraction mechanism unmoored from fiscal necessity?',
    'Comparative analysis of imperial spending, treasury balances (reported in sources like Suetonius, Dio Cassius), and confiscation frequency before/after major public works projects. Reconstruction of actual fiscal versus claimed fiscal crises.',
    'If rational: constraint is Tangled Rope (coordination + extraction for necessary functions). If pathological: constraint is pure Snare (extraction divorced from system maintenance). Classification differs substantially at the institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nero_survival_mechanism, empirical, 'Whether expropriation responds to genuine fiscal crisis or pathological extraction').

omega_variable(
    elite_anticipation_logic,
    'Did wealthy Romans rationally anticipate expropriation and adjust behavior accordingly (hiding wealth, fleeing, publicly signaling loyalty), or did expropriation occur as a surprise mechanism that defeated prior coordination?',
    'Analysis of documented wealth transfers, property sales, and emigration patterns during Nero''s reign. Examination of courtier behavior and factional realignment. Comparison with contemporaneous confiscation predictability.',
    'If anticipated: suppression score drops (agents have partial adaptation strategies). If surprising: suppression remains high (no time to exit). Theater ratio interpretation shifts: anticipated suppression may manifest as conspicuous loyalty performance (theater); surprising suppression manifests as straightforward coercion (lower theater, higher raw extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_anticipation_logic, empirical, 'Whether expropriation was anticipated or surprise mechanism').

omega_variable(
    successor_reversal_rate,
    'To what extent did Nero''s immediate successors (Galba, Otho, Vitellius, Vespasian) actually reverse confiscations and restore legitimacy, validating the Scaffold sunset clause?',
    'Historical record analysis of restoration edicts, restitution to heirs, Senate rehabilitation, and relationship normalization. Quantification of reversed versus retained confiscations.',
    'If high reversal rate: Scaffold perspective is vindicated — the constraint had genuine sunset. If low reversal rate: Scaffold is aspirational framing rather than structural feature — expropriation mechanism persists across regimes and is not actually temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(successor_reversal_rate, empirical, 'Extent of confiscation reversal by Nero''s successors').

omega_variable(
    provincial_feedback_loop,
    'Does provincial expropriation feed back into imperial stabilization (through tribute and troop maintenance), or does it degrade provincial capacity and generate long-term imperial vulnerability?',
    'Analysis of provincial revolts, tax collection efficiency, military recruitment rates, and infrastructure decline during Nero''s reign and immediate aftermath. Measurement of extractive sustainability.',
    'If stabilizing: constraint is Tangled Rope (extraction serves coordination). If destabilizing: constraint is pure Snare (extraction undermines system stability). Long-term sustainability of the classification depends on this feedback mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_feedback_loop, empirical, 'Whether provincial expropriation stabilizes or destabilizes the imperial system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nero_imperial_expropriation, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nero_tr_t0, nero_imperial_expropriation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(nero_tr_t3, nero_imperial_expropriation, theater_ratio, 3, 0.68).
narrative_ontology:measurement(nero_tr_t7, nero_imperial_expropriation, theater_ratio, 7, 0.78).

% Extraction over time
narrative_ontology:measurement(nero_be_t0, nero_imperial_expropriation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(nero_be_t3, nero_imperial_expropriation, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(nero_be_t7, nero_imperial_expropriation, base_extractiveness, 7, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nero_imperial_expropriation, resource_allocation).
narrative_ontology:affects_constraint(nero_imperial_expropriation, roman_imperial_succession_instability).
narrative_ontology:affects_constraint(nero_imperial_expropriation, provincial_tax_collection_collapse).

% DUAL FORMULATION NOTE:
% Nero's expropriation system is distinct from but structurally linked to imperial succession instability (downstream) and provincial fiscal collapse (downstream). The expropriation constraint creates conditions that destabilize succession by concentrating power and wealth in a single person without institutional continuity, and it degrades provincial economic capacity, making future tax collection more difficult. These are separate constraint stories with their own metrics but are influenced by this constraint's state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nero_imperial_expropriation, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
