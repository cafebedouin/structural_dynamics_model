% ============================================================================
% CONSTRAINT STORY: spartan_oliganthropia_demographic_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spartan_oliganthropia_demographic_collapse, []).

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
 *   constraint_id: spartan_oliganthropia_demographic_collapse
 *   human_readable: Spartan Oliganthropia: Demographic Collapse Locked Within Closed Citizenship
 *   domain: ancient_politics/demographics
 *
 * SUMMARY:
 *   Between the 6th and 4th centuries BCE, Sparta locked itself into a
 *   demographic and political snare by enforcing hereditary closure of
 *   citizenship while the Spartiate (full citizen) population collapsed from
 *   approximately 8,000 to fewer than 1,000. The Lycurgan system prohibited
 *   incorporating helots (slave underclass) or perioikoi (free but
 *   non-citizen periphery) into the citizen body, treating citizenship as an
 *   immutable hereditary set. As wars (Persian Wars, Peloponnesian War),
 *   population diseases, and deliberate marriage restrictions (to preserve
 *   kleros estates indivisible) eroded the Spartiate base, the system could
 *   not adapt. The formal authority structure persisted on paper — still
 *   claiming 8,000-strong military capacity — while operational reality had
 *   contracted to perhaps 1,000 actual hoplites. The constraint's
 *   extractiveness escalated as the gap between claim and capacity widened.
 *   The system became increasingly performative: Lycurgan ritual (agoge,
 *   public dining, ceremonial displays) maintained the image of Spartan unity
 *   even as the demographic substrate had vanished. By Leuctra in 371 BCE,
 *   the theater was all that remained, and even that collapsed when Thebes
 *   defeated the Spartan army, shattering the myth of invincibility. The
 *   constraint demonstrates how immutability of a core institutional rule
 *   (closed citizenship) transforms an initially functional system into a
 *   snare: the refusal to revise the kernel created a gap that environmental
 *   change could not fill, extracting from helots, perioikoi, Spartiates, and
 *   eventually from Sparta's hegemonic position itself.
 *
 * KEY AGENTS:
 *   - Helot Population: Primary victims (powerless/trapped) — chattel slaves with zero exit; bear extraction through forced agricultural surplus and systematic terror (krypteia)
 *   - Perioikoi Population: Secondary victims (powerless/trapped) — free but non-citizen, economically and militarily exploited with no political voice
 *   - Spartiate Elite (Declining Base): Primary beneficiaries trapped within constraint (institutional/constrained) — benefit from concentration of status and power but cannot exit the closure mechanism they defend
 *   - Spartan Military System: Victim/observer (powerless/trapped at operational level) — formal capacity structure on paper (8,000) versus actual operational capacity (1,000); the gap itself is the constraint
 *   - Lycurgan Constitutional Order: Institutional actor (institutional/arbitrage) — maintains itself through ceremony and tradition; the kernel's immutability is treated as natural law
 *   - Neighboring Greek Poleis (Thebes, Corinth, etc.): Secondary actors (moderate/constrained) — experience Spartan hegemony as mixed coordination-extraction; Thebes breaks free in 379 BCE
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as a structural snare that extracted from everyone simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spartan_oliganthropia_demographic_collapse, 0.68).
domain_priors:suppression_score(spartan_oliganthropia_demographic_collapse, 0.78).
domain_priors:theater_ratio(spartan_oliganthropia_demographic_collapse, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spartan_oliganthropia_demographic_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(spartan_oliganthropia_demographic_collapse, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(spartan_oliganthropia_demographic_collapse, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spartan_oliganthropia_demographic_collapse, snare).
narrative_ontology:human_readable(spartan_oliganthropia_demographic_collapse, "Spartan Oliganthropia: Demographic Collapse Locked Within Closed Citizenship").
narrative_ontology:topic_domain(spartan_oliganthropia_demographic_collapse, "ancient_politics/demographics").

domain_priors:requires_active_enforcement(spartan_oliganthropia_demographic_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spartan_oliganthropia_demographic_collapse, surviving_spartiate_elite).
narrative_ontology:constraint_victim(spartan_oliganthropia_demographic_collapse, helot_population).
narrative_ontology:constraint_victim(spartan_oliganthropia_demographic_collapse, perioikoi_population).
narrative_ontology:constraint_victim(spartan_oliganthropia_demographic_collapse, spartan_military_viability).
narrative_ontology:constraint_victim(spartan_oliganthropia_demographic_collapse, spartiate_demographic_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HELOT POPULATION (SNARE) — Trapped in chattel servitude with zero exit. The Lycurgan constraint enforces permanent subordination: no path to citizenship, no legal personhood, systematic suppression through periodic massacre and psychological terror (krypteia). Extraction is maximal and unavoidable. The helots bear the full cost of Spartiate military mobilization (agricultural surplus) with no benefit and no escape except rebellion.
constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PERIOIKOI POPULATION (SNARE) — Free but not citizens; politically powerless and trapped in permanent subordinate status. Economically extractable via forced contribution to Spartan military projects. No voice in governance despite bearing the cost of Sparta's wars. Exit is blocked by geographic dependence and military subordination. The constraint treats them as permanently excluded from political community.
constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MILITARY OPERATIONAL CAPACITY (SNARE) — As Spartiate population declines from 8,000 to 1,000 over two centuries, the constraint traps Sparta in a paradox: formal authority structure on paper claims 8,000-strong force, but operational reality has collapsed to 1,000. This abstract capacity cannot exit the trap: the Lycurgan prohibition on citizen incorporation means no demographic replacement is possible. The constraint extracts credibility (the gap between claimed and actual strength) and eventually extraction capacity itself (Leuctra, 371 BCE).
constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: SPARTAN ELITE INSTITUTIONAL VIEW (MOUNTAIN — FALSE SUMMIT) — From the elite's constrained perspective, the Lycurgan system appears as unchangeable natural law: citizen-only status is presented as the immutable constitutional foundation, not a contingent policy choice. The closure is justified as metaphysical necessity ('the Spartan way'), making exit unthinkable and revision impossible. However, the structural data reveals this as a false summit: the closure benefits the surviving elite by concentrating power, resources, and status. The engine will flag this as naturalization of a contingent institutional arrangement that serves the declarer's interests.
constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LYCURGAN CONSTITUTIONAL SYSTEM (PITON) — The formal institutional structure persists through ceremonial re-enactment (military agoge, communal dining, public honors) long after functional capacity has eroded. Theater ratio is extremely high: the ritual apparatus of Lycurganism continues to perform Spartiate unity and strength even as the demographic base has vanished. The system maintains itself through inertia and historical prestige rather than any genuine coordinating function. By Leuctra (371 BCE), the theater is all that remains.
constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: NEIGHBORING GREEK POLEIS (TANGLED ROPE) — Thebes, Corinth, and other allies experience Spartan hegemony as mixed coordination and extraction. The coordination function is genuine: Sparta provides military deterrent against Persian threats (pre-Leuctra) and maintains stability in the Peloponnesian system. The extraction function is also genuine: Sparta demands tributary forces, subordinates local autonomy, and extracts prestige through leadership claims. Exit is costly (loss of security guarantee, economic retaliation) but not impossible (Thebes escapes in 379 BCE). This perspective sees the constraint as hybrid rather than pure exploitation.
constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From universal civilizational time, the constraint appears as a structural snare that locked Sparta into demographic collapse. The kernel's prohibition on citizen incorporation created a closed set that could not metabolize environmental change. As population declined from disease, war losses, and deliberate restriction of marriage (to preserve kleros estates), the system had no mechanism for demographic adaptation. The constraint extracted from everyone: helots through slavery, perioikoi through subordination, Spartiates through demographic attrition. The snare itself was the immutability of the citizenship rule.
constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spartan_oliganthropia_demographic_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spartan_oliganthropia_demographic_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spartan_oliganthropia_demographic_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spartan_oliganthropia_demographic_collapse, TR),
    TR >= 0.70.

:- end_tests(spartan_oliganthropia_demographic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising over the interval. The constraint's extractiveness increases as the population decline accelerates and the gap between formal claim and operational reality widens. Initial extractiveness (0.35 at 600 BCE) reflects the system functioning with adequate demographic base — the helot-perioikoi subordination is severe but operationally sustainable. By 371 BCE (0.68), the constraint is extracting credibility itself: Sparta claims strength it cannot deliver, and the extraction manifests as military failure. Suppression (0.78): Very high. The Lycurgan prohibition on citizen incorporation is enforced through: (a) legal prohibition (citizen status is hereditary; no path to incorporation), (b) cultural-ideological enforcement (citizenship is presented as metaphysical, unchangeable), (c) violent suppression (periodic krypteia against helots to prevent rebellion, harsh punishment for any who question the order), and (d) structural lock-in (kleros estate system makes demographic change economically catastrophic for elite). Theater ratio (0.81): Very high at endpoint (371 BCE), increasing over time from 0.45 (600 BCE). The Lycurgan ritual apparatus (military agoge, public dining, ceremonial honors) performs unity even as the demographic base erodes. By the 4th century, the performance has become almost entirely detached from function — Sparta's reputation for martial excellence persists long after actual military capacity has vanished. The theater increases because the gap between claim and capacity widens, requiring more performative work to maintain the image.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural phenomenon can be classified radically differently depending on observer position. For the elite defending the order, Lycurganism is immutable (mountain). For the operational military, it is a snare (cannot adapt demographically). For the subordinated populations, it is pure extraction (snare). For neighboring states, it is a hybrid (tangled rope) — they receive security guarantees but pay tribute. The gap is not empirical disagreement but structural: each perspective reflects a genuine feature of the constraint from that position. The false summit identification is crucial: it reveals that the elite's mountain classification (natural law) is serving their interests by making reform unthinkable. The analytical perspective shows that the constraint is not a mountain of nature but a socially constructed snare that trapped its own defenders.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries of the constraint are the surviving Spartiate elite: the closure concentrates citizenship, resources, and status within a shrinking group. Their directionality (d) is low — they experience effective extraction as negative (they benefit) because they have power and can theoretically exit (arbitrage option), though in practice they are psychologically and institutionally locked into defending the system they benefit from. The victims are helots, perioikoi, and the military system itself. Helots have maximum d (trapped powerless agents): they bear extraction without benefit or exit. Perioikoi have high d (trapped but free): they contribute but have no political voice. The operational military capacity has high d (trapped powerless): it is the mechanism through which extraction flows, and its collapse is the extraction's ultimate cost. The neighboring poleis have moderate d (constrained moderate agents): they benefit from Spartan military coordination but bear tribute costs; they can exit (Thebes does in 379 BCE) but at significant cost. The analytical observer has high d (analytical agent viewing asymmetric structure): they see the full extraction architecture that constrains everyone simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exhibits pure extraction (snare) from the perspective of those trapped — helots, perioikoi, declining Spartiates, the operational military system itself. The neighboring poleis experience mixed coordination and extraction (tangled rope) — Spartan leadership coordinates Greek defense but extracts tribute. The elite experience the constraint as immutable natural law (false summit mountain) — a perspective that serves their interests but misrepresents the contingency of the institutional choice. The mandatrophy is resolved by recognizing that all three classifications are true from their respective positions, but the false summit reveals that the 'natural law' framing is a strategy to prevent the revision that would redistribute the system's costs. The snare classification is the engine's classification from the analytical perspective: the constraint is fundamentally extractive, locked by the immutability of the citizenship rule, and extracting from everyone simultaneously. The real question — why did Sparta choose immutability? — is answered by the false summit: immutability concentrated power and status among the elite, making it rational for them to defend it as necessary, even as it doomed the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_decline_causation,
    'What proportion of Spartiate population decline (8,000 to 1,000) was caused by constraint-enforced closure vs. structural factors (war losses, disease, economic decline)?',
    'Comparative analysis with other Greek city-states that experienced similar wars but had open citizen incorporation mechanisms. Archaeological evidence for disease prevalence and agricultural capacity decline. Analysis of heritance laws and marriage restriction enforcement.',
    'If closure is primary driver: constraint is active snare with high causal responsibility. If structural factors dominate: constraint is passive snare (trapping response rather than extracting cause). Classification remains snare but causal role differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_decline_causation, empirical, 'Causation of population decline: constraint-enforced closure vs. structural factors').

omega_variable(
    incorporation_counterfactual,
    'If Sparta had adopted citizen incorporation of perioikoi and helots, would the hegemonic collapse have been prevented, delayed, or would it have occurred regardless?',
    'Comparative analysis with Athens (open incorporation, sustained power), Thebes (periodic incorporation, recovered strength), and other systems. Counterfactual modeling based on demographic trajectories.',
    'If incorporation prevents collapse: the constraint''s immutability is the extraction mechanism, and the false summit is a false false-summit (the kernel really was the problem). If collapse is inevitable regardless: the constraint is a symptom rather than a cause, and Sparta faces a genuine mountain-like demographic-structural limit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incorporation_counterfactual, conceptual, 'Counterfactual: whether citizen incorporation would have prevented collapse').

omega_variable(
    elite_benefit_distribution,
    'Did the surviving elite actually benefit from the closed system, or did they experience net extraction through unsustainable military burden and demographic pressure?',
    'Analysis of elite wealth accumulation, land distribution, political power concentration. Comparison of elite material conditions before vs. after closure enforcement intensification. Exit behavior: did wealthy Spartiates flee to colonies or relocate?',
    'If elite genuinely benefited: false summit is correct diagnosis — the mountain framing serves beneficiary interests. If elite were also trapped: the constraint is a mutual snare, and the mountain framing is pathological denial rather than strategic naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_benefit_distribution, empirical, 'Whether elite benefited or were co-trapped by closure mechanism').

omega_variable(
    lycurgan_reform_counterfactual,
    'Was the Lycurgan kernel genuinely irreformable, or did political economy of power concentration make reformers unable to mobilize support?',
    'Historical record of reform attempts and their failure patterns. Analysis of Agiad vs. Eurypontid factional competition. Examination of rhetoric used to defend closure: does it track immutable principle or concentrated privilege?',
    'If genuinely irreformable: the mountain classification carries epistemic weight — the system was locked by its own logic, not just by power interests. If reformable but blocked by factions: the constraint is socially constructed snare, and the mountain framing is a strategy to prevent mobilization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lycurgan_reform_counterfactual, conceptual, 'Whether Lycurgan kernel was genuinely irreformable or blocked by factional interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spartan_oliganthropia_demographic_collapse, 600, 371).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spartan_oligo_tr_t600, spartan_oliganthropia_demographic_collapse, theater_ratio, 600, 0.45).
narrative_ontology:measurement(spartan_oligo_tr_t450, spartan_oliganthropia_demographic_collapse, theater_ratio, 450, 0.68).
narrative_ontology:measurement(spartan_oligo_tr_t371, spartan_oliganthropia_demographic_collapse, theater_ratio, 371, 0.81).
narrative_ontology:measurement(spartan_oligo_tr_t500, spartan_oliganthropia_demographic_collapse, theater_ratio, 500, 0.55).

% Extraction over time
narrative_ontology:measurement(spartan_oligo_be_t600, spartan_oliganthropia_demographic_collapse, base_extractiveness, 600, 0.35).
narrative_ontology:measurement(spartan_oligo_be_t450, spartan_oliganthropia_demographic_collapse, base_extractiveness, 450, 0.52).
narrative_ontology:measurement(spartan_oligo_be_t371, spartan_oliganthropia_demographic_collapse, base_extractiveness, 371, 0.68).
narrative_ontology:measurement(spartan_oligo_be_t500, spartan_oliganthropia_demographic_collapse, base_extractiveness, 500, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spartan_oliganthropia_demographic_collapse, enforcement_mechanism).
narrative_ontology:affects_constraint(spartan_oliganthropia_demographic_collapse, spartan_helot_terror_system).
narrative_ontology:affects_constraint(spartan_oliganthropia_demographic_collapse, peloponnesian_hegemony_structure).
narrative_ontology:affects_constraint(spartan_oliganthropia_demographic_collapse, greek_citizenship_incorporation_contrast).

% DUAL FORMULATION NOTE:
% This constraint represents the dynamic interaction between institutional closure (the Lycurgan prohibition) and demographic change (population decline). The prohibition itself could be modeled as a separate mountain-type constraint (natural law of Spartan constitutionalism from the elite perspective), while the demographic collapse is a separate snare (structural trap for the military system). The coupling between them — the fact that the immutable prohibition could not accommodate demographic necessity — is what produces the extractive snare documented here. The upstream constraint (Lycurgan institutional form) affects the downstream constraint (demographic collapse snare) by blocking the adaptation mechanism that could have prevented the snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spartan_oliganthropia_demographic_collapse, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
