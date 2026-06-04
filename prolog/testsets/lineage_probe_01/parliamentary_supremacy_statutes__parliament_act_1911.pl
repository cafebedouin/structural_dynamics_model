% ============================================================================
% CONSTRAINT STORY: parliamentary_supremacy_statutes__parliament_act_1911
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliament_act_1911, []).

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
 *   constraint_id: parliamentary_supremacy_statutes__parliament_act_1911
 *   human_readable: Parliament Act 1911 — Delimitation of Hereditary Chamber Power
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Parliament Act 1911 represents the definitive settlement of the
 *   bicameral supremacy question: the elected Commons would control money
 *   bills absolutely, and the hereditary House of Lords would retain only a
 *   two-year delaying veto on other legislation, enforceable by the threat of
 *   mass peer creation that would permanently dilute aristocratic power. This
 *   constraint exemplifies Tangled Rope classification because it combines
 *   genuine coordination (the need for a revising chamber, the procedural
 *   legitimacy that review provides) with asymmetric extraction (the
 *   suppression of hereditary institutional veto capacity under duress). The
 *   peerage was structurally mobilized to choose between two bad outcomes:
 *   yield legislative power or face permanent institutional dilution. The
 *   threat was credible — the Liberal government had secured the King's
 *   commitment to create enough peers to override Lords obstruction if
 *   necessary. This constraint instantiates one reading of the parliamentary
 *   supremacy kernel, distinct from the Acts of Union (which established the
 *   composite sovereignty), the Fixed-term Parliaments Act (which briefly
 *   formalized election-calling power before repealing it), and the
 *   Parliament Act 1949 (which used the 1911 mechanism to shorten the Lords
 *   delay to one year — the Act amending the terms of its own authorization).
 *   The 1911 reading settles the chambers through suppression of the
 *   hereditary veto, establishes electoral supremacy as the legitimate
 *   foundation, and reduces the aristocratic blocking capacity to a delay
 *   mechanism dependent on procedural deference rather than institutional
 *   right.
 *
 * KEY AGENTS:
 *   - Elected Commons: Primary beneficiary (institutional/arbitrage) — gains absolute control over money bills and budgets; can threaten peer creation to enforce legislative will; benefits from electoral legitimacy now encoded in institutional structure
 *   - Hereditary Peerage: Primary victim (powerless/trapped) — faces binary choice: accept veto suppression or trigger institutional dilution; no viable exit that preserves hereditary legislative power; bears cost of power redistribution under threat
 *   - Liberal Government: Strategic actor (institutional/arbitrage) — orchestrates peer-creation threat; uses it to extract peerage capitulation; legitimates extraction as enforcement of electoral supremacy
 *   - Conservative Party: Institutional victim-beneficiary (organized/constrained) — loses institutional blockade capacity but can coordinate through party politics in Commons; partly extracts party advantage from Conservative-dominated Lords, but overall loses institutional leverage
 *   - Reform Coalition: Organized agent (organized/constrained) — views 1911 as temporary settlement pending fuller democratization; sees Lords delay as sunset logic; benefits from Commons sovereignty but sees further reform as inevitable
 *   - Reformed Lords Institution: Institutional legacy (institutional/arbitrage) — adapts to revising role; maintains ceremonial legitimacy; theater ratio rises as functional power declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliamentary_supremacy_statutes__parliament_act_1911, 0.38).
domain_priors:suppression_score(parliamentary_supremacy_statutes__parliament_act_1911, 0.52).
domain_priors:theater_ratio(parliamentary_supremacy_statutes__parliament_act_1911, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__parliament_act_1911, extractiveness, 0.38).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__parliament_act_1911, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__parliament_act_1911, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliamentary_supremacy_statutes__parliament_act_1911, tangled_rope).
narrative_ontology:human_readable(parliamentary_supremacy_statutes__parliament_act_1911, "Parliament Act 1911 — Delimitation of Hereditary Chamber Power").
narrative_ontology:topic_domain(parliamentary_supremacy_statutes__parliament_act_1911, "political/legal").

domain_priors:requires_active_enforcement(parliamentary_supremacy_statutes__parliament_act_1911).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parliamentary_supremacy_statutes__parliament_act_1911, 'da06c183-1918-4532-946d-e50a15f38f4e').
narrative_ontology:cs_kernel_codification('da06c183-1918-4532-946d-e50a15f38f4e', formalized).
narrative_ontology:cs_authority_grounding('da06c183-1918-4532-946d-e50a15f38f4e', extraction).
narrative_ontology:cs_interpretation_layer_present('da06c183-1918-4532-946d-e50a15f38f4e').
narrative_ontology:cs_reading_relation('da06c183-1918-4532-946d-e50a15f38f4e', parliamentary_supremacy_statutes__acts_of_union, coexists_with).
narrative_ontology:cs_reading_relation('da06c183-1918-4532-946d-e50a15f38f4e', parliamentary_supremacy_statutes__fixed_term_parliaments_act, influences).
narrative_ontology:cs_reading_relation('da06c183-1918-4532-946d-e50a15f38f4e', parliamentary_supremacy_statutes__parliament_act_1949, influences).
narrative_ontology:cs_axiom('da06c183-1918-4532-946d-e50a15f38f4e', foundational, democratic_electoral_supremacy).
narrative_ontology:cs_axiom_status(democratic_electoral_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('da06c183-1918-4532-946d-e50a15f38f4e', democratic_electoral_supremacy, deontological).
narrative_ontology:cs_axiom('da06c183-1918-4532-946d-e50a15f38f4e', secondary, two_year_delay_as_revising_not_blocking).
narrative_ontology:cs_axiom_status(two_year_delay_as_revising_not_blocking, holdable).
narrative_ontology:cs_axiom_grounding('da06c183-1918-4532-946d-e50a15f38f4e', two_year_delay_as_revising_not_blocking, conventional).
narrative_ontology:cs_reference_frame('da06c183-1918-4532-946d-e50a15f38f4e', hereditary_veto_as_constitutional_right).
narrative_ontology:cs_drift_state('da06c183-1918-4532-946d-e50a15f38f4e', contemporary_representative_democracy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('da06c183-1918-4532-946d-e50a15f38f4e', '').
narrative_ontology:cs_kernel_id(parliamentary_supremacy_statutes__parliament_act_1911, parliamentary_supremacy_statutes).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliamentary_supremacy_statutes__parliament_act_1911, elected_commons).
narrative_ontology:constraint_beneficiary(parliamentary_supremacy_statutes__parliament_act_1911, electoral_constituencies).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__parliament_act_1911, hereditary_peerage).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__parliament_act_1911, conservative_institutional_blocking_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEREDITARY PEERAGE (SNARE) — The peerage faced a binary choice: accept permanent loss of money bill veto and reduction of legislative power to two-year delay, or trigger mass peer creation that would dilute their institutional power forever. No exit option existed that preserved hereditary veto authority. The threat of peer creation was structural — not a bluff. The peerage's legislative capacity was extracted under coercion. Experienced extraction is maximal from this position.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSERVATIVE PARTY (TANGLED ROPE) — The Conservative caucus in the Commons benefited from the Lords as an institutional blocking mechanism against progressive legislation; losing that leverage was extraction. However, the party also coordinated with the mass peer creation threat — the Tories used the supermajority-triggering mechanism as a tactical coordination tool to negotiate power-sharing in the reformed chamber. Mixed function: genuine legislative coordination + asymmetric extraction of conservative institutional blockade capacity.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELECTED COMMONS (ROPE) — The Commons benefited directly: money bills passed into law without veto obstruction; budgets became enforceable by electoral mandate. The constraint coordinated legislative function (enabling timely budget passage) with net extraction of conservative institutional veto power. The Commons experienced this as coordination of authentic democratic function — taxation and spending by popular representation. Arbitrage: the Commons could leverage the peer-creation threat to achieve this outcome and then maintain it.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — The Liberal government and nascent Labour movement saw the 1911 Act as a transitional settlement: the two-year Lords delay was a temporary concession to hereditary privilege pending full chamber reform. The delaying veto itself was sunset logic — as democratic norms matured, further curtailment would follow (realized in 1949). Extraction was justified as a temporary compression of an obsolete institution. The coalition had clear exit path: full Lords reform or reduction to ceremonial role. Theater_ratio lower for this perspective — the delaying mechanism is functional, not performative.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMED HOUSE OF LORDS (PITON) — Over the long term, the 1911 Act reduced the Lords to a revising chamber whose delays serve theatrical legitimation more than functional veto. The Lords review legislation, demand reconsideration, invoke procedure — largely performative maintenance of institutional status. The peerage lost real power but gained ceremonial legitimacy through continuing participation. The delaying veto persists through inertia and constitutional theatre, not because it functions as originally intended. Theater ratio ≥ 0.70 for this civilizational perspective.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Parliament Act 1911 enacts a natural law of constitutional mechanics: legislative supremacy cannot coexist with unlimited hereditary veto. Unicameral supremacy (Commons unrestricted) is the attractor state; bicameral systems must resolve toward it or collapse into instability. This perspective sees the 1911 Act as discovering and formalizing an immutable constraint on sustainable governance. However, the structural data contradicts the mountain classification: the specific beneficiaries (elected Commons, electoral representation) and specific extraction mechanism (hereditary veto suppression under peer-creation threat) reveal this as a contingent institutional arrangement benefiting identifiable parties, not a natural law. The false summit signature applies: what appears as immutable constitutional logic is actually the codification of democratic supremacy as an extractive power settlement.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliamentary_supremacy_statutes__parliament_act_1911_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__parliament_act_1911, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(parliamentary_supremacy_statutes__parliament_act_1911, TR),
    TR >= 0.70.

:- end_tests(parliamentary_supremacy_statutes__parliament_act_1911_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts the hereditary veto from the peerage under structural threat (peer creation), but the extraction is not maximal because: (1) the peerage retains a two-year delay mechanism that provides some functional leverage; (2) the constraint establishes a new equilibrium (not ongoing extraction) that the Lords can coordinate within; (3) much of the extraction is reframed as 'enforcement of democratic legitimacy' rather than pure power-taking, which reduces the experienced extraction once the new regime bed down. Over the 10-year interval, extractiveness declines as the two-year delay becomes routinized and the Lords develops its revising role. Suppression (0.52): Moderate-high. The suppression operated through the peer-creation threat (structural coercion) and loss of institutional legitimacy for hereditary veto (institutional suppression). The peerage had no exit option that preserved veto power; they could only accept reduction or trigger institutional collapse. Suppression declined over time as the threat became historical and procedural deference developed a cultural basis. Theater ratio (0.55): Moderate. The 1911 Act initially established a substantive delay mechanism (low theater), but over the interval the two-year delay became increasingly performative as legislative speed increased and Lords prestige declined. By the 1930s, the Lords review was vigorous but ceremonial — theater ratio trending toward piton threshold. At the moment of imposition (1911), theater was lower; at the analytical civilizational view, theater is higher.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental gap between institutional positions. The peerage perceives snare (structural coercion, no exit, maximal extraction). The Commons perceives rope (genuine legislative coordination). The Conservative party perceives tangled rope (loss of institutional leverage + tactical coordination value). The reform coalition perceives scaffold (temporary settlement pending further democratization). The civilizational Lords institution perceives piton (ceremonial status without substance). The analytical observer risks perceiving mountain (natural law of supremacy) but the structural data reveals false summit: the beneficiaries (electoral Commons, democratic constituencies) and specific extraction mechanism (hereditary veto suppression under peer-creation threat) show this is contingent institutional arrangement, not immutable law. The constraint's six-type span reveals that 'parliamentary supremacy' is not a unified concept — it is a family of institutional arrangements with different extractiveness, suppression, and theater profiles depending on which parties and time horizons you measure from.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from the structural position of each agent. The peerage as victims with trapped exit (no way to preserve veto power) derives high d (0.95) → high f(d) (1.42) → maximum experienced extraction. The Commons as beneficiaries with arbitrage exit (can enforce outcome through peer-creation threat) derives low d (0.15) → low f(d) (-0.01) → negative effective extraction. The Conservative party as organized actors with constrained exit (can negotiate within new framework but cannot restore veto) derives moderate-high d (0.55) → moderate f(d) (0.75) → moderate extraction. The reform coalition as organized actors with constrained exit but sunset framing derives lower d (0.45) → f(d) (0.40) → lower extraction. The analytical observer's d (0.73) reflects the external position and universal scope, producing moderate f(d) (1.15). The peerage's trapped exit option is the decisive structural fact: they face a choice between acceptance and institutional collapse, with no third option. This creates maximum directionality differential and maximum perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through the kernel reading frame. The 1911 Act is ONE READING of parliamentary supremacy — a reading that establishes electoral Commons control through suppression of hereditary veto. Other readings (Acts of Union, Fixed-term Parliaments, Parliament Act 1949) instantiate different constraint settlements around the same kernel. The mandatrophy question — 'Is this constraint coordination (Rope) or extraction (Snare) or something mixed (Tangled Rope)?' — is answered by recognizing that the kernel admits multiple readings with different extractiveness profiles. The 1911 reading extracts the hereditary veto (Tangled Rope from analytical view) while establishing legitimate democratic coordination (Rope from Commons view). Both are true because the reading is a power settlement that redistributes institutional authority. The apparent paradox (coordination + extraction) is the signature of Tangled Rope: genuine legislative coordination function (the revising chamber has real utility) layered with asymmetric extraction (the hereditary veto was suppressed under duress). The mandatrophy does not resolve to a single type; it resolves to a reading that legitimates power redistribution as democratic law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mass_peer_creation_credibility,
    'Was the threat of mass peer creation a credible commitment by the Crown and Liberal government, or a bluff that the peerage could have called?',
    'Historical examination of Cabinet deliberations, Crown legal opinions, and peerage private correspondence. Counterfactual analysis: what would have happened if the Lords had refused to yield in 1911-1912?',
    'If threat was credible: peerage faced genuine snare (no viable exit). If threat was bluff: peerage had constrained exit (could have negotiated better terms or forced full chamber reform). Shifts classification from snare to tangled_rope for hereditary victim perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mass_peer_creation_credibility, empirical, 'Whether mass peer creation was a credible structural threat or negotiating posture').

omega_variable(
    beneficiary_vs_legitimacy_reading,
    'Does the extractiveness derive from suppression of hereditary veto (institutional asymmetry reading) or from enforcement of democratic supremacy (legitimacy reading)?',
    'Examine constitutional discourse: did contemporaries frame 1911 as ''curbing an obstacle'' (extraction frame) or ''establishing proper representation'' (legitimacy frame)? Measure via parliamentary debate, constitutional doctrine, and public framing by each party.',
    'If extraction frame dominates: constraint is tangled_rope with clear asymmetry (Commons + beneficiaries vs. peerage victims). If legitimacy frame dominates: constraint reads as establishment of proper constitutional order (Rope, not Tangled Rope). Shifts theater ratio down if legitimacy frame becomes hegemonic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_vs_legitimacy_reading, conceptual, 'Whether 1911 Act is framed as extraction or as establishment of democratic legitimacy').

omega_variable(
    hereditary_veto_function_ambiguity,
    'Did the hereditary veto function as a genuine institutional check on Commons overreach, or was it primarily a tool for partisan Conservative blockade of progressive legislation?',
    'Analysis of Lords rejection patterns 1900-1911: frequency of partisan vs. principled refusals; comparison with post-1911 Lords behavior (revising chamber with no money-bill veto); measure change in Lords workload and legislative effectiveness.',
    'If check function was genuine: extractiveness of veto suppression is higher (legitimate institutional function was removed). If blockade function dominated: extractiveness is lower (removed an asymmetric partisan tool). Affects whether suppression (0.52) should be revised upward or downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hereditary_veto_function_ambiguity, empirical, 'Whether hereditary veto functioned as principled check or partisan blockade mechanism').

omega_variable(
    reading_kernel_interpretation_gap,
    'Is the Parliament Act 1911 a reading of one constitutional kernel (parliamentary supremacy as natural law) or does it resolve the kernel into separate contingent statutes (supremacy as enforced power settlement)?',
    'Examine whether subsequent constitutional doctrine treats the 1911 Act as discovery of immutable parliamentary law or as contingent institutional choice. Does the 1949 amendment (further shortening Lords delay) evidence that the 1911 settlement was itself contingent and revisable? Does the repeal of Fixed-term Parliaments (2022) confirm that ''constitutional'' rules are actually revisable statutes?',
    'If treated as natural law: mountain perspective is correct and false summit signature should not fire. If treated as contingent statute: false summit fires and reform coalition perspective (scaffold) becomes the analytical baseline. Determines whether the constraint is a genuine natural law or a power settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_interpretation_gap, conceptual, 'Whether 1911 Act is constitutional natural law or contingent statutory power settlement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliamentary_supremacy_statutes__parliament_act_1911, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pa1911_tr_t0, parliamentary_supremacy_statutes__parliament_act_1911, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pa1911_tr_t5, parliamentary_supremacy_statutes__parliament_act_1911, theater_ratio, 5, 0.55).
narrative_ontology:measurement(pa1911_tr_t10, parliamentary_supremacy_statutes__parliament_act_1911, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pa1911_be_t0, parliamentary_supremacy_statutes__parliament_act_1911, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pa1911_be_t2, parliamentary_supremacy_statutes__parliament_act_1911, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(pa1911_be_t5, parliamentary_supremacy_statutes__parliament_act_1911, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(pa1911_be_t10, parliamentary_supremacy_statutes__parliament_act_1911, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(pa1911_su_t0, parliamentary_supremacy_statutes__parliament_act_1911, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(pa1911_su_t2, parliamentary_supremacy_statutes__parliament_act_1911, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(pa1911_su_t10, parliamentary_supremacy_statutes__parliament_act_1911, suppression_requirement, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliamentary_supremacy_statutes__parliament_act_1911, enforcement_mechanism).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__parliament_act_1911, acts_of_union).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__parliament_act_1911, fixed_term_parliaments_act).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__parliament_act_1911, parliament_act_1949).

% DUAL FORMULATION NOTE:
% The Parliament Act 1911 is the centerpiece reading of the parliamentary_supremacy_statutes kernel family. It directly influences the 1949 amendment (which used the 1911 procedure to shorten delay) and coexists with the Acts of Union (both statute-based resets of authority, but at different historical moments). The constraint family models how the same kernel (how is legislative authority distributed?) admits multiple readings across time, each with its own extractiveness profile and institutional beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parliamentary_supremacy_statutes__parliament_act_1911, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
