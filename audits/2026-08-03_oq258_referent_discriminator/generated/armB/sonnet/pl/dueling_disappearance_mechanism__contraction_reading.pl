% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Substrate as Irreversible Replacement for Honor-Culture Axioms (Contraction Reading)
 *   domain: historical sociology / cultural anthropology / legal history
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   dueling-disappearance kernel: dueling did not merely lose a legal or
 *   institutional competition (that is the
 *   institutional_displacement_reading, a sibling constraint) but became
 *   literally unthinkable as an axiom-substrate shifted beneath the practice.
 *   Honor culture's foundational premise — that reputation is an external,
 *   forfeitable, defensible possession requiring mortal risk to protect — was
 *   displaced by dignity culture's premise that worth is inherent and
 *   non-forfeitable. Once dignity-culture axioms saturate a population,
 *   dueling does not look like a bad option among several; it looks like a
 *   category error, akin to challenging someone to combat over an insult to
 *   one's height. This reading treats the substrate shift itself as the
 *   operative mechanism and models it, once established, as effectively
 *   irreversible at the population level — hence claimed_type mountain rather
 *   than rope. The rope framing (dueling as a coordination solution to status
 *   disputes) is not wrong on its own terms; it describes the FUNCTION
 *   dueling served under the prior axiom set. This reading asserts the
 *   function's own presupposition — that reputation is externally defensible
 *   — became unavailable, not merely regulated away.
 *
 * KEY AGENTS:
 *   - dignity_culture_institutions: primary structural beneficiary (institutional/arbitrage) — constitutes the substrate that renders dueling illegible
 *   - professional_middle_class: secondary beneficiary (organized/mobile) — status advancement tied to dignity-culture respectability norms
 *   - state_monopoly_on_violence: institutional beneficiary riding the substrate shift (institutional/analytical) — legitimacy reinforced but not the causal engine in this reading
 *   - honor_culture_practitioners: primary victim (powerless/trapped) — the framework that gave their lives meaning became unspeakable
 *   - cultural_historians: analytical observer — adjudicates between this reading and its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.28).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.52).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Substrate as Irreversible Replacement for Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical sociology / cultural anthropology / legal history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'ebc487cb-a1a9-4dc9-952f-347a22b3ef6d').
narrative_ontology:cs_kernel_codification('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', distributed).
narrative_ontology:cs_authority_grounding('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', distributed).
narrative_ontology:cs_reading_relation('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', foundational, axiom_substrate_is_causally_sufficient).
narrative_ontology:cs_axiom_status(axiom_substrate_is_causally_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', axiom_substrate_is_causally_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', secondary, dignity_axiom_displacement_is_irreversible).
narrative_ontology:cs_axiom_status(dignity_axiom_displacement_is_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', dignity_axiom_displacement_is_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', honor_culture_externalized_reputation_regime).
narrative_ontology:cs_drift_state('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', post_bellum_dignity_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ebc487cb-a1a9-4dc9-952f-347a22b3ef6d', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, professional_middle_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_violence).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_codes_of_conduct).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, individual_dignity_as_inherent_status).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, state_adjudication_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schools, courts, churches, and professional associations that teach and enforce the new axiom — status derives from inherent worth, not from willingness to risk death over insult. They do not administer dueling's suppression directly; they simply constitute the cultural substrate in which dueling stops making sense as an intelligible act.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Rising bourgeois and professional strata whose social advancement depended on emotional self-restraint, credentialed merit, and non-violent reputation management. They gain status precisely because the honor code that required physical risk to defend reputation is no longer the operative grammar of respectability.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, professional_middle_class, beneficiary,
    organized, generational, mobile, national).

% The state's claim to sole legitimate arbiter of violence is reinforced when private lethal combat over reputation becomes not merely illegal but unthinkable — a category error rather than a live option. Benefits from the axiom shift but does not create it; it rides the substrate change.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_violence, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_violence, agenda_setter).

% Aristocrats, officers, and gentlemen whose entire framework for masculine self-respect required the willingness to answer insult with mortal risk. As dignity-culture axioms saturate the culture, their code becomes not merely disapproved of but structurally illegible — younger generations cannot even parse why an insult would warrant death. They cannot exit into a framework that no longer exists to receive them; the ground they stood on is gone, not merely regulated.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, trapped, regional).

% The honor code itself as an institution — its vocabulary, its rituals (challenge, seconds, satisfaction), its internal logic of graduated response — has no vehicle for persistence once the dignity-culture substrate no longer recognizes its terms as meaningful. Listed for completeness; not an actor, but a framework that loses all purchase.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_codes_of_conduct, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(dueling_disappearance_mechanism__contraction_reading, displaced_aristocratic_codes_of_conduct).

% Reconstruct the axiom shift from correspondence, dueling manuals, sermons, and legal commentary. Debate whether the substrate change was itself irreversible or whether it was one causal strand among several — the contest this story is one reading of.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under honor culture, dueling coordinated status disputes among social equals by providing a graduated, rule-bound ritual for restoring reputation without escalating to unlimited private war. Under dignity culture, the coordination function migrates entirely: status is presumed inherent and non-forfeitable, so there is nothing left for a duel to restore.
% TRANSFER_FUNCTION: Nothing is transferred in the ordinary extractive sense in this reading — the substrate shift moves cultural intelligibility itself away from honor-code practitioners and toward dignity-code populations. What honor-culture practitioners lose is not property but the very framework within which their claims could register as meaningful.
% ABSENT_VOICES: Honor-culture practitioners themselves are the primary absent voice in the historical record's dominant framing — dignity-culture institutions wrote the histories, so the phenomenology of losing an entire moral vocabulary (rather than merely a legal permission) is underrepresented. Their descendants inherited dignity-culture's judgment of the practice as barbaric without inheriting a fair hearing of its internal logic.
% DISAPPEARANCE_RATIONALE: If dignity-culture axioms were to retract (a counterfactual, since this reading holds the substrate as irreversible), honor-culture practices could in principle become intelligible again — status-defense-by-combat would re-enter the space of thinkable action. Since the reading treats the substrate shift as a completed, irreversible cultural mountain, its removal is not a live possibility, but the stakeholder structure built atop it (professional-class respectability norms, state violence monopoly legitimacy) would visibly depend on it if it somehow lifted.
% FOUNDING_PROBLEM: Honor culture's problem was regulating reputation-based violence among status-equals without either endless private war or total submission to a weak central authority; dignity culture emerged to solve status-security through non-violent, universalizable, non-forfeitable personhood claims compatible with centralized law and market society.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the dignity-culture institutions themselves (e.g., scholarship tracing the shift through non-partisan court records, insurance-industry archives, and comparative anthropology of honor societies still extant elsewhere) corroborate that the specific problem honor codes solved — reputation defense among armed equals absent reliable third-party enforcement — had been substantially resolved by state judicial capacity independent of any dignity-axiom argument, which is precisely the tension this reading contests against the institutional_displacement_reading.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because this reading does not model the substrate shift as a rent-extraction mechanism — no party actively profits from suppressing honor-culture practitioners in the way a snare's beneficiary profits from a trapped victim. What honor-culture practitioners lose is intelligibility, not wealth or labor. Suppression is authored moderate (0.52) and rising over the interval because as dignity-culture axioms consolidate, honor-culture practice becomes actively unspeakable in polite society, in law, and eventually in imagination — this is a real suppressive dynamic even though no single administrator wields it. Theater ratio stays low throughout (0.15) because there is minimal performative maintenance; the substrate shift, once it takes, requires little ongoing theatrical upkeep — its grip is precisely that it no longer needs defending. Accessibility collapse is authored very high (0.88), consistent with the mountain claim: once dignity culture is internalized, the honor-culture alternative is not merely disfavored but nearly unrecoverable as a lived option. Resistance is authored low (0.12) — by the time the substrate has shifted, there is little organized resistance left to shift it back, which is itself part of what makes this reading claim mountain rather than rope or tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity-culture institutions and the professional middle class are beneficiaries because the axiom shift is the very ground of their rising status claims — they did not need to fight for the shift to occur; it constituted them. Honor-culture practitioners are victims not because anyone extracts from them in an ongoing transactional sense, but because the framework within which their central social technology operated ceased to have listeners. Their exit option is trapped rather than merely constrained: there is no adjacent framework to flee into, because the substrate itself moved. The state's directionality sits near beneficiary but is marked secondary (agenda_setter) because it enforces bans concurrently with the axiom shift without being the shift's origin in this reading — that causal precedence belongs to the sibling institutional_displacement_reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific direction: it would be a mistake to treat the vanishing of dueling as evidence that its coordination function (managing status disputes among near-equals without reliable courts) was merely retired because a better tool arrived — that is the institutional story. This reading insists the underlying PROBLEM the honor code solved became moot not because a substitute solved it better but because the terms in which the problem was posed stopped being intelligible. The founding_problem_status is authored dead, but the corroboration is explicitly framed as contested against the sibling readings — a genuine case where the genealogy question does not resolve cleanly to one causal story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_shift_vs_institutional_substitution,
    'Did honor-culture practitioners abandon dueling because the axiom substrate beneath their framework genuinely collapsed (this reading), or because courts, insurance, and credit-reporting institutions out-competed dueling as a dispute-resolution mechanism while the underlying honor axioms persisted longer than surface behavior suggests (the institutional_displacement_reading)?',
    'Close reading of private correspondence and diaries from the transitional generations (roughly 1830s-1880s in the US and UK cases) for evidence of genuine axiom abandonment (statements that insult no longer feels like a threat to selfhood) versus strategic abandonment (statements that dueling is imprudent or illegal but honor logic remains privately intact).',
    'If correspondence shows honor logic persisting privately while dueling behavior stops, this reading is substantially weakened in favor of institutional_displacement_reading — the axiom did not contract, it was merely out-competed operationally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_shift_vs_institutional_substitution, empirical, 'Whether decline reflects genuine axiom collapse or strategic behavioral substitution under persisting honor logic.').

omega_variable(
    reversibility_of_substrate_shift,
    'Is the dignity-culture substrate genuinely irreversible at the population level (justifying the mountain classification), or could a sufficiently severe institutional legitimacy collapse (e.g., total breakdown of courts and policing) cause honor-culture axioms to re-emerge, indicating the substrate shift is itself contingent on the persistence of the very institutions the sibling reading credits?',
    'Comparative study of societies experiencing state collapse to observe whether honor-culture-style private violence codes re-emerge, and whether such re-emergence tracks institutional failure (supporting institutional_displacement_reading) or remains absent even amid institutional collapse (supporting genuine axiom irreversibility).',
    'Evidence of honor-culture re-emergence under state collapse would suggest the dignity-culture substrate is itself institutionally scaffolded rather than a free-standing mountain, collapsing this reading''s core claim of irreversibility and favoring institutional_displacement_reading or overdetermined_composite_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_substrate_shift, conceptual, 'Whether dignity-culture''s apparent irreversibility is a genuine substrate fact or dependent on persisting institutions this reading treats as secondary.').

omega_variable(
    kernel_disagreement_locus,
    'The three sibling readings of the dueling-disappearance kernel disagree specifically about causal ARCHITECTURE, not about the observed outcome (dueling declined). Where exactly is the disagreement located: is it about which single factor was causally sufficient (this reading vs. institutional_displacement_reading), or about whether a single-factor causal story is even the right frame (vs. overdetermined_composite_reading)?',
    'This is a conceptual/framing disagreement not fully resolvable by additional data — it depends on the historiographical commitment to monocausal versus multicausal explanation, which is itself a live methodological dispute in social history.',
    'If the composite reading is adopted as the correct frame, then this reading (and institutional_displacement_reading) would each need to be understood as documenting real but PARTIAL and jointly-necessary mechanisms rather than independently sufficient ones — changing how much explanatory weight either reading alone can bear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Locates the kernel disagreement as a dispute over causal architecture (monocausal vs. multicausal framing), not over the historical outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(duel_tr_t40, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(duel_tr_t80, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(duel_tr_t100, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(duel_be_t40, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(duel_be_t80, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(duel_be_t100, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(duel_su_t20, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(duel_su_t40, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(duel_su_t80, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 80, 0.51).
narrative_ontology:measurement(duel_su_t100, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language claim 'dueling became culturally unthinkable' per the ε-invariance principle. contraction_reading claims mountain (irreversible axiom substrate, low-moderate ε, victims are honor-culture practitioners who lost a framework); institutional_displacement_reading (sibling, not authored here) is expected to claim rope or tangled_rope (functional substitution by competing institutions, moderate ε, different victim/beneficiary structure centered on institutional competition); overdetermined_composite_reading (sibling, not authored here) treats no single mechanism as sufficient and is expected to resist clean type assignment, modeling joint necessity. All three share the same historical outcome (dueling's decline) but assert incompatible causal architectures, hence different structural facts and different ε values — they are linked here as a constraint family rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
