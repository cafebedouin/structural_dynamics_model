% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE (contested kernel reading; historical instantiations formally repudiated)]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Fitness-Contingent Personhood Boundary (Certified-Capacity Reading)
 *   domain: moral philosophy/historical ethics/commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the personhood_boundary kernel:
 *   the fitness_contingent_reading, under which moral standing is conferred
 *   only upon demonstrated fitness certified by authorized assessors, and
 *   entities short of the line — paradigmatically newborns and infants with
 *   severe impairments — fall outside the moral community altogether. The
 *   standing arrangement under contest is the fitness-contingent regime
 *   itself (historically: hereditary health courts, mandatory defect
 *   registries, the children's specialty departments and adult transfer
 *   program of 1939-1945); epsilon is authored for THAT arrangement as this
 *   reading's own lights frame it. The reading's definitional exclusion of
 *   its targets is not a discount on extraction but its enabling mechanism:
 *   nothing protects what the doctrine defines as unprotected, which is why
 *   the measured extraction is total rather than partial. The colloquial
 *   label 'where personhood begins' decomposes into three structurally
 *   distinct constraints per the epsilon-invariance principle: this reading
 *   (victims = all pre-fitness entities; state authority over the line), the
 *   birth_threshold_reading (no victims among the born; the line is an event,
 *   not an assessment), and the potential_based_reading (a narrow contested
 *   margin only). Their epsilon values differ widely; they are separate
 *   stories linked by network edges, not one story with a measurement
 *   parameter. KEY AGENTS (by structural relationship):
 *   state_eugenic_authorities — agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — writes and enforces the criterion;
 *   fitness_assessment_professionals — administering beneficiary
 *   (institutional/identity_locked) — operates the sorting machinery;
 *   fit_citizenry — diffuse beneficiary (organized/constrained);
 *   severely_disabled_infants and institutionalized_psychiatric_patients —
 *   primary targets (powerless/trapped) — bear the arrangement's full cost;
 *   parents_of_targeted_children — dual-positioned (moderate/constrained) —
 *   bear losses while absorbing incidental relief; dissenting_clergy_and_kin
 *   — excluded objectors (organized/constrained); postwar_moral_philosophers
 *   — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.93).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.55).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood Boundary (Certified-Capacity Reading)").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral philosophy/historical ethics/commitment systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '422ee8f0-ea4a-4221-a47f-885c1c4ba91f').
narrative_ontology:cs_kernel_codification('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', formalized).
narrative_ontology:cs_authority_grounding('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', expertise).
narrative_ontology:cs_interpretation_layer_present('422ee8f0-ea4a-4221-a47f-885c1c4ba91f').
narrative_ontology:cs_reading_relation('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', foundational, moral_standing_requires_demonstrated_fitness).
narrative_ontology:cs_axiom_status(moral_standing_requires_demonstrated_fitness, holdable).
narrative_ontology:cs_axiom_grounding('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', moral_standing_requires_demonstrated_fitness, empirically_contingent).
narrative_ontology:cs_axiom('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', secondary, state_guardianship_over_prefitness_life).
narrative_ontology:cs_axiom_status(state_guardianship_over_prefitness_life, overridden).
narrative_ontology:cs_axiom_grounding('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', state_guardianship_over_prefitness_life, conventional).
narrative_ontology:cs_reference_frame('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', expert_certified_fitness_threshold).
narrative_ontology:cs_drift_state('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', post_nuremberg_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('422ee8f0-ea4a-4221-a47f-885c1c4ba91f', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_eugenic_authorities).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fitness_assessment_professionals).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fit_citizenry).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, institutionalized_psychiatric_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, parents_of_targeted_children).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, parents_of_targeted_children).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, racial_hygiene_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, life_unworthy_of_life_principle).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, hereditarian_determinism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts the fitness statutes, appoints the assessing bodies, and owns the registries of classified individuals. Decides which diagnoses disqualify, funds the special departments, and receives reports on throughput. When public protest made the adult program politically costly, it issued the formal stop order while continuing the operation through decentralized channels. Nothing in the arrangement binds it: it writes the criterion it enforces.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_eugenic_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Physicians and administrators who complete the classification questionnaires, sit on review panels, sign transfer orders, and certify deaths. Advancement and institutional standing flow to cooperative participants; refusal carried dismissal, and under wartime conditions worse. The profession's self-conception as arbiter of medical worthiness fused with the program's function, so leaving the role meant abandoning professional identity itself.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fitness_assessment_professionals, beneficiary,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, fitness_assessment_professionals, agenda_setter).

% Receives hospital beds, food allocations, and public assurances of national vigor freed of hereditary burden. Benefits indirectly from redirected care resources and directly from the status order the fitness line maintains. Consent is manufactured through film and press campaigns; dissent carries social cost, so participation is passive but real.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fit_citizenry, beneficiary,
    organized, biographical, constrained, national).

% Registered at diagnosis through mandatory reporting, examined by commissioned reviewers, and transferred to special pediatric departments where treatment is withheld or lethal doses administered. Has no voice, no advocate of record, no process beyond the paperwork that classifies it, and no exit; survival depends entirely on a parent willing to conceal.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Adults in long-stay custodial care, sorted by mailed questionnaires reviewed in minutes, transported to killing centers under the guise of routine transfer, and reported dead of invented causes to their families. Confined before the program and unreachable during it; the institution that was their shelter became the conduit.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, institutionalized_psychiatric_patients, payer,
    powerless, biographical, trapped, national).

% Pressured to sign admission forms described as therapeutic placement and told no treatment exists elsewhere. Some conceal their children at home for the duration at constant risk of denunciation; others comply, relieved of an unbearable caregiving burden and quietly approved of for it. Bears the loss and shares, unevenly and involuntarily, in the arrangement's relief.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, parents_of_targeted_children, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, parents_of_targeted_children, beneficiary).

% Bishops preaching against the killings from the pulpit, relatives demanding return of remains and asking questions the death certificates cannot answer. Entirely outside the program's proceedings — the classification machinery affords them no standing to object — they act only through public address and private inquiry, and their intervention forced the one formal pause in the adult program.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, dissenting_clergy_and_kin, excluded,
    organized, biographical, constrained, national).

% Adjudicate the doctrine in retrospect from trial transcripts, surviving ledgers, and the genetics that undid hereditarian premises. Neither collected nor paid under the arrangement; they assess whether any fitness criterion can be drawn that does not expand with administrative convenience.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, postwar_moral_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, state_eugenic_authorities).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared criterion for membership in the moral and legal community — demonstrated fitness certified by authorized assessors — and a procedure for allocating care resources, legal protection, and social recognition according to that criterion. It also coordinates the medical profession around a unified evaluative role and gives the administrative state a single registry-backed answer to who counts.
% TRANSFER_FUNCTION: Moves moral standing, legal protection, and care resources away from entities classified as pre-fitness and toward the fit population and the assessing apparatus; moves decision authority over life and death from families and individual conscience to state-appointed boards.
% ABSENT_VOICES: The classified themselves — structurally silenced, since the doctrine's first move is to define them outside the conversation before it begins. Also dissenting clergy, disability advocates, and coerced parents, who held no standing within the assessment proceedings; their objections counted only when voiced from outside the machinery, which is why the one effective protest came from a pulpit rather than a review panel.
% DISAPPEARANCE_RATIONALE: If the fitness boundary vanished overnight, registered children would revert to their families' protection, transfers to killing centers would stop, the assessing professions would lose their delegated sovereignty over the line, and the fit majority's resource claims would have to compete openly. The moral community would re-expand to include all born humans — which is in fact what happened de facto after 1945, demonstrating that the surrounding arrangements depended on the boundary rather than the reverse.
% FOUNDING_PROBLEM: Built to solve the perceived burden of unproductive lives on a war economy and the eugenic fear of national degeneration — framed by its architects as a crisis requiring the community to identify and shed 'life unworthy of life' through medically certified judgment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set by the Nuremberg Doctors' Trial record, postwar denazification testimony, surviving relatives' accounts, and contemporaneous clerical protest — all of which attest that the 'burden' framing was constructed by the program's own administrators to legitimate elimination, and that the underlying care-allocation pressures were manageable by ordinary means without any revision of the standing line. No source outside the benefiting parties attests the founding problem as the architects stated it; that absence is itself the finding.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.93, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.93 (interval end) because the arrangement takes the maximal good — continued life — from everyone below the line, with no process, compensation, or appeal; the reading's own lights supply the reason the extraction is unlimited rather than bounded. Suppression is authored at 0.55 as the end-state value matching the enforcement-collapse terminus of the series, which peaks at 0.82 during the program's coercive height (mandatory registration, compelled parental consent, denunciation, police enforcement against concealing families); suppression here is overwhelmingly structural (law, registry, terror) with a secondary internalized component (parents induced to read surrender as medical treatment), noted rather than separately scored. Theater rises with atrocity (0.35 to 0.66) — euphemistic transport names, forged certificates, condolence letters, minutes-long questionnaire 'reviews' performed as clinical judgment — then declines as the regime burns files and abandons justification. Accessibility_collapse is 0.70: once the machinery is understood, exits largely close for targets and refusers alike, though concealment and resignation remained possible at severe cost, so not the near-total collapse of a natural law. Resistance is 0.45: real and occasionally effective (the 1941 clerical protest forcing a formal halt; the Dutch physicians' strike stopping extension outright) but dangerous, late, and unable to reach the child program. All three tracked series run on one shared seven-point grid (T0=1933 codification through T24=1945 collapse) so no metric is sampled against another metric's end-state. Receipt surface: gains demonstrably accrue to the state_eugenic_authorities seat — the decisive captured asset was sovereign jurisdiction over the line itself, plus confiscated estates and consolidated institutional budgets — so gain_flow names that seat rather than asserting diffuseness. Fixing cost is 'cheap': the formal halt required a single decree, and external dissolution took days in 1945; what bound the arrangement was will, not cost. Coalition check: the victims themselves cannot coalition — their silence is constitutive of the classification — so the operative coalition question sits with third parties and is routed to the third_party_coalition_potential omega. The trajectory is monotone intensification followed by terminal collapse, not cyclical; no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from the same structure. From the agenda-setter seat the arrangement is prudent stewardship: a community curating its own moral membership against demonstrable burden, administered by credentialed judgment. From the target seats it is annihilation without process — the same questionnaire that reads as clinical diligence from above reads as a death warrant from below, reviewed in minutes. The parents' seat splits the difference structurally: grief and relief in the same household, which is why their directionality derives mid-scale rather than at either pole. The fitness_assessment_professionals seat carries the sharpest identity-lock: the fusion of medical self-conception ('healing') with selection function made exit indistinguishable from professional death, so the seat persisted past the point where its members' private doubts were well documented. The engine computes this divergence from power, exit, and role data; the authored snare claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State_eugenic_authorities sit nearest the beneficiary pole (d near 0.0): they subsidize themselves from the arrangement, define the test, and hold arbitrage-grade exit from any criterion they dislike. Fitness_assessment_professionals derive low-to-moderate d as beneficiaries, tempered by their identity_locked exit — they collect standing and income but cannot leave. Fit_citizenry derive low d: genuine diffuse gains in resources and status, at the price of manufactured consent. The two victim groups derive near-full-target d (d near 1.0): trapped, powerless, bearing the arrangement's entire cost. Parents_of_targeted_children derive intermediate d from their dual declaration — primary bearers of loss with involuntary secondary benefit. The distinctive structural fact this reading contributes to the derivation chain is definitional laundering: the doctrine attempts to move its targets off the target end of the scale by declaring them outside the class of beings toward whom directionality can run. The structural data refuses the move — the declared victims remain declared — and the laundering attempt itself is recorded as the definitional_laundering omega rather than honored in the arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Against mislabeling-as-coordination: the arrangement's coordination story (clear membership criteria, humane resource stewardship, expert administration) is exactly the cover a pure-extraction structure would wear, and the gate data — named victims, active enforcement, concentrated capture, suppressed exits — must be allowed to convict it rather than letting the coordination narrative launder the verdict. Against mislabeling-as-piton: this is not an atrophied shell maintained theatrically after its function died; while it held, it did exactly what it said, and it ended by defeat and repudiation, not by mandate decay — so no mandatrophy_resolved flag is authored and no theater-driven downgrade is invited. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): no dead-mandate zombie flag fires, correctly — the founding problem's status is disputed between the postwar repudiation consensus and persistent academic revivals, and the world demonstrably rearranged when the boundary fell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_laundering,
    'Does the reading''s own denial of standing to the classified mean the measured extraction fails to count as extraction — or is definitional exclusion precisely the mechanism by which the extraction is rendered total and invisible to its beneficiaries?',
    'Cross-reading comparison: generate the same historical arrangement under the birth_threshold_reading and observe the resulting victim set and epsilon; convergence on high extraction across readings with disjoint victim sets establishes the laundering as structural rather than definitional.',
    'If the laundering reading holds, the constraint computes as pure extraction despite the reading''s internal denial that victims exist; if not, the story collapses toward a benign boundary rule and the snare claim fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_laundering, conceptual, 'Whether the reading''s victim-denial launders real extraction or genuinely dissolves it.').

omega_variable(
    fitness_criterion_discretion,
    'Is ''demonstrated fitness'' a determinate criterion, or administrative discretion that expands with political need?',
    'The historical category-expansion record: registries beginning with hereditary illness extended within six years to the institutionalized, the elderly, displaced urban children, and ''maladjusted'' youth, with criteria set by circular rather than statute.',
    'Discretionary expansion converts a boundary rule into open-ended elimination authority and fixes the snare classification; a genuinely bounded criterion would support at most a hybrid coordination reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fitness_criterion_discretion, empirical, 'Determinacy of the fitness criterion under administrative pressure.').

omega_variable(
    kernel_reading_delta,
    'How would the sibling readings of the personhood_boundary kernel restructure this constraint''s victim set and epsilon?',
    'Generate the sibling stories: the birth_threshold_reading empties the victim set of all born humans and drops extraction toward coordination-cost levels; the potential_based_reading narrows victims to a contested margin of severe impairment.',
    'Locates the disagreement in the criterion for standing (event versus demonstration versus potential) rather than in any measurable property of the affected entities; the engine''s foreclosure computation should fire on the contradicting necessity/sufficiency pair between this reading and each sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer structure: this story is one reading of the personhood_boundary kernel; siblings change the victim set wholesale.').

omega_variable(
    third_party_coalition_potential,
    'The victims cannot form coalitions — silence is constitutive of their classification — but refusers, clergy, and kin temporarily forced a formal halt in 1941. How much durable coalition capacity do third parties carry, and why did it not hold?',
    'Cross-jurisdiction comparison: the Dutch physicians'' collective refusal stopped extension of the program outright, while the German clerical protest bought only a suspension that continued covertly — the difference tracks enforcement density and the exit options of the refusers.',
    'High durable third-party coalition capacity would push the classification toward contested persistence rather than stable extraction; its absence explains why coercive enforcement, not participant preference, carried the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_coalition_potential, empirical, 'Whether refuser coalitions could have held against the enforcement apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t4, personhood_boundary__fitness_contingent_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement_basis(pers_tr_t4, observed).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__fitness_contingent_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement_basis(pers_tr_t8, observed).
narrative_ontology:measurement(pers_tr_t12, personhood_boundary__fitness_contingent_reading, theater_ratio, 12, 0.63).
narrative_ontology:measurement_basis(pers_tr_t12, observed).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__fitness_contingent_reading, theater_ratio, 16, 0.66).
narrative_ontology:measurement_basis(pers_tr_t16, observed).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__fitness_contingent_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement_basis(pers_tr_t20, observed).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__fitness_contingent_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement_basis(pers_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t4, personhood_boundary__fitness_contingent_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(pers_be_t4, observed).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__fitness_contingent_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement_basis(pers_be_t8, observed).
narrative_ontology:measurement(pers_be_t12, personhood_boundary__fitness_contingent_reading, base_extractiveness, 12, 0.84).
narrative_ontology:measurement_basis(pers_be_t12, observed).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__fitness_contingent_reading, base_extractiveness, 16, 0.9).
narrative_ontology:measurement_basis(pers_be_t16, observed).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__fitness_contingent_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement_basis(pers_be_t20, observed).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__fitness_contingent_reading, base_extractiveness, 24, 0.93).
narrative_ontology:measurement_basis(pers_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t4, personhood_boundary__fitness_contingent_reading, suppression_requirement, 4, 0.64).
narrative_ontology:measurement_basis(pers_su_t4, observed).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__fitness_contingent_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement_basis(pers_su_t8, observed).
narrative_ontology:measurement(pers_su_t12, personhood_boundary__fitness_contingent_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement_basis(pers_su_t12, observed).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__fitness_contingent_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement_basis(pers_su_t16, observed).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__fitness_contingent_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(pers_su_t20, observed).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__fitness_contingent_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(pers_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'where personhood begins' decomposes into three structurally distinct constraints with disjoint victim sets and widely separated epsilon values. This story (fitness_contingent_reading) is the maximally extractive member: victims = all pre-fitness entities, state authority over the line, historical instantiations 1933-1945. The birth_threshold_reading is the upstream, high-confidence member (the standing modern arrangement; no victims among the born; extraction near coordination cost). The potential_based_reading is downstream and contested (narrow margin victims; live neonatal-treatment disputes). Influence runs from the established birth-threshold arrangement outward: its settledness is what forces the other two readings to argue from criterion-first premises rather than practice. Each family member links the others via affects_constraints; no member hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
