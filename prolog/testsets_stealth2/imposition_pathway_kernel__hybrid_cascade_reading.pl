% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Meiji Decree-Manufactured Fringe Cascade (Hybrid Imposition Pathway)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   In 1872 the Meiji cabinet began issuing decrees binding the compulsory
 *   sector of the new state — ministry officials, army and navy personnel,
 *   police — to the symbols and practices of the international order it
 *   sought entry to: Western dress at court and office, regulated hairstyles,
 *   the Gregorian calendar, standardized administrative forms. The decrees
 *   did not address the country at large; they addressed a few hundred
 *   thousand salaried personnel whose livelihoods depended on compliance.
 *   That compulsory bloc became a manufactured fringe: a visible,
 *   disciplined, state-paid adopter population stationed in every prefecture,
 *   whose daily practice displayed the new commitment to everyone the state
 *   touched. Over the following four decades adoption climbed outward through
 *   the ordinary channels — office-seeking, schooling, commercial fashion,
 *   marriage markets — until by the 1910s the formerly imposed practices were
 *   largely self-sustaining custom and the enforcement machinery had gone
 *   quiet. This story instantiates the hybrid cascade reading: the override
 *   initiates, the climb completes. Constraint-family note: the colloquial
 *   label 'top-down imposition' decomposes into three structurally distinct
 *   claims (see network.dual_formulation_note). This file's epsilon is
 *   authored for the hybrid reading's referent — the decree-backed
 *   compulsory-adopter arrangement across its full life — under which the
 *   arrangement is a genuine coordination device carrying real, front-loaded,
 *   self-liquidating extraction. The endogenous reading's referent contains
 *   no coerced class at all (lower epsilon, empty victim set); the exogenous
 *   reading's referent contains a permanently coerced population with no
 *   climb-phase offset (higher sustained epsilon). Same historical material,
 *   three different arrangements, three different epsilon values. Receipt
 *   surface: the arrangement's gains demonstrably accrued to the cabinet
 *   seat, so gain_flow names that seat; unwinding the arrangement after
 *   launch would have forfeited the treaty-revision timeline the entire
 *   program was sequenced against, so fixing_cost is prohibitive for the only
 *   actor who could fix it.
 *
 * KEY AGENTS:
 *   - meiji_ministerial_cabinet: agenda-setter and primary beneficiary (institutional/arbitrage) — authors the decrees, funds enforcement, collects uniformity and diplomatic credibility
 *   - mandated_civil_servants: primary target (moderate/constrained) — bears coerced adoption; their visible compliance is the manufactured fringe
 *   - imperial_military_personnel: primary target (organized/trapped) — uniform and grooming regulation enforce adoption on the body; bears the fullest identity discontinuity
 *   - dispossessed_samurai_households: resisting target (organized/trapped) — stipend conversion and status-symbol prohibition made them the arrangement's armed opposition
 *   - traditionalist_rural_households: downstream target (powerless/constrained) — bear displacement as the climb reaches them decades after the decree
 *   - treaty_powers: external beneficiary (institutional/arbitrage) — collect a conformed counterpart without paying into the arrangement
 *   - village_officeholders: climb intermediaries and incidental beneficiaries (moderate/mobile) — convert adoption into office and tax standing
 *   - rural_women_outside_decree_reach: excluded voice (powerless/trapped) — bear the climb's household costs without ever being the decree's addressed audience
 *   - historical_sociologists: analytical observer (analytical/analytical) — sees the full initiation-completion structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.36).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.26).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Meiji Decree-Manufactured Fringe Cascade (Hybrid Imposition Pathway)").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, 'd399ee44-cc2d-48d7-9976-5a41800c10a8').
narrative_ontology:cs_kernel_codification('d399ee44-cc2d-48d7-9976-5a41800c10a8', formalized).
narrative_ontology:cs_authority_grounding('d399ee44-cc2d-48d7-9976-5a41800c10a8', expertise).
narrative_ontology:cs_interpretation_layer_present('d399ee44-cc2d-48d7-9976-5a41800c10a8').
narrative_ontology:cs_reading_relation('d399ee44-cc2d-48d7-9976-5a41800c10a8', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('d399ee44-cc2d-48d7-9976-5a41800c10a8', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('d399ee44-cc2d-48d7-9976-5a41800c10a8', foundational, exogenous_initiation_is_real).
narrative_ontology:cs_axiom_status(exogenous_initiation_is_real, holdable).
narrative_ontology:cs_axiom_grounding('d399ee44-cc2d-48d7-9976-5a41800c10a8', exogenous_initiation_is_real, empirically_contingent).
narrative_ontology:cs_axiom('d399ee44-cc2d-48d7-9976-5a41800c10a8', foundational, imposed_displacement_completes_through_climb).
narrative_ontology:cs_axiom_status(imposed_displacement_completes_through_climb, holdable).
narrative_ontology:cs_axiom_grounding('d399ee44-cc2d-48d7-9976-5a41800c10a8', imposed_displacement_completes_through_climb, empirically_contingent).
narrative_ontology:cs_axiom('d399ee44-cc2d-48d7-9976-5a41800c10a8', secondary, state_manufactured_fringe_functions_as_fringe).
narrative_ontology:cs_axiom_status(state_manufactured_fringe_functions_as_fringe, holdable).
narrative_ontology:cs_axiom_grounding('d399ee44-cc2d-48d7-9976-5a41800c10a8', state_manufactured_fringe_functions_as_fringe, empirically_contingent).
narrative_ontology:cs_reference_frame('d399ee44-cc2d-48d7-9976-5a41800c10a8', override_seeded_climb_cascade).
narrative_ontology:cs_drift_state('d399ee44-cc2d-48d7-9976-5a41800c10a8', contemporary_comparative_historiography, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('d399ee44-cc2d-48d7-9976-5a41800c10a8', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, meiji_ministerial_cabinet).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, treaty_powers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, mandated_civil_servants).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, imperial_military_personnel).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, dispossessed_samurai_households).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditionalist_rural_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, mandated_civil_servants).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, imperial_military_personnel).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, village_officeholders).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, hybrid_cascade_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and promulgated the 1870s decrees binding ministry officials, military, and police to Western dress, regulated grooming, the Gregorian calendar, and standardized administrative forms; funded the inspection apparatus; collected administrative legibility, fiscal-military synchronization, and the diplomatic credibility that keyed treaty revision. Exit is effectively unlimited — it wrote the arrangement and could amend or rescind any part of it.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, meiji_ministerial_cabinet, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, meiji_ministerial_cabinet, beneficiary).

% Foreign governments whose commercial and diplomatic agreements with Japan were gated on civilized-standard legal and administrative conformity. They paid nothing into the arrangement and collected a progressively conformed counterpart, easing treaty revision negotiations from 1894 onward. Their participation is purely external; exit is unlimited.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, treaty_powers, beneficiary,
    institutional, generational, arbitrage, continental).

% Salaried officials required to adopt Western dress, cut the topknot, and keep the new calendar on pain of dismissal or demotion. Bore the identity and wardrobe costs immediately; recouped part of them later as career standing in the new order. Exit means leaving state employment — feasible for a few, ruinous for most careers built on office.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, mandated_civil_servants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, mandated_civil_servants, beneficiary).

% Conscripts and officers governed by uniform regulation, grooming standards, and calendar discipline enforced through service law. Bore the fullest bodily dimension of adoption. Exit from the obligation runs through desertion or discharge, both catastrophic; conscription cycles kept the seat continuously refilled.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, imperial_military_personnel, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, imperial_military_personnel, beneficiary).

% Former warrior-rank families whose stipends were converted to bonds and whose swords and topknots — the visible constitution of their rank — were prohibited or stigmatized. Some entered the new army and police as enforcers of what displaced them; others mounted the 1877 rebellion. Exit from the status loss did not exist; the rank itself was abolished.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, dispossessed_samurai_households, payer,
    organized, biographical, trapped, regional).

% Farming and fishing households never addressed by any decree, whose customary dress, calendars, and ritual schedules were displaced over decades as adoption climbed outward from towns and offices through schools, markets, and marriage. Landbound; exit means migrating to frontier settlement, available to some sons but not to households.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditionalist_rural_households, payer,
    powerless, generational, constrained, regional).

% Headmen, tax collectors, and school sponsors who converted visible adoption into office, tax standing, and brokerage fees — the climb's local transmission points. Adoption was for them an investment with reliable returns; non-adoption meant losing access to the state's local patronage.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, village_officeholders, beneficiary,
    moderate, biographical, mobile, local).

% Women in farming households, never the addressed audience of any decree, who absorbed the household-level costs of the shift — re-sewing wardrobes, re-timing festival labor, absorbing stigma for slower adoption — without access to the office and career returns that motivated male adoption. No channel existed through which they could have objected.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rural_women_outside_decree_reach, excluded,
    powerless, generational, trapped, local).

% Analytical seat: reconstructs the cascade from decree texts, adoption statistics, inspection records, and household inventories; sees the full initiation-completion structure and holds the competing readings of it. No stake in the arrangement's operation.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, meiji_ministerial_cabinet).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of rapid national commitment standardization: a centralizing state facing external treaty deadlines needed uniform administrative practice, timekeeping, and official presentation across a fragmented customary landscape, and mandating adoption in the compulsory sector created a single synchronized adopter bloc instead of waiting for dispersed organic convergence.
% TRANSFER_FUNCTION: Moves compliance and identity-discontinuity costs onto salaried state personnel and, downstream, rural households reached by the climb, converting them into administrative legibility, fiscal-military synchronization, and diplomatic credibility; moves careers, stipends, and local office to visible adopters.
% ABSENT_VOICES: Rural women and non-officeholding commoners — never addressed by any decree yet bearing the climb's later household costs — had no seat in the decree process; nor did custodians of the displaced customary commitments, whose ritual and calendar infrastructure was dismantled as adoption climbed. Both absences are structural: the pathway works precisely by acting first on a small compulsory sector and leaving everyone else to be reached later.
% DISAPPEARANCE_RATIONALE: Without the decree-manufactured fringe, the displacement timeline stretches by decades: administrative uniformity, calendar standardization, and treaty-revision sequencing all keyed to the compulsory-sector adoption bloc. Remove it and the 1890s diplomatic breakthroughs, the synchronized fiscal-military apparatus, and the intergenerational climb vector all rearrange around a slower, more contested diffusion path.
% FOUNDING_PROBLEM: A centralizing state under external treaty pressure faced a fragmented customary commitment landscape converging too slowly to meet diplomatic and fiscal deadlines; the arrangement was built to compress the convergence timeline by manufacturing the adopter fringe by decree.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the comparative-historical literatures on state-led sumptuary transformation (Republican Turkey's hat law, Pahlavi Iran's unveiling campaigns), which attest that the problem-class recurs wherever late-centralizing states face external-standard deadlines, and by treaty-power diplomatic correspondence pressing civilized-standard reforms on Meiji Japan. The cabinet's own decree preambles attest the original urgency but are cover-story-prone and carry no independent weight. No living member of the founding coalition exists to attest from inside.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).
:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores describe the arrangement's end-state at interval close (1912), with the shared-grid measurement series carrying the life-course arc. Extractiveness (0.36 at close) was front-loaded: the decree years concentrated identity and livelihood costs on a few hundred thousand salaried personnel who had not chosen adoption, then decayed monotonically as compliance converted into custom and enforcement became redundant — the cascade's signature is compulsion that retires itself on success. Suppression (0.26 at close) traces an enforcement build-out and retirement: inspection regimes and police enforcement peaked in the 1880s (series peak 0.74) and decayed as noncompliance stopped mattering. Theater (0.30 at close) rose slowly throughout: early enforcement was almost entirely functional, producing actual adoption, while late-period inspections increasingly performed vigilance over practices already universal. Accessibility collapse (0.55) is moderate: within the compulsory sector alternatives collapsed nearly completely — an official could not keep the topknot and the salary — while rural and household practice persisted covertly for decades. Resistance (0.55) was real and occasionally armed: the 1877 Satsuma rebellion fused stipend grievance with status-symbol resistance, and grooming edicts drew riots, but the opposition was numerically and fiscally overwhelmed. Identity-lock operated differently by seat: for officials and soldiers the lock was professional (career and commission conditional on visible adoption); for samurai households it was relational and class-based (the sword and topknot constituted rank itself), which is why the same decree produced grumbling compliance in one seat and rebellion in the other. Claim/metric independence: tangled_rope is claimed on structural grounds — a real coordination function, an identifiable paying class, and enforcement the arrangement could not have persisted without — while the metrics describe a decaying-extraction life course; the divergence between the structural claim and the end-state scalars is data, not an inconsistency to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from the same decrees. From the cabinet seat the cascade is a designed instrument it built, funded, and successfully retired — coordination it authored. From the mandated personnel seats the same instrument arrived as a condition of employment: adopt the dress, cut the hair, keep the new calendar, or lose the salary — a burden with a delayed, diffuse offset in career standing. From the samurai households' seat it arrived as confiscation of the visible constitution of their rank. From the treaty-power seat it registered as welcome convergence. Village officeholders experienced it as opportunity — adoption convertible into office and tax standing. The engine derives these per-seat classifications from the declared structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The cabinet sits nearest the beneficiary pole: it authors the rules, collects the uniformity and the diplomatic payoff, and bears no compliance cost, with arbitrage-grade exit since it wrote the arrangement. Treaty powers are external beneficiaries with arbitrage exit — they paid nothing and collected a conformed counterpart. Village officeholders lean beneficiary: adoption purchased office and tax advantages unavailable to non-adopters. Mandated civil servants sit past symmetric toward target: coerced costs up front, partial career offsets later, exit constrained by employment. Military personnel sit nearer the target pole: trapped by service discipline, bearing the fullest bodily discontinuity. Dispossessed samurai households sit near the full-target pole: stripped of stipend and sword, their revolt is the measured resistance. Traditionalist rural households are downstream targets — never consenting, never addressed, absorbing displacement decades later through the climb, with landbound exit. Rural women outside the decree's reach bear the climb's household-level costs with no addressed-audience status at all. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are blocked. Reading the cascade as pure voluntary modernization (rope) erases the coerced fringe — the paying class is constitutive of the mechanism, not an accident of implementation. Reading it as pure cultural imperialism (snare) misses the arrangement's defining temporal fact: its extraction was front-loaded and self-liquidating, because success made enforcement unnecessary — a snare must suppress exits permanently to persist, whereas this arrangement dismantled its own enforcement as adoption generalized. The mandate did outlive its function, but only briefly and ceremonially (late inspections of already-universal practice), and the arrangement dissolved into ordinary custom rather than atrophying into maintained performance: mandatrophy resolved in the benign direction, by completion rather than capture. The tangled_rope classification holds the both-and the life course demands — genuine coordination whose completion mechanism converts compulsion into volition — and the falling extraction series is the observable that separates this arrangement from a snare with a coordination alibi.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This constraint is one reading of the imposition_pathway_kernel — the hybrid cascade reading. What would adopting a sibling reading change structurally, and where exactly is the disagreement located?',
    'Cross-case diffusion-curve decomposition: date the decree-jump separately from the post-decree organic slope across multiple state formations (Meiji Japan, Republican Turkey, Pahlavi Iran). If imposed displacements consistently show both a dated discontinuity and a long organic tail, the hybrid reading holds; if the tail dissolves under archival scrutiny, the endogenous reading gains; if the tail is absent for low-identity-load commitments, the exogenous reading gains ground.',
    'The endogenous reading would dissolve the coerced-victim class (the fringe becomes voluntary, epsilon falls, the victim set empties); the exogenous reading would delete the climb phase (completion credit shifts wholly to state capacity, suppression stops decaying, and the arrangement trends toward permanent-enforcement extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer positioning: one reading of the imposition-pathway kernel; sibling readings relocate the victim set and the epsilon.').

omega_variable(
    manufactured_fringe_equivalence,
    'Is a decree-created compulsory adopter population functionally equivalent to an organic fringe for climb purposes, or does state manufacture change the climb''s dynamics?',
    'Compare climb velocities and adoption-depth profiles originating from salaried compulsory blocs versus volunteer pioneer populations across comparable diffusion episodes.',
    'If manufactured fringes climb faster initially but shallower (display without conviction), the compressed-climb framing needs a distinct M-set cell and the arrangement''s coordination credit shrinks; if equivalent, the hybrid reading''s synthesis stands unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufactured_fringe_equivalence, empirical, 'Whether state-manufactured fringes behave like organic ones in the completion phase.').

omega_variable(
    counterfactual_climb_timeline,
    'Would the displaced commitments have lost to the new ones without the decrees, and on what timeline?',
    'Counterfactual analysis from parallel commitments the state did not mandate (commercial fashions, religious affiliation shifts) whose diffusion rates are independently measurable; extrapolate the unmandated baseline against the mandated practices'' adoption curves.',
    'If organic climb was imminent, the decree''s compulsion purchased little coordination and the extraction-to-benefit ratio worsens sharply; if climb was remote before the 1890s, the decree was decisive and much of the measured extraction is the price of the transition itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_climb_timeline, empirical, 'How much displacement the override actually caused versus merely accelerated.').

omega_variable(
    enforcement_retirement_vs_abandonment,
    'Does the falling suppression series record genuine enforcement retirement (the arrangement succeeding) or state enforcement failure (the arrangement being abandoned)?',
    'Fiscal and personnel records of the inspection apparatus: budget lines, staffing, and citation rates declining alongside rising voluntary adoption indicate retirement; declining amid stagnant adoption indicates abandonment.',
    'Retirement supports the completion reading and the benign mandatrophy resolution; abandonment would recast the late-interval arrangement as a decaying imposed structure whose displacement stalled incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_retirement_vs_abandonment, empirical, 'Whether suppression decay reflects success or collapse.').

omega_variable(
    fiat_completion_boundary,
    'Where is the boundary between commitments the cascade completes and commitments decree alone completes — does the near-instant Gregorian calendar adoption (legal overnight, dual use persisting only residually) refute the climb-completion axiom at the low-identity-load margin?',
    'Sort imposed commitments by identity load (how much the practice constituted the adopter''s social self) and test whether completion mode tracks identity load: fiat for calendrics and units of measure, climb for dress, grooming, and bodily practice.',
    'A clean identity-load boundary would refine the climb-completion axiom into a conditional claim and push the drift state toward acknowledged; no boundary would leave the axiom intact as universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiat_completion_boundary, empirical, 'Whether some imposed commitments complete without any climb phase, pressuring the second foundational axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(impo_tr_t5, observed).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(impo_tr_t15, observed).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(impo_tr_t20, observed).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement_basis(impo_tr_t25, observed).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(impo_tr_t30, observed).
narrative_ontology:measurement(impo_tr_t35, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(impo_tr_t35, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(impo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(impo_be_t5, observed).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(impo_be_t15, observed).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(impo_be_t20, observed).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement_basis(impo_be_t25, observed).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement_basis(impo_be_t30, observed).
narrative_ontology:measurement(impo_be_t35, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 35, 0.39).
narrative_ontology:measurement_basis(impo_be_t35, observed).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(impo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(impo_su_t5, observed).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(impo_su_t15, observed).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(impo_su_t20, observed).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(impo_su_t25, observed).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(impo_su_t30, observed).
narrative_ontology:measurement(impo_su_t35, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 35, 0.33).
narrative_ontology:measurement_basis(impo_su_t35, observed).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 40, 0.26).
narrative_ontology:measurement_basis(impo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% 'Top-down imposition' as commonly used conflates three structurally distinct claims about how imposed commitment displacement proceeds: that no genuine override exists (endogenous_climb_reading), that override alone completes displacement (exogenous_override_reading), and that override initiates while climb completes (this file). The readings assign different victim sets and different epsilon to the same historical material: the endogenous referent contains no coerced class; the exogenous referent contains a permanently coerced population with no climb-phase offset; the hybrid referent contains a coerced manufactured fringe whose costs are recouped as the climb converts compulsion into custom. Family links run through network.affects_constraints; epsilon is invariant within each file because each file fixes one referent and one reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
