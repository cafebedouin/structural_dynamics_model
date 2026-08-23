% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Household Syncretic Devotion as Divine-Legitimacy Substrate (Folk Reading)
 *   domain: religious/political-economy/ancient-history
 *
 * SUMMARY:
 *   In New Kingdom Egyptian village life, divine legitimacy was not
 *   administered from above: it was produced in courtyards and front rooms,
 *   where households maintained shrines to protective powers — Bes and
 *   Taweret at births, ancestors at deaths, local forms of Hathor wherever a
 *   request seemed answered — and pragmatically incorporated or retired
 *   deities according to results. The arrangement required no temple, no
 *   priestly broker, and no royal warrant; pharaoh and the great priesthood
 *   were distant figures whose own legitimacy claims ran on other rails
 *   entirely. The arrangement's defining stress test came when the state
 *   attempted to reroute legitimacy through exclusive royal revelation:
 *   household practice went quiet, waited, and resumed essentially unchanged
 *   within a generation of the attempt collapsing. CONSTRAINT FAMILY NOTE:
 *   per the epsilon-invariance principle, the colloquial label 'how divine
 *   legitimacy worked in ancient Egypt' decomposes into three structurally
 *   distinct claims — this file authors the folk syncretistic reading only;
 *   the amun_polytheistic_reading (priestly mediation, temple-economy
 *   extraction) and atenist_monotheistic_reading (exclusive royal revelation,
 *   maximal coercion) are separate files linked via
 *   network.affects_constraints, with substantially higher epsilon referents.
 *
 * KEY AGENTS:
 *   - - rural_household_practitioners: primary participants and net recipients (organized/constrained) — bear the small material costs, receive the practice's goods, rotate deities freely
 *   - - village_elders_and_household_heads: local agenda-setters (moderate/identity_locked) — set the ritual calendar, admit new deities, transmit procedure; their rank IS custodianship
 *   - - local_ritual_specialists: secondary beneficiaries (moderate/mobile) — wise women, mourners, festival performers, reciters converting need into rite for payment in kind
 *   - - votive_artisans: secondary beneficiaries (moderate/mobile) — makers of amulets, stelae, and figures whose demand tracks devotional fashion
 *   - - pharaoh_royal_estate: distant elite outside the exchange (powerful/arbitrage) — neither supplies nor taxes household practice; invests legitimacy elsewhere
 *   - - state_temple_priesthood: distant elite outside the exchange (institutional/arbitrage) — mediates for those who can reach the temples; receives only fringe village votives
 *   - - modern_egyptologists: analytical observer (analytical/analytical) — reconstructs the arrangement from domestic archaeology and village texts no ancient party commissioned
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.23).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.06).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.23).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Household Syncretic Devotion as Divine-Legitimacy Substrate (Folk Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political-economy/ancient-history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, 'a352b619-e518-4779-a6d5-e55924632d7d').
narrative_ontology:cs_kernel_codification('a352b619-e518-4779-a6d5-e55924632d7d', implicit).
narrative_ontology:cs_authority_grounding('a352b619-e518-4779-a6d5-e55924632d7d', practice).
narrative_ontology:cs_interpretation_layer_present('a352b619-e518-4779-a6d5-e55924632d7d').
narrative_ontology:cs_reading_relation('a352b619-e518-4779-a6d5-e55924632d7d', divine_legitimacy_substrate__amun_polytheistic_reading, influences).
narrative_ontology:cs_reading_relation('a352b619-e518-4779-a6d5-e55924632d7d', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_axiom('a352b619-e518-4779-a6d5-e55924632d7d', foundational, legitimacy_through_domestic_praxis).
narrative_ontology:cs_axiom_status(legitimacy_through_domestic_praxis, holdable).
narrative_ontology:cs_axiom_grounding('a352b619-e518-4779-a6d5-e55924632d7d', legitimacy_through_domestic_praxis, conventional).
narrative_ontology:cs_axiom('a352b619-e518-4779-a6d5-e55924632d7d', foundational, efficacy_based_deity_incorporation).
narrative_ontology:cs_axiom_status(efficacy_based_deity_incorporation, holdable).
narrative_ontology:cs_axiom_grounding('a352b619-e518-4779-a6d5-e55924632d7d', efficacy_based_deity_incorporation, instrumental).
narrative_ontology:cs_reference_frame('a352b619-e518-4779-a6d5-e55924632d7d', household_customary_plurality).
narrative_ontology:cs_drift_state('a352b619-e518-4779-a6d5-e55924632d7d', post_amarna_restoration, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a352b619-e518-4779-a6d5-e55924632d7d', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, rural_household_practitioners).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, votive_artisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders_and_household_heads).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, rural_household_practitioners).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, unmediated_devotional_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain a shrine niche in the house with images of protective powers — Bes and Taweret for childbirth, ancestors for the dead, local forms of Hathor and whichever deity lately delivered — and set out bread, beer, water, and flowers at life crises and calendar moments. A deity that stops answering gets quietly retired and another tried; nothing forbids letting a shrine lapse, but custom, neighbors, and the festival round make lapse conspicuous. The material costs are small and recurrent; what they buy, as the household reckons it, is protected births, survived illness, a manageable funeral, and a place in the village's shared occasions.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, rural_household_practitioners, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, rural_household_practitioners, payer).

% Organize the village ritual year — procession days, feast days, first-fruits gestures — decide which newly reported deity the village will adopt, and teach procedure to the next generation. Their rank in the village rests on performing custodianship credibly; setting the custom aside would dissolve the very role that gives them standing, so the option exists formally and is unthinkable practically.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders_and_household_heads, agenda_setter,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders_and_household_heads, beneficiary).

% Wise women who attend births, hired mourners, festival drummers and dancers, and part-time reciters who know the effective words: they turn household need into performed rite for payment in kind. Their skill travels — a specialist with a reputation serves neighboring villages or attaches to a wealthier house — so a failing village market costs them income, not livelihood.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists, beneficiary,
    moderate, biographical, mobile, local).

% Carve votive stelae, mold faience Bes figures and Taweret pendants, and paint offering tables sold at festival stalls. Demand tracks devotional fashion: a deity's run of apparent successes fills their workshops, and a deity's retirement empties them. Their stock-in-trade moves with them if a village market dies.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, votive_artisans, beneficiary,
    moderate, biographical, mobile, local).

% Rules from palace and state temple, presenting royal ritual as the pillar on which cosmic order turns. Household altars address local protectors without invoking royal mediation; the crown neither provisions nor taxes this practice, and its one attempt to redirect popular devotion wholesale was ignored at the village level until it collapsed. The king's legitimacy investments route through court ceremony and temple endowment instead — rails this arrangement does not touch.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh_royal_estate, excluded,
    powerful, civilizational, arbitrage, national).

% Staffs the great temples, behind whose colonnades laypeople — women especially — could rarely go. Income comes from state endowment and elite dedication, with village votives arriving only at exterior listening-chapels where the gods were believed to lend an ear. Devotion reaching the deities without priestly brokerage bypasses the priesthood's mediating role, and simultaneously, when a fashionable household deity overlaps a temple god, feeds the temple's prestige.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, state_temple_priesthood, excluded,
    institutional, generational, arbitrage, national).

% Reconstruct the arrangement from domestic archaeology, workmen's-village inscriptions, and votive deposits, comparing it against the temple-centered and royal-revelation accounts of the same centuries. No ancient party commissioned the assessment, and none can dispute it except through the material record itself.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, modern_egyptologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves household-level crisis coping without central administration: childbirth protection, illness response, funerary transition, and festival-timed mutual aid are coordinated through shared customary procedure that any household can perform with local materials and no gatekeeper, plus a mechanism (reported-results deity rotation) for allocating trust among candidate protective powers.
% TRANSFER_FUNCTION: Moves small material offerings (bread, beer, water, flowers), faience and pottery goods, festival labor, and attention from participating households toward the addressed deities ritually and, materially, toward communal feasts that recycle the offerings, local specialists paid in kind, and artisans selling votive goods; the flows are reciprocal in expectation rather than enforced in fact.
% ABSENT_VOICES: The state priesthood and the royal house are the absent voices: both would insist that legitimacy must route through temple mediation and royal cult, and both are structurally outside the household exchange, which consults neither. Within the villages, outright dissent barely existed as a voice — a household skeptical of a deity's efficacy substituted another deity rather than mounting an objection, so pragmatic substitution absorbed what would otherwise have been opposition. Women carried much of the ritual labor while formal festival offices sat with male elders; their side of the ledger is reconstructed mainly from objects, not testimony (see the gendered-labor omega).
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, every birth, illness, and death in the countryside loses its working script; the festival calendar — the spine of village sociability and redistribution — collapses, taking specialists' and artisans' livelihood niches with it; ancestral continuity practices end, severing a core mechanism of household identity; and the legitimacy question the kernel names reopens with no answer at the level where most people actually lived. The elite rails (court ceremony, temple cult) would continue untouched, which is exactly the point: what rearranges is the world of the households, not the world of the palace.
% FOUNDING_PROBLEM: Households facing mortal uncertainty — childbirth death rates, disease, crop failure, bereavement — needed actionable channels to protective power that did not depend on access to temple precincts, which ordinary people and especially women could rarely enter; villages additionally needed shared recurring occasions for redistribution and solidarity that did not require elite sponsorship.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the material record itself — domestic shrines, votive beds, and Bes/Taweret imagery in non-elite homes across sites and centuries, deposited by the participants and readable without any beneficiary's narration; by workmen's-village texts recording direct personal appeals to deities with no priestly intermediary mentioned; and by the historical outcome that the attempt to reroute legitimacy through exclusive royal revelation collapsed within a generation of its enforcer's death while household practice resumed unchanged, an outcome attested by the restoring regime's own public edicts. Modern scholarly analysis of New Kingdom personal piety attests the same reading, though it postdates the arrangement by three millennia and is cited as analysis, not witness.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.23, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. CLAIMED TYPE rope: the arrangement solves real household-level problems (crisis coping, festival solidarity, knowledge transmission) with essentially zero coercive overhead, participants are net recipients, and far from suppressing alternatives it manufactures them — a deity that stops delivering is retired and replaced, which is the opposite of exit-blocking. METRICS, descriptively: extractiveness 0.23 because real but small flows (bread, beer, faience, festival labor) leave households, and notably ABOVE the attachment-coordination floor of 0.08 — the excess is expressive devotional surplus willingly paid, not coerced rent, and directionality damping at the beneficiary seats is where that judgment gets computed. Suppression 0.06: no enforcement machinery was ever built for this arrangement at any point in the interval; the flat near-zero suppression_requirement series is the point of tracking it — the arrangement runs on voluntary uptake, and its one historical stress test showed compliance was never what held it up. Theater_ratio 0.17: the rite is the function, not a proxy for it; the mild late-interval rise reflects habitual reproduction where efficacy-confidence thinned, not Goodhart capture. Accessibility_collapse 0.15: alternatives multiply by design; nothing collapses. Resistance 0.14 in the quiet end-state, but the scalar understates the arrangement's demonstrated defensive capacity — under external attack (time points 97-121) households hid images and continued rites covertly, and restoration followed within a generation. MEASUREMENT DISCIPLINE: one shared eight-point grid (years since a 1450 BCE baseline; point 97 is royal-iconoclasm onset, 121 the restoration) carries all three metrics at every point, so the persecution-window spike cannot be misdated by scalar substitution. The spike-and-recovery shape is a perturbation-recovery cycle driven by an exogenous attack, NOT intermittent reinforcement — the oscillation is not itself an extraction mechanism. Base_properties scalars snapshot the end-state (point 300, quiet era). Receipt surface: gain_flow is authored as 'diffuse' as a checked affirmative — each named seat was examined, and none captures: specialists and artisans earn incomes tracking delivered effort, and offering goods recycle to participants through communal feasts. fixing_cost is deliberately OMITTED: under fully diffuse authority there is no seat that could fix or remove the arrangement (the only historical removal attempt belonged to the sibling atenist constraint's enforcement machinery, not to any fixer of this one), so the field's presupposition of a fixer fails and authoring it would fabricate an actor.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the household seat, the arrangement is lived necessity: the same flows that look like 'costs' in aggregate are, seat-internally, purchases of protection at life's most exposed moments, and exit is socially conspicuous rather than forbidden. From the elder seat it is sacred custodianship fused with rank. From the specialist and artisan seats it is a livelihood that follows devotional fashion. From the pharaoh and priesthood seats it is background noise — a legitimacy economy running beneath their own, indifferent to them, which they discovered they could not command. The engine derives these divergent per-seat classifications from the structural data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: rural_household_practitioners, local_ritual_specialists, and votive_artisans sit at the beneficiary end (low d), so effective extraction damps toward subsidy for them — practitioners pay small flows and receive the arrangement's entire point; specialists and artisans monetize it without running it. No victims are declared because none exist at village granularity: no group bears the arrangement's costs against its interest, which is precisely why the snare and tangled_rope gates lack their required inputs. DIRECTIONALITY OVERRIDES: two are authored, for power_atom 'powerful' (pharaoh) and 'institutional' (priesthood), both at d=0.5. The derivation chain has no structural data for these agents — they appear in neither beneficiaries nor victims — so canonical fallbacks keyed to their power atoms would guess a targeting relationship that does not exist; under this reading they are outside the exchange entirely, neither subsidized nor extracted-from, and the explicit neutral override records that indifference rather than letting a power-keyed default fabricate engagement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mortal uncertainty requiring actionable channels to protective power without priestly gatekeeping — is LIVE at interval end: births, illnesses, and deaths did not stop happening, and the arrangement kept solving for them. Mandatrophy_resolved is therefore false, no sunset clause exists, and the rope classification blocks two symmetrical misreadings. It blocks the extraction misreading: a snare reading would need coerced payers and suppressed exits, and the record shows free deity rotation and voluntary uptake instead — treating devotional flows as rents would misclassify willing purchase as plunder. It blocks the naturality misreading: the arrangement is constructed custom, not natural law (emerges_naturally false), and its survival through the iconoclasm window shows persistence-by-consent rather than irreversibility — the omega on revision-capacity asymmetry keeps the distinction between resilient consent and rigid inertia open rather than settling it rhetorically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading (folk_syncretistic_reading) of the contested kernel divine_legitimacy_substrate; the sibling readings amun_polytheistic_reading and atenist_monotheistic_reading are separate constraints with their own beneficiary/victim structures and epsilon values. Where exactly does the disagreement between readings sit, and what would change structurally if a sibling reading governed?',
    'Comparative classification across the three sibling files: the disagreement locates in the ROUTING of legitimacy (which seat mediates it) and in the SCOPE of admissible deities, not in whether divine legitimacy exists. Sibling adoption changes the party structure: the amun_polytheistic_reading concentrates mediation in the priesthood and routes temple-economy rents to it; the atenist_monotheistic_reading makes the pharaoh sole legitimate beneficiary and renders every household practitioner a coerced payer.',
    'If a sibling reading were adopted as governing constraint, this story''s beneficiary set dissolves: practitioners become either priestly clients (amun reading) or persecuted targets (atenist reading), and effective extraction redistributes accordingly. Cross-reading comparison is the corpus-level measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer-frame omega: this file is one indexed reading of a three-way kernel contest; sibling structure lives in sibling files.').

omega_variable(
    beneficiary_structure_opacity,
    'Who, if anyone, systematically gains beyond reciprocal participant benefit: do local ritual specialists and votive artisans constitute a capturing class living off compulsory devotional demand, or mere cost-recovering service providers whose income tracks effort?',
    'Quantitative reconstruction of household ritual expenditure versus specialist and artisan income concentration across excavated village sites (workmen''s villages, estate villages, temple towns): if a persistent price wedge separates what households pay from what delivery costs, a capturing class exists.',
    'Evidence of concentrated capture would push this story toward tangled_rope, with specialists or artisans as beneficiaries and poorer households as an identifiable paying class; confirmation of cost-tracking incomes supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_opacity, empirical, 'Whether diffuse devotional flows hide a concentrated capturer seat.').

omega_variable(
    epsilon_external_shock_conflation,
    'During the royal iconoclasm window (time points 97-121), the elevated base_extractiveness reflects externally imposed risk premiums on practicing, not any intrinsic growth in what the household arrangement takes from participants. Does the measurement series conflate a sibling constraint''s enforcement operation with this arrangement''s intrinsic burden?',
    'Decompose the series into intrinsic burden (offerings, labor, festival contribution, reconstructable from offering lists and votive economics) and risk premium (observable only in persecution windows); classify on the intrinsic component.',
    'If intrinsic extraction is flat at roughly 0.21-0.23 throughout, the spike is diagnostic of the sibling atenist constraint''s suppression profile rather than of drift in this one, and this reading''s classification is robust; conflating them would falsely date a type transition into the persecution window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_external_shock_conflation, conceptual, 'Whether the extractiveness spike measures this constraint or its attacker.').

omega_variable(
    gendered_ritual_labor_distribution,
    'Household ritual labor fell disproportionately on women (birth protection, domestic shrine upkeep, mourning), while formal village ritual offices sat mostly with male elders. Does the arrangement distribute costs and standing unevenly by gender such that a latent paying seat exists inside the participant population?',
    'Gender-disaggregated analysis of the domestic cult record: distribution of votive beds, birth-related equipment, and female votaries versus the gender composition of festival organization and customary decision-making.',
    'If ritual burden and ritual authority diverge systematically by gender, a within-household asymmetry exists that a household-level analysis averages away; that would add an internal payer seat and shade the classification toward tangled_rope without changing the inter-household picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_ritual_labor_distribution, empirical, 'Whether devotional costs and authority split along gender lines inside households.').

omega_variable(
    revision_capacity_asymmetry,
    'The arrangement constantly absorbs bottom-up revisions (deities added and retired on reported results) yet repelled the one top-down revision attempted. Is its resistance to top-down revision genuine distributed veto power, or mere institutional inertia that would equally block beneficial change?',
    'Compare outcomes of revision attempts by initiation locus: catalog successful bottom-up incorporations (new deity uptake cycles) against failed top-down impositions across the interval; asymmetry with fast bottom-up uptake indicates working distributed consent, not inertia.',
    'Distributed-consent confirmation secures the rope reading (alternatives live, uptake voluntary); inertia-only confirmation would suggest the arrangement survives despite rather than because of participant preference, moving it toward piton-flavored persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_capacity_asymmetry, conceptual, 'Whether anti-revision robustness is consent or inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.11).
narrative_ontology:measurement_basis(divi_tr_t50, observed).
narrative_ontology:measurement(divi_tr_t97, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 97, 0.12).
narrative_ontology:measurement_basis(divi_tr_t97, observed).
narrative_ontology:measurement(divi_tr_t108, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 108, 0.16).
narrative_ontology:measurement_basis(divi_tr_t108, observed).
narrative_ontology:measurement(divi_tr_t121, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 121, 0.12).
narrative_ontology:measurement_basis(divi_tr_t121, observed).
narrative_ontology:measurement(divi_tr_t170, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 170, 0.12).
narrative_ontology:measurement_basis(divi_tr_t170, observed).
narrative_ontology:measurement(divi_tr_t240, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 240, 0.14).
narrative_ontology:measurement_basis(divi_tr_t240, observed).
narrative_ontology:measurement(divi_tr_t300, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 300, 0.17).
narrative_ontology:measurement_basis(divi_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement_basis(divi_be_t50, observed).
narrative_ontology:measurement(divi_be_t97, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 97, 0.28).
narrative_ontology:measurement_basis(divi_be_t97, observed).
narrative_ontology:measurement(divi_be_t108, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 108, 0.37).
narrative_ontology:measurement_basis(divi_be_t108, observed).
narrative_ontology:measurement(divi_be_t121, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 121, 0.27).
narrative_ontology:measurement_basis(divi_be_t121, observed).
narrative_ontology:measurement(divi_be_t170, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 170, 0.21).
narrative_ontology:measurement_basis(divi_be_t170, observed).
narrative_ontology:measurement(divi_be_t240, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 240, 0.22).
narrative_ontology:measurement_basis(divi_be_t240, observed).
narrative_ontology:measurement(divi_be_t300, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 300, 0.23).
narrative_ontology:measurement_basis(divi_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t50, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 50, 0.06).
narrative_ontology:measurement_basis(divi_su_t50, observed).
narrative_ontology:measurement(divi_su_t97, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 97, 0.08).
narrative_ontology:measurement_basis(divi_su_t97, observed).
narrative_ontology:measurement(divi_su_t108, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 108, 0.09).
narrative_ontology:measurement_basis(divi_su_t108, observed).
narrative_ontology:measurement(divi_su_t121, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 121, 0.06).
narrative_ontology:measurement_basis(divi_su_t121, observed).
narrative_ontology:measurement(divi_su_t170, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 170, 0.05).
narrative_ontology:measurement_basis(divi_su_t170, observed).
narrative_ontology:measurement(divi_su_t240, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 240, 0.05).
narrative_ontology:measurement_basis(divi_su_t240, observed).
narrative_ontology:measurement(divi_su_t300, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 300, 0.06).
narrative_ontology:measurement_basis(divi_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, attachment_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'ancient Egyptian divine legitimacy' covers three structurally distinct claims and is authored as three files sharing the kernel divine_legitimacy_substrate. This file (folk_syncretistic_reading) authors the lowest-extraction member: diffuse household praxis, epsilon ~0.23, no enforcement machinery. The amun_polytheistic_reading authors priestly mediation with temple-economy extraction concentrating on the priesthood seat (upstream in evidentiary confidence — the institutional record is richer — and downstream-linked to this file because temple festival calendars structured village ritual time and popular devotion to temple deities fed back as pressure forcing lay accommodation). The atenist_monotheistic_reading authors exclusive royal revelation with maximal suppression and a coerced universal payer set; it is downstream of BOTH siblings in the sense that its enforcement operation is what this file's persecution-window measurements register, and its collapse within a generation is corroborating evidence FOR this reading's routing claim. Edges are declared from this file to both siblings; the sibling files carry the reciprocal declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__folk_syncretistic_reading, powerful, 0.5).
constraint_indexing:directionality_override(divine_legitimacy_substrate__folk_syncretistic_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
